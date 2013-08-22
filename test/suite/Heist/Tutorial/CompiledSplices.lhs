Introduction to Compiled Heist
==============================

Before version 0.10, Heist has essentially been an interpreter.  It loads your
templates and "runs" them whenever a page is served.  This is relatively
inefficient since a lot of document transformations happen every time the
template is requested.  For Heist version 0.10 we completely rethought
everything with performance in mind.  We call it "compiled Heist".  The main
idea is to do most of your splice processing up front at load time.  There is
still a mechanism for rendering dynamic information at runtime, but it is
faster than the fully interpreted approach that Heist started with.

It should also be mentioned that the old "interpreted Heist" is not gone.  You
can still use the old approach where all the transformations happen at
render time.  This allows you to upgrade without making sweeping changes to
your code, and gradually convert your application to the more performant
compiled approach as you see fit.

Before we continue it should be mentioned that you are reading real live
literate Haskell code from our test suite.  All the code you see here is
compiled into our test suite and the results automatically checked by our
buildbot.  So first we need to get some boilerplate and imports out of the way.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Heist.Tutorial.CompiledSplices where
> import           Heist
> import qualified Heist.Compiled as C
> import           Heist.Tutorial.Imports

> import           Control.Applicative
> import qualified Data.Text as T
> import           Data.Text.Encoding
> import qualified Heist.Compiled.LowLevel as C
> import           Text.XmlHtml

As a review, normal (interpreted) Heist splices are defined like this.

< type Splice m = HeistT m m [Node]

The type parameter `m` is the runtime execution monad (in a Snap application
this will usually be `Handler` or `Snap`).  Don't worry about why the `m` is
there twice right now.  We'll get to that later.  The splice's return value is
a list of nodes that is substituted back into the document wherever the
spliced node was.  

This kind of splice proccessing involves traversing the DOM, which is
inefficient.  Compiled Heist is designed so that all the DOM traversals happen
once at load time in the IO monad.  This is the "compile" phase.  The type
signature for compiled splices is this.

< type Splice n = HeistT n IO (DList (Chunk n))

We see that where Heist splices ran in the m monad, compiled splices run in the
IO monad.  This also explains why HeistT now has two monad type parameters.
The first parameter is a placeholder for the runtime monad and the second
parameter is the monad that we're actually running in now.

But the key point of the compiled splice type signature is the return value.
They return a DList of Chunks.  DList is a list that supports efficient
insertion to both the front and back of the list.  The Chunk type is not
exposed publicly, but there are three ways to construct a Chunk.

< yieldPure :: Builder -> DList (Chunk m)
< yieldRuntime :: RuntimeSplice m Builder -> DList (Chunk m)
< yieldRuntimeEffect :: Monad m => RuntimeSplice m () -> DList (Chunk m)

If your splice output can be calculated at load time, then you should use
`yieldPure` or one of its variants.  When you do this, Heist can concatenate
all adjacent pure chunks into a single precalculated ByteString that can be
rendered very efficiently.  If your template needs a value that has to be
calculated at runtime, then you should use the `yieldRuntime` constructor and
supply a computation in the RuntimeSplice monad transformer that is
parameterized by `m` which we saw above is the runtime monad.  Occasionally
you might want to run a runtime side effect that doesn't actually insert any
data into your template.  The `yieldRuntimeEffect` function gives you that
capability.

An Example
==========

With that background, let's get to a real example.

> stateSplice :: C.Splice (StateT Int IO)
> stateSplice = return $ C.yieldRuntimeText $ do
>     val <- lift get
>     return $ pack $ show (val+1)

Here we see that our splice's runtime monad is `StateT Int IO`.  This makes
for a simple example that can clearly demonstrate the different contexts that
we are operating in.  To make things more clear, here's a version with some
print statements that clarify the details of which monad is executed when.

> stateSplice2 :: C.Splice (StateT Int IO)
> stateSplice2 = do
>     -- :: C.Splice (StateT Int IO)
>     lift $ putStrLn "This executed at load time"
>     let res = C.yieldRuntimeText $ do
>             -- :: RuntimeSplice (StateT Int IO) a
>             lift $ lift $ putStrLn "This executed at run/render time"
>             val <- lift get
>             return $ pack $ show (val+1)
>     lift $ putStrLn "This also executed at load time"
>     return res

Note here that even though the type parameter to C.Splice is a monad, it is not
a monad transformer.  RuntimeSplice, however, is.  Now let's look at a simple
load function that sets up a default HeistState and loads templates from a
directory with compiled splices.

> load :: MonadIO n
>      => FilePath
>      -> Splices (C.Splice n)
>      -> IO (HeistState n)
> load baseDir splices = do
>     tmap <- runEitherT $ do
>         let hc = HeistConfig mempty defaultLoadTimeSplices splices mempty
>                              [loadTemplates baseDir]
>         initHeist hc
>     either (error . concat) return tmap

Here's a function demonstrating all of this in action.

> runWithStateSplice :: FilePath
>                    -> IO ByteString
> runWithStateSplice baseDir = do
>     hs <- load baseDir ("div" ## stateSplice)
>     let runtime = fromJust $ C.renderTemplate hs "index"
>     builder <- evalStateT (fst runtime) 2
>     return $ toByteString builder

First this function loads the templates with the above compiled splice.  You
have to specify all the compiled splices in the call to loadTemplates because
loadTemplates takes care of compiling all the templates up front.  If you were
able to bind compiled splices later, then all the templates would have to be
recompiled, a potentially expensive operation.  Next, the function renders the
template called "index" using a runtime (StateT Int IO) seeded with a value of
2 and returns the resulting ByteString.

Now let's look at a more complicated example.  We want to render a data
structure with a compiled splice.

> data Person = Person
>     { pFirstName :: Text
>     , pLastName  :: Text
>     , pAge       :: Int
>     }
> 
> personSplices :: Monad n
>              => Splices (RuntimeSplice n Person -> C.Splice n)
> personSplices = mapS (C.pureSplice . C.textSplice) $ do
>     "firstName" ## pFirstName
>     "lastName" ## pLastName
>     "age" ## pack . show . pAge
> 
> peopleSplice :: (Monad n)
>              => RuntimeSplice n [Person]
>              -> C.Splice n
> peopleSplice = C.manyWithSplices C.runChildren personSplices
> 
> allPeopleSplice :: C.Splice (StateT [Person] IO)
> allPeopleSplice = peopleSplice (lift get)
> 
> personListTest :: FilePath
>                -> IO ByteString
> personListTest baseDir = do
>     hs <- load baseDir ("people" ## allPeopleSplice)
>     let runtime = fromJust $ C.renderTemplate hs "people"
>     builder <- evalStateT (fst runtime)
>                  [ Person "John" "Doe" 42
>                  , Person "Jane" "Smith" 21
>                  ]
>     return $ toByteString builder


Disadvantages of Compiled Heist
===============================

Compiled Heist is faster than the original interpreted approach, but as with
most things in computing there is a tradeoff.  Compiled Heist is strictly less
powerful than interpreted Heist.  There are two things that compiled Heist
loses: the ability to bind new splices on the fly at runtime and splice
recursion/composability.

The first point follows immediately from the definition of compiled Heist.
When you decide to do all your splice DOM traversals once at load time you're
unavoidably limited to only those splices that you defined at load time.  But
this seems to be a good pattern to use in general because debugging your
splices will be easier if you don't have to consider the possibility that
the handler that binds them didn't run.

The loss of recursion/composability happens because of the change in the type
signature of splices.  Interpreted splices are a essentially function `[Node]
-> m [Node]`.  This means that the output of one splice can be the input of
another splice (including itself).  Compiled splices are a function `[Node] ->
IO (DList (Chunk m))`.  Therefore, once a splice processes some nodes, the
output is no longer something that can be passed into other splices.  

This composability turns out to be a very powerful feature.  Head merging is
one feature that can't be done without it.  Head merging allows you to put
`<head>` tags anyhere in any template and have them all merged into a single
`<head>` tag at the top of your HTML document.  This is useful because it allows
you to keep concerns localized.  For instance, you can have a template
represent a small piece of functionality that uses a less common javascript or
CSS file.  Instead of having to depend on that resource being included in the
top-level `<head>` tag, you can include it in a `<head>` tag right where you're
using it.  Then it will only be included on your pages when you are using the
markup that needs it.

Our implementation of head merging uses a splice bound to the `<html>` tag.
This splice removes all the `<head>` nodes from its children, combines them, and
inserts them as its first child.  This won't work unless the `<html>` splice
first runs all its children to make sure all `<apply>` and `<bind>` tags have
happened first.  And that is impossible to do with compiled splices.

To get around this problem we added the concept of load time splices.  Load
time splices are just interpreted splices that are completely executed at load
time.  If interpreted splices have type `[Node] -> m [Node]` where m is the
runtime monad, then load time splices have type `[Node] -> IO [Node]`, where
IO is the monad being executed at load time.  Load time splices give you the
power and composability of interpreted splices as long as they are performing
transformations that don't require runtime data.  All of the built-in splices
that we ship with Heist work as load time splices.  So you can still have head
merging by including our html splice in the load time splice list in your
HeistConfig.


A More Involved Example
=======================

The person example above is a very common and useful pattern for using dynamic
data in splices.  But it has the simplification that it always generates
output the same way.  Sometimes you might want a splice's output to have one
form in some cases and a different form in other cases.  A simple example is a
splice that reads some kind of a key from a request parameter then looks that
key up in some kind of map.  If the key is present the splice uses its child
nodes as a view for the retrieved value, otherwise it outputs an error message.  

This pattern is a little tricky because you're making decisions about what to
render based on runtime data, but the actual rendering of child nodes has to
be done at load time.  To bridge the gap and allow communication between load
time and runtime processing we provide the Promise data type.  A Promise is
kind of like an IORef except that operations on them are restricted to the
appropriate Heist context.  You create a new empty promise in the HeistT n IO
(load time) monad, and you operate on it in the RuntimeSplice monad.

Here's an example of how to use a promise manually to render a splice
differently in the case of failure.

< failingSplice :: MonadSnap m => C.Splice m
< failingSplice = do
<     children <- childNodes <$> getParamNode
<     promise <- C.newEmptyPromise
<     outputChildren <- C.withSplices C.runChildren splices (C.getPromise promise)
<     return $ C.yieldRuntime $ do         
<         -- :: RuntimeSplice m Builder
<         mname <- lift $ getParam "username"
<         let err = return $ fromByteString "Must supply a username"
<             single name = do          
<                 euser <- lift $ lookupUser name
<                 either (return . fromByteString . encodeUtf8 . T.pack)
<                        doUser euser
<               where
<                 doUser value = do
<                   C.putPromise promise (name, value)
<                   C.codeGen outputChildren
<         maybe err single mname
<   
<   
< splices :: Monad n
<         => Splices (RuntimeSplice n (Text, Text) -> C.Splice n)
< splices = mapS (C.pureSplice . C.nodeSplice) $ do
<   "user"  ## (:[]) . TextNode . fst
<   "value" ## (:[]) . TextNode . snd

