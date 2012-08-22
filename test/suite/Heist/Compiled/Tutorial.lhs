Introduction to Compiled Heist
==============================

Heist is essentially an interpreter.  It loads your templates and "runs" them
whenever a page is served.  This is relatively inefficient since a lot of
document transformations happen every time the template is requested.
Compiled Heist does most of your splice processing up front at load time.
Dynamic information can still be rendered at runtime, but it is faster than
the fully interpreted approach that Heist started with.

Before we continue it should be mentioned that you are reading real live
literate Haskell code from our test suite.  All the code you see here is
compiled into our test suite and the results automatically checked by our
buildbot.  So first we need to get some boilerplate and imports out of the way.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Heist.Compiled.Tutorial where
> import           Heist
> import qualified Heist.Compiled as C
> import           Heist.Compiled.TutorialImports

As a review, Heist splices are defined like this.

< type Splice m = HeistT m m [Node]

The type parameter `m` is the runtime execution monad (in a Snap application
this will usually be `Handler` or `Snap`).  Don't worry why the `m` is there
twice right now.  We'll get to that later.  The splice's return value is a
list of nodes that is substituted back into the document wherever the spliced
node was.  

Splice proccessing involves traversing the DOM, which is inefficient.
Compiled Heist is designed so that all the DOM traversals happen once at load
time in the IO monad.  This is the "compile" phase.  The type signature for
compiled splices is this.

< type Splice n = HeistT n IO (DList (Chunk n))

We see that where Heist splices ran in the m monad, compiled splices run in the
IO monad.  This also explains why HeistT now has two monad type parameters.
The first parameter is a placeholder for the runtime monad and the second
parameter is the monad that we're actually running in now.

But the key point of the compiled splice type signature is the return value.
They return a DList of Chunks.  DList is a list that supports efficient
insertion to both the front and back of the list.  The Chunk type defined like
this.

< data Chunk m = Pure !Text
<                -- ^ output known at load time
<              | RuntimeHtml !(RuntimeSplice m Text)
<                -- ^ output computed at run time
<              | RuntimeAction !(RuntimeSplice m ())
<                -- ^ runtime action used only for its side-effect

If your splice output can be calculated at load time, then you will use the
`Pure` constructor.  When you do this, Heist can concatenate all adjacent Pure
chunks into a single precalculated ByteString that can be rendered very
efficiently.  If your template needs a value that has to be calculated at
runtime, then you should use the RuntimeHtml constructor and supply a
computation in the RuntimeSplice monad transformer that is parameterized by
`m` which we saw above is the runtime monad.  With that background, let's get
to a real example.

> stateSplice :: C.Splice (StateT Int IO)
> stateSplice = C.yieldRuntimeText $ do
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
>     res <- C.yieldRuntimeText $ do
>         -- :: RuntimeSplice (StateT Int IO) a
>         lift $ lift $ putStrLn "This executed at run/render time"
>         val <- lift get
>         return $ pack $ show (val+1)
>     lift $ putStrLn "This also executed at load time"
>     return res

Note here that even though the type parameter to C.Splice is a monad, it is not
a monad transformer.  RuntimeSplice, however, is.  Now let's look at a simple
load function that sets up a default HeistState and loads templates from a
directory with compiled splices.

> load :: MonadIO n
>      => FilePath
>      -> [(Text, C.Splice n)]
>      -> IO (HeistState n)
> load baseDir splices = do
>     tmap <- runEitherT $ initHeist [] [] splices =<< loadTemplates baseDir
>     either (error . concat) return tmap

Here's a function demonstrating all of this in action.

> runWithStateSplice :: FilePath
>                    -> IO ByteString
> runWithStateSplice baseDir = do
>     hs <- load baseDir [ ("div", stateSplice) ]
>     let runtime = fromJust $ C.lookupCompiledTemplate "index" hs
>     builder <- evalStateT runtime 2
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
> personSplice :: (Monad n)
>              => C.Promise Person
>              -> HeistT n IO (RuntimeSplice n Builder)
> personSplice = C.promiseChildrenWithText
>     [ ("firstName", pFirstName)
>     , ("lastName", pLastName)
>     , ("age", pack . show . pAge)
>     ]
> 
> peopleSplice :: (Monad n)
>              => n [Person]
>              -> C.Splice n
> peopleSplice getPeople = C.mapPromises personSplice getPeople
> 
> allPeopleSplice :: C.Splice (StateT [Person] IO)
> allPeopleSplice = peopleSplice get
> 
> personListTest :: FilePath
>                -> IO ByteString
> personListTest baseDir = do
>     hs <- load baseDir [ ("people", allPeopleSplice) ]
>     let runtime = fromJust $ C.lookupCompiledTemplate "people" hs
>     builder <- evalStateT runtime
>                  [ Person "John" "Doe" 42
>                  , Person "Jane" "Smith" 21
>                  ]
>     return $ toByteString builder

