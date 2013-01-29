{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Heist.Compiled.Internal where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8
import           Control.Arrow
import           Control.Monad
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.Text            as AP
import           Data.ByteString (ByteString)
import           Data.DList                      (DList)
import qualified Data.DList                      as DL
import qualified Data.HashMap.Strict             as H
import qualified Data.HeterogeneousEnvironment   as HE
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Vector                     as V
import           Prelude                         hiding (catch)
import qualified Text.XmlHtml                    as X
------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Types
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A compiled Splice is a HeistT computation that returns a @DList
-- (Chunk m)@.
--
-- The more interesting part of the type signature is what comes before the
-- return value.  The first type parameter in @'HeistT' n IO@ is the runtime
-- monad.  This reveals that the Chunks know about the runtime monad.  The
-- second type parameter in @HeistT n IO@ is @IO@.  This tells is that the
-- compiled splices themselves are run in the IO monad, which will usually
-- mean at load time.  Compiled splices run at load time, and they return
-- computations that run at runtime.
type Splice n = HeistT n IO (DList (Chunk n))


------------------------------------------------------------------------------
-- | Runs the parameter node's children and returns the resulting compiled
-- chunks.  By itself this function is a simple passthrough splice that makes
-- the spliced node disappear.  In combination with locally bound splices,
-- this function makes it easier to pass the desired view into your splices.
runChildren :: Monad n => Splice n
runChildren = runNodeList . X.childNodes =<< getParamNode
{-# INLINE runChildren #-}


------------------------------------------------------------------------------
-- | Takes a promise function and a runtime action returning a list of items
-- that fit in the promise and returns a Splice that executes the promise
-- function for each item and concatenates the results.
--
-- This function works nicely with the 'promiseChildrenWith' family of
-- functions, much like the combination of 'mapSplices' and 'runChildrenWith'
-- for interpreted splices.
mapPromises :: Monad n
            => (Promise a -> HeistT n IO (RuntimeSplice n Builder))
            -- ^ Use 'promiseChildrenWith' or a variant to create this
            -- function.
            -> RuntimeSplice n [a]
            -- ^ Runtime computation returning a list of items
            -> Splice n
mapPromises f getList = do
    singlePromise <- newEmptyPromise
    runSingle <- f singlePromise
    return $ yieldRuntime $ do
        list <- getList
        htmls <- forM list $ \item ->
            putPromise singlePromise item >> runSingle
        return $ mconcat htmls


------------------------------------------------------------------------------
-- | Returns a runtime computation that simply renders the node's children.
promiseChildren :: Monad m => HeistT m IO (RuntimeSplice m Builder)
promiseChildren = liftM codeGen runChildren
{-# INLINE promiseChildren #-}


------------------------------------------------------------------------------
-- | Binds a list of Builder splices before using the children of the spliced
-- node as a view.
promiseChildrenWith :: (Monad n)
                    => [(Text, a -> Builder)]
                    -> Promise a
                    -> HeistT n IO (RuntimeSplice n Builder)
promiseChildrenWith splices prom =
    localHS (bindSplices splices') promiseChildren
  where
    fieldSplice p f = return $ yieldRuntime $ liftM f $ getPromise p
    splices' = map (second (fieldSplice prom)) splices


------------------------------------------------------------------------------
-- | Wrapper that composes a transformation function with the second item in
-- each of the tuples before calling promiseChildren.
promiseChildrenWithTrans :: Monad n
                         => (b -> Builder)
                         -> [(Text, a -> b)]
                         -> Promise a
                         -> HeistT n IO (RuntimeSplice n Builder)
promiseChildrenWithTrans f = promiseChildrenWith . map (second (f .))


------------------------------------------------------------------------------
-- | Binds a list of Text splices before using the children of the spliced
-- node as a view.
promiseChildrenWithText :: (Monad n)
                        => [(Text, a -> Text)]
                        -> Promise a
                        -> HeistT n IO (RuntimeSplice n Builder)
promiseChildrenWithText = promiseChildrenWithTrans fromText


------------------------------------------------------------------------------
-- | Binds a list of Node splices before using the children of the spliced
-- node as a view.  Note that this will slow down page generation because the
-- nodes generated by the splices must be traversed and rendered into a
-- ByteString at runtime.
promiseChildrenWithNodes :: (Monad n)
                         => [(Text, a -> [X.Node])]
                         -> Promise a
                         -> HeistT n IO (RuntimeSplice n Builder)
promiseChildrenWithNodes =
    promiseChildrenWithTrans (X.renderHtmlFragment X.UTF8)


------------------------------------------------------------------------------
-- | Yields pure text known at load time.
pureTextChunk :: Text -> Chunk n
pureTextChunk t = Pure $ T.encodeUtf8 t
{-# INLINE pureTextChunk #-}


------------------------------------------------------------------------------
-- | Yields a pure Builder known at load time.  You should use this and
-- 'yieldPureText' as much as possible to maximize the parts of your page that
-- can be compiled to static ByteStrings.
yieldPure :: Builder -> DList (Chunk m)
yieldPure = DL.singleton . Pure . toByteString
{-# INLINE yieldPure #-}


------------------------------------------------------------------------------
-- | Yields a runtime action that returns a builder.
yieldRuntime :: RuntimeSplice m Builder -> DList (Chunk m)
yieldRuntime = DL.singleton . RuntimeHtml
{-# INLINE yieldRuntime #-}


------------------------------------------------------------------------------
-- | Yields a runtime action that returns no value and is only needed for its
-- side effect.
yieldRuntimeEffect :: Monad m => RuntimeSplice m () -> DList (Chunk m)
yieldRuntimeEffect = DL.singleton . RuntimeAction
{-# INLINE yieldRuntimeEffect #-}


------------------------------------------------------------------------------
-- | A convenience wrapper around yieldPure for working with Text.  Roughly
-- equivalent to 'textSplice' from Heist.Interpreted.
yieldPureText :: Text -> DList (Chunk m)
yieldPureText = DL.singleton . pureTextChunk
{-# INLINE yieldPureText #-}


------------------------------------------------------------------------------
-- | Convenience wrapper around yieldRuntime allowing you to work with Text.
yieldRuntimeText :: Monad m => RuntimeSplice m Text -> DList (Chunk m)
yieldRuntimeText = yieldRuntime .  liftM fromText
{-# INLINE yieldRuntimeText #-}


------------------------------------------------------------------------------
-- | This lets you turn a plain runtime monad function returning a Builder
-- into a compiled splice.
yieldLater :: Monad m => m Builder -> DList (Chunk m)
yieldLater = yieldRuntime . RuntimeSplice . lift
{-# INLINE yieldLater #-}


------------------------------------------------------------------------------
-- | Returns a computation that performs load-time splice processing on the
-- supplied list of nodes.
runNodeList :: Monad n => [X.Node] -> Splice n
runNodeList = mapSplices runNode


------------------------------------------------------------------------------
-- | Runs a single splice and returns the builder computation.
runSplice :: (Monad n)
          => X.Node
          -> HeistState n
          -> Splice n
          -> IO [Chunk n]
runSplice node hs splice = do
    (!a,_) <- runHeistT splice node hs
    return $! consolidate a


------------------------------------------------------------------------------
-- | Runs a DocumentFile with the appropriate template context set.
runDocumentFile :: Monad n
                => TPath
                -> DocumentFile
                -> Splice n
runDocumentFile tpath df = do
    modifyHS (setCurTemplateFile curPath .  setCurContext tpath)
    runNodeList nodes
  where
    curPath     = dfFile df
    nodes       = X.docContent $! dfDoc df


------------------------------------------------------------------------------
compileTemplate :: Monad n
                => HeistState n
                -> TPath
                -> DocumentFile
                -> IO [Chunk n]
compileTemplate hs tpath df = do
    !chunks <- runSplice nullNode hs $! runDocumentFile tpath df
    return chunks
  where
    -- This gets overwritten in runDocumentFile
    nullNode = X.TextNode ""


------------------------------------------------------------------------------
compileTemplates :: Monad n => HeistState n -> IO (HeistState n)
compileTemplates hs = do
    ctm <- compileTemplates' hs
    return $! hs { _compiledTemplateMap = ctm }
--    let f = flip evalStateT HE.empty . unRT . codeGen
--    return $! hs { _compiledTemplateMap = H.map (first f) ctm }


------------------------------------------------------------------------------
compileTemplates' :: Monad m
                  => HeistState m
                  -> IO (H.HashMap TPath ([Chunk m], MIMEType))
compileTemplates' hs = do
    ctm <- foldM runOne H.empty tpathDocfiles
    return $! ctm
  where
    tpathDocfiles :: [(TPath, DocumentFile)]
    tpathDocfiles = map (\(a,b) -> (a, b))
                        (H.toList $ _templateMap hs)

    runOne tmap (tpath, df) = do
        !mHtml <- compileTemplate hs tpath df
        return $! H.insert tpath (mHtml, mimeType $! dfDoc df) tmap


------------------------------------------------------------------------------
-- | Consolidate consecutive Pure Chunks.
consolidate :: (Monad m) => DList (Chunk m) -> [Chunk m]
consolidate = consolidateL . DL.toList
  where
    consolidateL []     = []
    consolidateL (y:ys) = boilDown [] $! go [] y ys
      where
        ----------------------------------------------------------------------
        go soFar x [] = x : soFar
    
        go soFar (Pure a) ((Pure b) : xs) =
            go soFar (Pure $! a `mappend` b) xs
    
        go soFar (RuntimeHtml a) ((RuntimeHtml b) : xs) =
            go soFar (RuntimeHtml $! a `mappend` b) xs
    
        go soFar (RuntimeHtml a) ((RuntimeAction b) : xs) =
            go soFar (RuntimeHtml $! a >>= \x -> b >> return x) xs
    
        go soFar (RuntimeAction a) ((RuntimeHtml b) : xs) =
            go soFar (RuntimeHtml $! a >> b) xs
    
        go soFar (RuntimeAction a) ((RuntimeAction b) : xs) =
            go soFar (RuntimeAction $! a >> b) xs
    
        go soFar a (b : xs) = go (a : soFar) b xs
    
        ----------------------------------------------------------------------
        boilDown soFar []              = soFar
    
        boilDown soFar ((Pure h) : xs) = boilDown ((Pure $! h) : soFar) xs
    
        boilDown soFar (x : xs) = boilDown (x : soFar) xs


------------------------------------------------------------------------------
-- | Given a list of output chunks, consolidate turns consecutive runs of
-- @Pure Html@ values into maximally-efficient pre-rendered strict
-- 'ByteString' chunks.
codeGen :: Monad m => DList (Chunk m) -> RuntimeSplice m Builder
codeGen l = V.foldr mappend mempty $!
            V.map toAct $! V.fromList $! consolidate l
  where
    toAct !(RuntimeHtml !m)   = m
    toAct !(Pure !h)          = return $! fromByteString h
    toAct !(RuntimeAction !m) = m >> return mempty
{-# INLINE codeGen #-}


------------------------------------------------------------------------------
-- | Looks up a splice in the compiled splice map.
lookupSplice :: Text -> HeistT n IO (Maybe (Splice n))
lookupSplice nm = getsHS (H.lookup nm . _compiledSpliceMap)


------------------------------------------------------------------------------
-- | Runs a single node.  If there is no splice referenced anywhere in the
-- subtree, then it is rendered as a pure chunk, otherwise it calls
-- compileNode to generate the appropriate runtime computation.
runNode :: Monad n => X.Node -> Splice n
runNode node = localParamNode (const node) $ do
    isStatic <- subtreeIsStatic node
    if isStatic
      then return $! yieldPure $!
             X.renderHtmlFragment X.UTF8 [parseAttrs node]
      else compileNode node


parseAttrs :: X.Node -> X.Node
parseAttrs (X.Element nm attrs ch) = newAttrs `seq` X.Element nm newAttrs ch
  where
    newAttrs = map parseAttr attrs
parseAttrs !n = n

parseAttr :: (Text, Text) -> (Text, Text)
parseAttr (k,v) = (k, T.concat $! map cvt ast)
  where
    !ast = case AP.feed (AP.parse attParser v) "" of
            (AP.Done _ res) -> res
            (AP.Fail _ _ _) -> []
            (AP.Partial _ ) -> []
    cvt (Literal x) = x
    cvt (Ident i) = T.concat ["${", i, "}"]

------------------------------------------------------------------------------
-- | Checks whether a node's subtree is static and can be rendered up front at
-- load time.
subtreeIsStatic :: X.Node -> HeistT n IO Bool
subtreeIsStatic (X.Element nm attrs ch) = do
    isNodeDynamic <- liftM isJust $ lookupSplice nm
    attrSplices <- getsHS _attrSpliceMap
    let hasSubstitutions (k,v) = hasAttributeSubstitutions v ||
                                 H.member k attrSplices
    if isNodeDynamic
      then return False
      else do
          let hasDynamicAttrs = any hasSubstitutions attrs
          if hasDynamicAttrs
            then return False
            else do
                staticSubtrees <- mapM subtreeIsStatic ch
                return $ and staticSubtrees

subtreeIsStatic _ = return True


------------------------------------------------------------------------------
-- | Checks whether a string has any attribute substitutions.
hasAttributeSubstitutions :: Text -> Bool
hasAttributeSubstitutions txt = any isIdent ast
  where
    ast = case AP.feed (AP.parse attParser txt) "" of
            (AP.Done _ res) -> res
            (AP.Fail _ _ _) -> []
            (AP.Partial _ ) -> []


------------------------------------------------------------------------------
-- |
parseAtt :: Monad n => (Text, Text) -> HeistT n IO (DList (Chunk n))
parseAtt (k,v) = do
    mas <- getsHS (H.lookup k . _attrSpliceMap)
    maybe doInline (return . doAttrSplice) mas

  where
    cvt (Literal x) = return $ yieldPureText x
    cvt (Ident x) =
        localParamNode (const $ X.Element x [] []) $ getAttributeSplice x

    -- Handles inline parsing of $() splice syntax in attributes
    doInline = do
        let ast = case AP.feed (AP.parse attParser v) "" of
                    (AP.Done _ res) -> res
                    (AP.Fail _ _ _) -> []
                    (AP.Partial _ ) -> []
        chunks <- mapM cvt ast
        let value = DL.concat chunks
        return $ attrToChunk k value

    -- Handles attribute splices
    doAttrSplice splice = DL.singleton $ RuntimeHtml $ do
        res <- splice v
        return $ mconcat $ map attrToBuilder res

    
------------------------------------------------------------------------------
-- | Given a 'X.Node' in the DOM tree, produces a \"runtime splice\" that will
-- generate html at runtime.
compileNode :: Monad n => X.Node -> Splice n
compileNode (X.Element nm attrs ch) =
    -- Is this node a splice, or does it merely contain splices?
    lookupSplice nm >>= fromMaybe compileStaticElement
  where
    tag0 = T.append "<" nm
    end = T.concat [ "</" , nm , ">"]
    -- If the tag is not a splice, but it contains dynamic children
    compileStaticElement = do
        -- Parse the attributes: we have Left for static and Right for runtime
        compiledAttrs <- runAttributes attrs

        childHtml <- runNodeList ch

        return $! if null (DL.toList childHtml)
          then DL.concat [ DL.singleton $! pureTextChunk $! tag0
                         , DL.concat compiledAttrs
                         , DL.singleton $! pureTextChunk " />"
                         ]
          else DL.concat [ DL.singleton $! pureTextChunk $! tag0
                         , DL.concat compiledAttrs
                         , DL.singleton $! pureTextChunk ">"
                         , childHtml
                         , DL.singleton $! pureTextChunk $! end
                         ]
compileNode _ = error "impossible"


------------------------------------------------------------------------------
-- | Performs splice processing on a list of attributes.  This is useful in
-- situations where you need to stop recursion, but still run splice
-- processing on the node's attributes.
runAttributes :: Monad n => [(Text, Text)] -> HeistT n IO [DList (Chunk n)]
runAttributes = mapM parseAtt


attrToChunk :: Text -> DList (Chunk n) -> DList (Chunk n)
attrToChunk !k !v = do
    DL.concat
        [ DL.singleton $! pureTextChunk $! T.concat [" ", k, "=\""]
        , v, DL.singleton $! pureTextChunk "\"" ]
    

attrToBuilder :: (Text, Text) -> Builder
attrToBuilder (k,v)
  | T.null v  = mconcat
    [ fromText " "
    , fromText k
    ]
  | otherwise = mconcat
    [ fromText " "
    , fromText k
    , fromText "=\""
    , fromText v
    , fromText "\""
    ]


------------------------------------------------------------------------------
getAttributeSplice :: Text -> HeistT n IO (DList (Chunk n))
getAttributeSplice name =
    lookupSplice name >>= fromMaybe
      (return $ DL.singleton $ Pure $ T.encodeUtf8 $
       T.concat ["${", name, "}"])
{-# INLINE getAttributeSplice #-}


------------------------------------------------------------------------------
-- | Promises are used for referencing the results of future runtime
-- computations during load time splice processing.
newtype Promise a = Promise (HE.Key a)


------------------------------------------------------------------------------
-- | Gets the result of a promised runtime computation.
getPromise :: (Monad m) => Promise a -> RuntimeSplice m a
getPromise (Promise k) = do
    mb <- gets (HE.lookup k)
    return $ fromMaybe e mb

  where
    e = error $ "getPromise: dereferenced empty key (id "
                ++ show (HE.getKeyId k) ++ ")"
{-# INLINE getPromise #-}


------------------------------------------------------------------------------
-- | Adds a promise to the runtime splice context.
putPromise :: (Monad m) => Promise a -> a -> RuntimeSplice m ()
putPromise (Promise k) x = modify (HE.insert k x)
{-# INLINE putPromise #-}


------------------------------------------------------------------------------
-- | Modifies a promise.
adjustPromise :: Monad m => Promise a -> (a -> a) -> RuntimeSplice m ()
adjustPromise (Promise k) f = modify (HE.adjust f k)
{-# INLINE adjustPromise #-}


------------------------------------------------------------------------------
-- | Creates an empty promise.
newEmptyPromise :: HeistT n IO (Promise a)
newEmptyPromise = do
    keygen <- getsHS _keygen
    key    <- liftIO $ HE.makeKey keygen
    return $! Promise key
{-# INLINE newEmptyPromise #-}


-- ------------------------------------------------------------------------------
-- -- | Creates an empty promise with some error checking to help with debugging.
-- newEmptyPromiseWithError :: (Monad n)
--                          => String -> HeistT n IO (Promise a)
-- newEmptyPromiseWithError from = do
--     keygen <- getsHS _keygen
--     prom   <- liftM Promise $ liftIO $ HE.makeKey keygen
--     yieldRuntimeEffect $ putPromise prom
--                        $ error
--                        $ "deferenced empty promise created at" ++ from
--     return prom
-- {-# INLINE newEmptyPromiseWithError #-}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Creates a promise for a future runtime computation.
-- promise :: (Monad n) => n a -> HeistT n IO (Promise a)
-- promise act = runtimeSplicePromise (lift act)
-- {-# INLINE promise #-}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Turns a RuntimeSplice computation into a promise.
-- runtimeSplicePromise :: (Monad n)
--                      => RuntimeSplice n a
--                      -> HeistT n IO (Promise a)
-- runtimeSplicePromise act = do
--     prom <- newEmptyPromiseWithError "runtimeSplicePromise"
-- 
--     let m = do
--         x <- act
--         putPromise prom x
--         return ()
-- 
--     yieldRuntimeEffect m
--     return prom
-- {-# INLINE runtimeSplicePromise #-}
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Sets up a runtime transformation on a 'Promise'.
-- withPromise :: (Monad n)
--             => Promise a
--             -> (a -> n b)
--             -> HeistT n IO (Promise b)
-- withPromise promA f = do
--     promB <- newEmptyPromiseWithError "withPromise"
-- 
--     let m = do
--         a <- getPromise promA
--         b <- lift $ f a
--         putPromise promB b
--         return ()
-- 
--     yieldRuntimeEffect m
--     return promB
-- {-# INLINE withPromise #-}


------------------------------------------------------------------------------
-- | Binds a compiled splice.  This function should not be exported.
bindSplice :: Text             -- ^ tag name
           -> Splice n         -- ^ splice action
           -> HeistState n     -- ^ source state
           -> HeistState n
bindSplice n v ts =
    ts { _compiledSpliceMap = H.insert n v (_compiledSpliceMap ts) }


------------------------------------------------------------------------------
-- | Binds a list of compiled splices.  This function should not be exported.
bindSplices :: [(Text, Splice n)]  -- ^ splices to bind
            -> HeistState n        -- ^ source state
            -> HeistState n
bindSplices ss ts = foldr (uncurry bindSplice) ts ss


------------------------------------------------------------------------------
-- | Adds a list of compiled splices to the splice map.  This function is
-- useful because it allows compiled splices to bind other compiled splices
-- during load-time splice processing.
addSplices :: Monad m => [(Text, Splice n)] -> HeistT n m ()
addSplices ss = modifyHS (bindSplices ss)
{-# DEPRECATED addSplices "addSplices will be removed in the next release!  Use withLocalSplices instead."#-}


------------------------------------------------------------------------------
-- | Adds a list of compiled splices to the splice map.  This function is
-- useful because it allows compiled splices to bind other compiled splices
-- during load-time splice processing.
withLocalSplices :: [(Text, Splice n)]
                 -> [(Text, AttrSplice n)]
                 -> HeistT n IO a
                 -> HeistT n IO a
withLocalSplices ss as = localHS (bindSplices ss . bindAttributeSplices as)


------------------------------------------------------------------------------
-- | Looks up a compiled template and returns a runtime monad computation that
-- constructs a builder.
renderTemplate :: Monad n
               => HeistState n
               -> ByteString
               -> Maybe (n Builder, MIMEType)
renderTemplate hs nm =
    fmap (first (interpret . DL.fromList) . fst) $!
      lookupTemplate nm hs _compiledTemplateMap


------------------------------------------------------------------------------
-- | Looks up a compiled template and returns a compiled splice.
callTemplate :: Monad n
             => ByteString
             -> HeistT n IO (DList (Chunk n))
callTemplate nm = do
    hs <- getHS
    return $ maybe DL.empty (DL.fromList . fst . fst) $
      lookupTemplate nm hs _compiledTemplateMap


interpret :: Monad m => DList (Chunk m) -> m Builder
interpret = flip evalStateT HE.empty . unRT . codeGen


------------------------------------------------------------------------------
-- Functions for manipulating lists of compiled splices
------------------------------------------------------------------------------


mapSnd :: (b -> c) -> [(d, b)] -> [(d, c)]
mapSnd = map . second

applySnd :: a -> [(d, a -> b)] -> [(d, b)]
applySnd a = mapSnd ($a)

textSplices :: [(Text, a -> Text)] -> [(Text, a -> Builder)]
textSplices = mapSnd textSplice

textSplice :: (a -> Text) -> a -> Builder
textSplice f = fromText . f

nodeSplices :: [(Text, a -> [X.Node])] -> [(Text, a -> Builder)]
nodeSplices = mapSnd nodeSplice

nodeSplice :: (a -> [X.Node]) -> a -> Builder
nodeSplice f = X.renderHtmlFragment X.UTF8 . f

pureSplices :: Monad m => [(d, a -> Builder)] -> [(d, Promise a -> Splice m)]
pureSplices = mapSnd pureSplice

pureSplice :: Monad m => (a -> Builder) -> Promise a -> Splice m
pureSplice f p = do
    return $ yieldRuntime $ do
        a <- getPromise p
        return $ f a

mapInputPromise :: Monad m
                => (a -> b)
                -> (Promise b -> Splice m)
                -> Promise a -> Splice m
mapInputPromise f g p1 = do
    p2 <- newEmptyPromise
    let action = yieldRuntimeEffect $ do
        a <- getPromise p1
        putPromise p2 (f a)
    res <- g p2
    return $ action `mappend` res


------------------------------------------------------------------------------
-- | Allows you to use deferred Promises in a compiled splice.  It takes care
-- of the boilerplate of creating and storing data in a promise to be used at
-- load time when compiled splices are processed.  This function is similar to
-- mapPromises but runs on a single value instead of a list.
defer :: Monad n
      => (Promise a -> Splice n)
      -> RuntimeSplice n a
      -> Splice n
defer f getItem = do
    promise <- newEmptyPromise
    chunks <- f promise
    return $ yieldRuntime $ do
        item <- getItem
        putPromise promise item
        codeGen chunks


------------------------------------------------------------------------------
-- | Takes a promise function and a runtime action returning a list of items
-- that fit in the promise and returns a Splice that executes the promise
-- function for each item and concatenates the results.
deferMany :: Monad n
          => (Promise a -> Splice n)
          -> RuntimeSplice n [a]
          -> Splice n
deferMany f getItems = do
    promise <- newEmptyPromise
    chunks <- f promise
    return $ yieldRuntime $ do
        items <- getItems
        res <- forM items $ \item -> do
            putPromise promise item
            codeGen chunks
        return $ mconcat res


withSplices :: Monad n
            => Splice n
            -> [(Text, Promise a -> Splice n)]
            -> n a
            -> Splice n
withSplices splice splices runtimeAction = do
    p <- newEmptyPromise
    let splices' = mapSnd ($p) splices
    chunks <- withLocalSplices splices' [] splice
    let fillPromise = yieldRuntimeEffect $ putPromise p =<< lift runtimeAction
    return $ fillPromise `mappend` chunks


------------------------------------------------------------------------------
-- | Gets a list of items at runtime, then for each item it runs the splice
-- with the list of splices bound.  There is no pure variant of this function
-- because the desired behavior can only be achieved as a function of a
-- Promise.
manyWithSplices :: Monad n
                => Splice n
                -- ^ Splice to run for each of the items in the runtime list.
                -- You'll frequently use 'runChildren' here.
                -> [(Text, Promise a -> Splice n)]
                -- ^ List of splices to bind
                -> n [a]
                -- ^ Runtime action returning a list of items to render.
                -> Splice n
manyWithSplices splice splices runtimeAction = do
    p <- newEmptyPromise
    let splices' = mapSnd ($p) splices
    chunks <- withLocalSplices splices' [] splice
    return $ yieldRuntime $ do
        items <- lift runtimeAction
        res <- forM items $ \item -> putPromise p item >> codeGen chunks
        return $ mconcat res


withPureSplices :: Monad n
                => Splice n
                -> [(Text, a -> Builder)]
                -> n a
                -> Splice n
withPureSplices splice splices action = do
    let fieldSplice g = return $ yieldRuntime $ liftM g $ lift action
    let splices' = map (second fieldSplice) splices
    withLocalSplices splices' [] splice


