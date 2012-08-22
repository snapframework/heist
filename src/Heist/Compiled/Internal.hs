{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Heist.Compiled.Internal where

import           Blaze.ByteString.Builder
import           Control.Arrow
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.Text            as AP
import           Data.ByteString (ByteString)
import           Data.DList                      (DList)
import qualified Data.DList                      as DL
import qualified Data.HashMap.Strict             as H
import qualified Data.HeterogeneousEnvironment   as HE
import           Data.Maybe
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Vector                     as V
import           Prelude                         hiding (catch)
import qualified Text.XmlHtml                    as X

import           Heist.Common
import           Heist.Types

------------------------------------------------------------------------------
-- | A compiled Splice is a HeistT computation that returns a @DList
-- (Chunk m)@.
type Splice n = HeistT n IO (DList (Chunk n))


------------------------------------------------------------------------------
-- | Promises are used for referencing the results of future runtime
-- computations during load time splice processing.
newtype Promise a = Promise (HE.Key a)


------------------------------------------------------------------------------
-- | Returns a computation that performs load-time splice processing on the
-- supplied list of nodes.
runNodeList :: [X.Node] -> Splice n
runNodeList = mapSplices runNode


------------------------------------------------------------------------------
-- | Runs a single splice and returns the builder computation.
runSplice :: (Monad n)
          => X.Node
          -> HeistState n
          -> Splice n
          -> IO (n Builder)
runSplice node hs splice = do
    (!a,_) <- runHeistT splice node hs
    return $! (flip evalStateT HE.empty $! unRT $! codeGen a)


------------------------------------------------------------------------------
-- | Runs a DocumentFile with the appropriate template context set.
runDocumentFile :: TPath
                -> DocumentFile
                -> Splice n
runDocumentFile tpath df = do
    modifyTS (setCurTemplateFile curPath .  setCurContext tpath)
    runNodeList nodes
  where
    curPath     = dfFile df
    nodes       = X.docContent $ dfDoc df


------------------------------------------------------------------------------
compileTemplate :: Monad n
                => HeistState n
                -> TPath
                -> DocumentFile
                -> IO (n Builder)
compileTemplate hs tpath df = do
    runSplice nullNode hs $ runDocumentFile tpath df
  where
    -- This gets overwritten in runDocumentFile
    nullNode = X.TextNode ""


------------------------------------------------------------------------------
compileTemplates :: Monad n => HeistState n -> IO (HeistState n)
compileTemplates hs = do
    ctm <- foldM runOne H.empty tpathDocfiles
    return $ hs { _compiledTemplateMap = ctm }
  where
    tpathDocfiles :: [(TPath, DocumentFile)]
    tpathDocfiles = map (\(a,b) -> (a, b))
                        (H.toList $ _templateMap hs)

    runOne tmap (tpath, df) = do
        mHtml <- compileTemplate hs tpath df
        return $! H.insert tpath mHtml tmap


------------------------------------------------------------------------------
-- | Given a list of output chunks, consolidate turns consecutive runs of
-- @Pure Html@ values into maximally-efficient pre-rendered strict
-- 'ByteString' chunks.
codeGen :: Monad m => DList (Chunk m) -> RuntimeSplice m Builder
codeGen = compileConsolidated . consolidate . DL.toList
  where
    consolidate :: (Monad m) => [Chunk m] -> [Chunk m]
    consolidate []     = []
    consolidate (y:ys) = boilDown [] $! go [] y ys
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


    --------------------------------------------------------------------------
    compileConsolidated :: (Monad m) => [Chunk m] -> RuntimeSplice m Builder
    compileConsolidated l = V.foldr mappend mempty v
      where
        toAct (RuntimeHtml m)   = m
        toAct (Pure h)          = return h
        toAct (RuntimeAction m) = m >> return mempty

        !v = V.map toAct $! V.fromList l
    {-# INLINE compileConsolidated #-}
{-# INLINE codeGen #-}


------------------------------------------------------------------------------
-- | Helper function for creating DList splices.
yieldChunk :: Monad m => a -> m (DList a)
yieldChunk = return . DL.singleton
{-# INLINE yieldChunk #-}


------------------------------------------------------------------------------
-- | Yields a pure Builder known at load time.
yieldPure :: Builder -> Splice n
yieldPure = yieldChunk . Pure
{-# INLINE yieldPure #-}


------------------------------------------------------------------------------
-- | Yields pure text known at load time.
pureTextChunk :: Text -> Chunk n
pureTextChunk = Pure . fromByteString . T.encodeUtf8
{-# INLINE pureTextChunk #-}


------------------------------------------------------------------------------
-- | Roughly equivalent to 'textSplice' from Heist.Interpreted.
yieldPureText :: Text -> Splice n
yieldPureText = yieldChunk . pureTextChunk
{-# INLINE yieldPureText #-}


------------------------------------------------------------------------------
-- | Yields a runtime action that returns no value and is only needed for its
-- side effect.
yieldRuntimeSplice :: RuntimeSplice n () -> Splice n
yieldRuntimeSplice = yieldChunk . RuntimeAction
{-# INLINE yieldRuntimeSplice #-}


------------------------------------------------------------------------------
-- | Yields a runtime action that returns a builder.
yieldRuntime :: RuntimeSplice n Builder -> Splice n
yieldRuntime = yieldChunk . RuntimeHtml
{-# INLINE yieldRuntime #-}


------------------------------------------------------------------------------
-- | Convenience wrapper around yieldRuntime allowing you to work with Text.
yieldRuntimeText :: Monad n => RuntimeSplice n Text -> Splice n
yieldRuntimeText = yieldRuntime .  liftM (fromByteString . T.encodeUtf8)
{-# INLINE yieldRuntimeText #-}


------------------------------------------------------------------------------
yieldLater :: (Monad n) => n Builder -> Splice n
yieldLater = yieldRuntime . RuntimeSplice . lift
{-# INLINE yieldLater #-}


------------------------------------------------------------------------------
-- | Yields the promised Builder.
yieldPromise :: (Monad n) => Promise Builder -> Splice n
yieldPromise p = yieldRuntime $ getPromise p
{-# INLINE yieldPromise #-}


------------------------------------------------------------------------------
-- | Looks up a splice in the compiled splice map.
lookupSplice :: Text -> HeistT n IO (Maybe (Splice n))
lookupSplice nm = getsTS (H.lookup nm . _compiledSpliceMap)


------------------------------------------------------------------------------
-- | Runs a single node.  If there is no splice referenced anywhere in the
-- subtree, then it is rendered as a pure chunk, otherwise it is compiled to
-- a runtime computation.
runNode :: X.Node -> Splice n
runNode node = localParamNode (const node) $ do
    isStatic <- subtreeIsStatic node
    if isStatic
      then yieldPure $! X.renderHtmlFragment X.UTF8 [node]
      else compileNode node


------------------------------------------------------------------------------
-- | Checks whether a node's subtree is static and can be rendered up front at
-- load time.
subtreeIsStatic :: X.Node -> HeistT n IO Bool
subtreeIsStatic (X.Element nm attrs ch) = do
    isNodeDynamic <- liftM isJust $ lookupSplice nm
    if isNodeDynamic
      then return False
      else do
          let hasDynamicAttrs = any hasSubstitutions attrs
          if hasDynamicAttrs
            then return False
            else do
                staticSubtrees <- mapM subtreeIsStatic ch
                return $ and staticSubtrees
  where
    hasSubstitutions (k,v) = hasAttributeSubstitutions k ||
                             hasAttributeSubstitutions v

subtreeIsStatic _ = return True


------------------------------------------------------------------------------
-- | Checks whether a string has any attribute substitutions.
hasAttributeSubstitutions :: Text -> Bool
hasAttributeSubstitutions txt = all isLiteral ast
  where
    ast = case AP.feed (AP.parse attParser txt) "" of
            (AP.Done _ res) -> res
            (AP.Fail _ _ _) -> []
            (AP.Partial _ ) -> []


------------------------------------------------------------------------------
-- | Given a 'X.Node' in the DOM tree, produces a \"runtime splice\" that will
-- generate html at runtime. Leaves the writer monad state untouched.
compileNode :: X.Node -> Splice n
compileNode (X.Element nm attrs ch) =
    -- Is this node a splice, or does it merely contain splices?
    lookupSplice nm >>= fromMaybe compileStaticElement
  where
    tag0 = T.append "<" nm
    end = T.concat [ "</" , nm , ">"]
    -- If the tag is not a splice, but it contains dynamic children
    compileStaticElement = do
        -- Parse the attributes: we have Left for static and Right for runtime
        -- TODO: decide: do we also want substitution in the key?
        compiledAttrs <- mapM parseAtt attrs

        childHtml <- runNodeList ch

        return $ DL.concat [ DL.singleton $ pureTextChunk tag0
                           , DL.concat compiledAttrs
                           , DL.singleton $ pureTextChunk ">"
                           , childHtml
                           , DL.singleton $ pureTextChunk end
                           ]
compileNode _ = error "impossible"


------------------------------------------------------------------------------
-- | If this function returns a 'Nothing', there are no dynamic splices in the
-- attribute text, and you can just spit out the text value statically.
-- Otherwise, the splice has to be resolved at runtime.
parseAtt :: (Text, Text) -> HeistT n IO (DList (Chunk n))
parseAtt (k,v) = do
    let ast = case AP.feed (AP.parse attParser v) "" of
                (AP.Done _ res) -> res
                (AP.Fail _ _ _) -> []
                (AP.Partial _ ) -> []
    chunks <- mapM cvt ast
    let value = DL.concat chunks
    return $ DL.concat [ DL.singleton $ pureTextChunk $ T.concat [" ", k, "=\""]
                       , value, DL.singleton $ pureTextChunk "\"" ]
  where
    cvt (Literal x) = yieldPureText x
    cvt (Escaped c) = yieldPureText $ T.singleton c
    cvt (Ident x) =
        localParamNode (const $ X.Element x [] []) $ getAttributeSplice x


------------------------------------------------------------------------------
getAttributeSplice :: Text -> HeistT n IO (DList (Chunk n))
getAttributeSplice name =
    lookupSplice name >>= fromMaybe (return DL.empty)
{-# INLINE getAttributeSplice #-}


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
    keygen <- getsTS _keygen
    key    <- liftIO $ HE.makeKey keygen
    return $! Promise key
{-# INLINE newEmptyPromise #-}


------------------------------------------------------------------------------
-- | Creates an empty promise with some error checking to help with debugging.
newEmptyPromiseWithError :: (Monad n)
                         => String -> HeistT n IO (Promise a)
newEmptyPromiseWithError from = do
    keygen <- getsTS _keygen
    prom   <- liftM Promise $ liftIO $ HE.makeKey keygen
    yieldRuntimeSplice $ putPromise prom
                       $ error
                       $ "deferenced empty promise created at" ++ from
    return prom
{-# INLINE newEmptyPromiseWithError #-}


------------------------------------------------------------------------------
-- | Creates a promise for a future runtime computation.
promise :: (Monad n) => n a -> HeistT n IO (Promise a)
promise act = runtimeSplicePromise (lift act)
{-# INLINE promise #-}


------------------------------------------------------------------------------
-- | Turns a RuntimeSplice computation into a promise.
runtimeSplicePromise :: (Monad n)
                     => RuntimeSplice n a
                     -> HeistT n IO (Promise a)
runtimeSplicePromise act = do
    prom <- newEmptyPromiseWithError "runtimeSplicePromise"

    let m = do
        x <- act
        putPromise prom x
        return ()

    yieldRuntimeSplice m
    return prom
{-# INLINE runtimeSplicePromise #-}


------------------------------------------------------------------------------
-- | Sets up a runtime transformation on a 'Promise'.
withPromise :: (Monad n)
            => Promise a
            -> (a -> n b)
            -> HeistT n IO (Promise b)
withPromise promA f = do
    promB <- newEmptyPromiseWithError "withPromise"

    let m = do
        a <- getPromise promA
        b <- lift $ f a
        putPromise promB b
        return ()

    yieldRuntimeSplice m
    return promB
{-# INLINE withPromise #-}


------------------------------------------------------------------------------
-- | Takes a promise function and a runtime action returning a list of items
-- that fit in the promise and returns a Splice that executes the promise
-- function for each item and concatenates the results.
mapPromises :: Monad n
            => (Promise a -> HeistT n IO (RuntimeSplice n Builder))
            -> n [a] -> Splice n
mapPromises f getList = do
    singlePromise <- newEmptyPromise
    runSingle <- f singlePromise
    yieldRuntime $ do
        list <- lift getList
        htmls <- forM list $ \item ->
            putPromise singlePromise item >> runSingle
        return $ mconcat htmls


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
addSplices ss = modifyTS (bindSplices ss)


------------------------------------------------------------------------------
-- | Returns a runtime computation that simply renders the node's children.
promiseChildren :: Monad m => HeistT m IO (RuntimeSplice m Builder)
promiseChildren = do
    res <- runNodeList . X.childNodes =<< getParamNode
    return $ codeGen res


------------------------------------------------------------------------------
-- | Binds a list of Builder splices before using the children of the spliced
-- node as a view.
promiseChildrenWith :: (Monad n)
                    => [(Text, a -> Builder)]
                    -> Promise a
                    -> HeistT n IO (RuntimeSplice n Builder)
promiseChildrenWith splices prom =
    localTS (bindSplices splices') promiseChildren
  where
    fieldSplice p f = yieldRuntime $ liftM f $ getPromise p
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
promiseChildrenWithText =
    promiseChildrenWithTrans (fromByteString . T.encodeUtf8)


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
-- | Looks up a compiled template and returns a runtime monad computation that
-- constructs a builder.
renderCompiledTemplate :: ByteString -> HeistState n -> Maybe (n Builder)
renderCompiledTemplate nm hs =
    fmap fst $ lookupTemplate nm hs _compiledTemplateMap


