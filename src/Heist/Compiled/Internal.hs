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
import           Heist.Common
import           Heist.Types
import qualified Text.XmlHtml                    as X


------------------------------------------------------------------------------
-- | A Splice is a HeistT computation that returns a 'Template'.
type Splice n = HeistT n IO (DList (Chunk n))


------------------------------------------------------------------------------
newtype Promise a = Promise (HE.Key a)


------------------------------------------------------------------------------
runNodeList :: [X.Node] -> Splice n
runNodeList = mapSplices runNode


------------------------------------------------------------------------------
lookupCompiledTemplate :: ByteString -> HeistState n m -> Maybe (n Builder)
lookupCompiledTemplate nm hs =
    fmap fst $ lookupTemplate nm hs _compiledTemplateMap


------------------------------------------------------------------------------
runSplice :: (Monad n)
          => X.Node
          -> HeistState n IO
          -> Splice n
          -> IO (n Builder)
runSplice node hs splice = do
    (!a,_) <- runHeistT splice node hs
    return $! (flip evalStateT HE.empty $! unRT $! codeGen a)


------------------------------------------------------------------------------
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
                => HeistState n IO
                -> TPath
                -> DocumentFile
                -> IO (n Builder)
compileTemplate hs tpath df = do
    runSplice nullNode hs $ runDocumentFile tpath df
  where
    -- This gets overwritten in runDocumentFile
    nullNode = X.TextNode ""


------------------------------------------------------------------------------
compileTemplates :: Monad n => HeistState n IO -> IO (HeistState n IO)
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
        -- FIXME Why isn't this used?
        --render h = unsafeByteString $! S.concat $! L.toChunks $! renderHtml h

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
yieldChunk :: Monad m => a -> m (DList a)
yieldChunk = return . DL.singleton
{-# INLINE yieldChunk #-}


------------------------------------------------------------------------------
yield :: Builder -> Splice n
yield = yieldChunk . Pure
{-# INLINE yield #-}


------------------------------------------------------------------------------
pureText :: Text -> Chunk n
pureText = Pure . fromByteString . T.encodeUtf8
{-# INLINE pureText #-}


------------------------------------------------------------------------------
yieldText :: Text -> Splice n
yieldText = yieldChunk . pureText
{-# INLINE yieldText #-}


------------------------------------------------------------------------------
yieldRuntimeSplice :: RuntimeSplice n () -> Splice n
yieldRuntimeSplice = yieldChunk . RuntimeAction
{-# INLINE yieldRuntimeSplice #-}


------------------------------------------------------------------------------
yieldRuntime :: RuntimeSplice n Builder -> Splice n
yieldRuntime = yieldChunk . RuntimeHtml
{-# INLINE yieldRuntime #-}


------------------------------------------------------------------------------
yieldRuntimeText :: Monad n => RuntimeSplice n Text -> Splice n
yieldRuntimeText = yieldChunk . RuntimeHtml .
                   liftM (fromByteString . T.encodeUtf8)
{-# INLINE yieldRuntimeText #-}


------------------------------------------------------------------------------
yieldLater :: (Monad n) => n Builder -> Splice n
yieldLater = yieldRuntime . RuntimeSplice . lift
{-# INLINE yieldLater #-}


------------------------------------------------------------------------------
yieldPromise :: (Monad n) => Promise Builder -> Splice n
yieldPromise p = yieldRuntime $ getPromise p
{-# INLINE yieldPromise #-}


------------------------------------------------------------------------------
lookupSplice :: Text -> HeistT n IO (Maybe (Splice n))
lookupSplice nm = getsTS (H.lookup nm . _compiledSpliceMap)


------------------------------------------------------------------------------
runNode :: X.Node -> Splice n
runNode node = localParamNode (const node) $ do
    isStatic <- subtreeIsStatic node
    if isStatic
      then yield $! X.renderHtmlFragment X.UTF8 [node]
      else compileNode node


------------------------------------------------------------------------------
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

        return $ DL.concat [ DL.singleton $ pureText tag0
                           , DL.concat compiledAttrs
                           , DL.singleton $ pureText ">"
                           , childHtml
                           , DL.singleton $ pureText end
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
    return $ DL.concat [ DL.singleton $ pureText $ T.concat [" ", k, "=\""]
                       , value, DL.singleton $ pureText "\"" ]
  where
    cvt (Literal x) = yieldText x
    cvt (Ident x) =
        localParamNode (const $ X.Element x [] []) $ getAttributeSplice x


------------------------------------------------------------------------------
getAttributeSplice :: Text -> HeistT n IO (DList (Chunk n))
getAttributeSplice name =
    lookupSplice name >>= fromMaybe (return DL.empty)
{-# INLINE getAttributeSplice #-}


------------------------------------------------------------------------------
getPromise :: (Monad m) => Promise a -> RuntimeSplice m a
getPromise (Promise k) = do
    mb <- gets (HE.lookup k)
    return $ fromMaybe e mb

  where
    e = error $ "getPromise: dereferenced empty key (id "
                ++ show (HE.getKeyId k) ++ ")"
{-# INLINE getPromise #-}


------------------------------------------------------------------------------
putPromise :: (Monad m) => Promise a -> a -> RuntimeSplice m ()
putPromise (Promise k) x = modify (HE.insert k x)
{-# INLINE putPromise #-}


------------------------------------------------------------------------------
adjustPromise :: Monad m => Promise a -> (a -> a) -> RuntimeSplice m ()
adjustPromise (Promise k) f = modify (HE.adjust f k)
{-# INLINE adjustPromise #-}


------------------------------------------------------------------------------
newEmptyPromise :: HeistT n IO (Promise a)
newEmptyPromise = do
    keygen <- getsTS _keygen
    key    <- liftIO $ HE.makeKey keygen
    return $! Promise key
{-# INLINE newEmptyPromise #-}


------------------------------------------------------------------------------
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
promise :: (Monad n) => n a -> HeistT n IO (Promise a)
promise act = runtimeSplicePromise (lift act)
{-# INLINE promise #-}


------------------------------------------------------------------------------
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
bindSplice :: Text             -- ^ tag name
                -> Splice n  -- ^ splice action
                -> HeistState n m   -- ^ source state
                -> HeistState n m
bindSplice n v ts =
    ts { _compiledSpliceMap = H.insert n v (_compiledSpliceMap ts) }


------------------------------------------------------------------------------
bindSplices :: [(Text, Splice n)]  -- ^ splices to bind
                 -> HeistState n m             -- ^ source state
                 -> HeistState n m
bindSplices ss ts = foldr (uncurry bindSplice) ts ss


addSplices :: Monad m => [(Text, Splice n)] -> HeistT n m ()
addSplices ss = modifyTS (bindSplices ss)


------------------------------------------------------------------------------
-- | Converts 'Text' to a splice yielding the text, html-encoded.
textSplice :: Text -> Splice n
textSplice = yieldText


------------------------------------------------------------------------------
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
    

