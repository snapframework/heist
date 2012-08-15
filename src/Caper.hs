{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Caper where

import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.ByteString
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
--import           Text.Blaze.Html
--import qualified Text.Blaze.Html                 as Blaze
--import           Text.Blaze.Internal
--import qualified Text.Blaze.Html.Renderer.String as BlazeString
--import qualified Text.Blaze.Html.Renderer.Text   as BlazeText
--import           Text.Blaze.Html.Renderer.Utf8
import           Text.Templating.Heist.Common
import           Text.Templating.Heist.Types
import qualified Text.XmlHtml                    as X


-- $hookDoc
-- Heist hooks allow you to modify templates when they are loaded and before
-- and after they are run.  Every time you call one of the addAbcHook
-- functions the hook is added to onto the processing pipeline.  The hooks
-- processes the template in the order that they were added to the
-- HeistState.
--
-- The pre-run and post-run hooks are run before and after every template is
-- run/rendered.  You should be careful what code you put in these hooks
-- because it can significantly affect the performance of your site.


-- dlistRunNode :: Monad m
--              => X.Node
--              -> HeistT (Output m1) m (Output m1)
-- dlistRunNode (X.Element nm attrs ch) = do
--     -- Parse the attributes: we have Left for static and Right for runtime
--     -- TODO: decide: do we also want substitution in the key?
--     compiledAttrs <- mapM attSubst attrs
--     childHtml <- runNodeList ch
--     return $ DL.concat [ DL.singleton $ Pure tag0
--                        , DL.concat $ map renderAttr compiledAttrs
--                        , DL.singleton $ Pure ">"
--                        , childHtml
--                        , DL.singleton $ Pure end
--                        ]
--   where
--     tag0 = T.append "<" nm
--     end = T.concat [ "</" , nm , ">"]
--     renderAttr (n,v) = DL.concat [ DL.singleton $ Pure $ T.append " " n
--                                  , DL.singleton $ Pure "="
--                                  , v ]
-- dlistRunNode (X.TextNode t) = return $ textSplice t
-- dlistRunNode (X.Comment t) = return $ textSplice t
-- 
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Renders a template with the specified arguments passed to it.  This is a
-- -- convenience function for the common pattern of calling renderTemplate after
-- -- using bindString, bindStrings, or bindSplice to set up the arguments to the
-- -- template.
-- renderWithArgs :: Monad m
--                => [(Text, Text)]
--                -> HeistState (Output m) m
--                -> ByteString
--                -> m (Maybe (Builder, MIMEType))
-- renderWithArgs args ts = renderTemplate (bindStrings args ts)
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- | Renders a template from the specified HeistState to a 'Builder'.  The
-- -- MIME type returned is based on the detected character encoding, and whether
-- -- the root template was an HTML or XML format template.  It will always be
-- -- @text/html@ or @text/xml@.  If a more specific MIME type is needed for a
-- -- particular XML application, it must be provided by the application.
-- renderCaperTemplate :: Monad m
--                     => HeistState (Output m) m
--                     -> ByteString
--                     -> m (Maybe (Builder, MIMEType))
-- renderCaperTemplate ts name = evalHeistT tpl (X.TextNode "") ts
--   where
--     -- FIXME should not use lookupAndRun because that uses heist templates
--     -- instead of caper templates.
--     tpl = lookupAndRun name $ \(t,ctx) -> do
--         addDoctype $ maybeToList $ X.docType $ cdfDoc t
--         localTS (\ts' -> ts' {_curContext = ctx}) $ do
--             res <- runNodeList $ X.docContent $ cdfDoc t
--             return $ Just (res, mimeType $ cdfDoc t)


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
runNodeList :: [X.Node] -> CaperSplice n
runNodeList = mapSplices runNode


------------------------------------------------------------------------------
lookupCompiledTemplate :: ByteString -> HeistState n m -> Maybe (n Builder)
lookupCompiledTemplate nm hs =
    fmap fst $ lookupTemplate nm hs _caperTemplateMap


------------------------------------------------------------------------------
runSplice :: (Monad n)
          => X.Node
          -> HeistState n IO
          -> CaperSplice n
          -> IO (n Builder)
runSplice node hs splice = do
    (!a,_) <- runHeistT splice node hs
    return $! (flip evalStateT HE.empty $! unRT $! codeGen a)


------------------------------------------------------------------------------
runDocumentFile :: TPath
                -> DocumentFile
                -> CaperSplice n
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
    return $ hs { _caperTemplateMap = ctm }
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
yield :: Builder -> CaperSplice n
yield = yieldChunk . Pure
{-# INLINE yield #-}


------------------------------------------------------------------------------
pureText :: Text -> Chunk n
pureText = Pure . fromByteString . T.encodeUtf8
{-# INLINE pureText #-}


------------------------------------------------------------------------------
yieldText :: Text -> CaperSplice n
yieldText = yieldChunk . pureText
{-# INLINE yieldText #-}


------------------------------------------------------------------------------
yieldRuntimeSplice :: RuntimeSplice n () -> CaperSplice n
yieldRuntimeSplice = yieldChunk . RuntimeAction
{-# INLINE yieldRuntimeSplice #-}


------------------------------------------------------------------------------
yieldRuntime :: RuntimeSplice n Builder -> CaperSplice n
yieldRuntime = yieldChunk . RuntimeHtml
{-# INLINE yieldRuntime #-}


------------------------------------------------------------------------------
yieldRuntimeText :: Monad n => RuntimeSplice n Text -> CaperSplice n
yieldRuntimeText = yieldChunk . RuntimeHtml .
                   liftM (fromByteString . T.encodeUtf8)
{-# INLINE yieldRuntimeText #-}


------------------------------------------------------------------------------
yieldLater :: (Monad n) => n Builder -> CaperSplice n
yieldLater = yieldRuntime . RuntimeSplice . lift
{-# INLINE yieldLater #-}


------------------------------------------------------------------------------
yieldPromise :: (Monad n) => Promise Builder -> CaperSplice n
yieldPromise p = yieldRuntime $ getPromise p
{-# INLINE yieldPromise #-}


------------------------------------------------------------------------------
lookupCaperSplice :: Text -> CaperT n (Maybe (CaperSplice n))
lookupCaperSplice nm = getsTS (H.lookup nm . _caperSpliceMap)


------------------------------------------------------------------------------
runNode :: X.Node -> CaperSplice n
runNode node = localParamNode (const node) $ do
    isStatic <- subtreeIsStatic node
    if isStatic
      then yield $! X.renderHtmlFragment X.UTF8 [node]
      else compileNode node


------------------------------------------------------------------------------
subtreeIsStatic :: X.Node -> CaperT n Bool
subtreeIsStatic (X.Element nm attrs ch) = do
    isNodeDynamic <- liftM isJust $ lookupCaperSplice nm
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
compileNode :: X.Node -> CaperSplice n
compileNode (X.Element nm attrs ch) =
    -- Is this node a splice, or does it merely contain splices?
    lookupCaperSplice nm >>= fromMaybe compileStaticElement
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
parseAtt :: (Text, Text) -> CaperT n (DList (Chunk n))
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
getAttributeSplice :: Text -> CaperT n (DList (Chunk n))
getAttributeSplice name =
    lookupCaperSplice name >>= fromMaybe (return DL.empty)
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
newEmptyPromise :: CaperT n (Promise a)
newEmptyPromise = do
    keygen <- getsTS _keygen
    key    <- liftIO $ HE.makeKey keygen
    return $! Promise key
{-# INLINE newEmptyPromise #-}


------------------------------------------------------------------------------
newEmptyPromiseWithError :: (Monad n)
                         => String -> CaperT n (Promise a)
newEmptyPromiseWithError from = do
    keygen <- getsTS _keygen
    prom   <- liftM Promise $ liftIO $ HE.makeKey keygen

    yieldRuntimeSplice $ putPromise prom
                       $ error
                       $ "deferenced empty promise created at" ++ from

    return prom
{-# INLINE newEmptyPromiseWithError #-}


------------------------------------------------------------------------------
promise :: (Monad n) => n a -> CaperT n (Promise a)
promise act = runtimeSplicePromise (lift act)
{-# INLINE promise #-}


------------------------------------------------------------------------------
runtimeSplicePromise :: (Monad n)
                     => RuntimeSplice n a
                     -> CaperT n (Promise a)
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
            -> CaperT n (Promise b)
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
-- that fit in the promise and returns a CaperSplice that executes the promise
-- function for each item and concatenates the results.
mapPromises :: Monad n
            => (Promise a -> HeistT n IO (RuntimeSplice n Builder))
            -> n [a] -> CaperSplice n
mapPromises f getList = do
    singlePromise <- newEmptyPromise
    runSingle <- f singlePromise
    yieldRuntime $ do
        list <- lift getList
        htmls <- forM list $ \item ->
            putPromise singlePromise item >> runSingle
        return $ mconcat htmls


------------------------------------------------------------------------------
bindCaperSplice :: Text             -- ^ tag name
                -> CaperSplice n  -- ^ splice action
                -> HeistState n m   -- ^ source state
                -> HeistState n m
bindCaperSplice n v ts =
    ts { _caperSpliceMap = H.insert n v (_caperSpliceMap ts) }


------------------------------------------------------------------------------
bindCaperSplices :: [(Text, CaperSplice n)]  -- ^ splices to bind
                 -> HeistState n m             -- ^ source state
                 -> HeistState n m
bindCaperSplices ss ts = foldr (uncurry bindCaperSplice) ts ss


addCaperSplices :: Monad m => [(Text, CaperSplice n)] -> HeistT n m ()
addCaperSplices ss = modifyTS (bindCaperSplices ss)


------------------------------------------------------------------------------
-- | Converts 'Text' to a splice yielding the text, html-encoded.
textSplice :: Text -> CaperSplice n
textSplice = yieldText


------------------------------------------------------------------------------
--runChildrenCaper :: CaperSplice n
runChildrenCaper :: Monad m => HeistT m IO (RuntimeSplice m Builder)
runChildrenCaper = do
    res <- runNodeList . X.childNodes =<< getParamNode
    return $ codeGen res


------------------------------------------------------------------------------
-- | Binds a list of splices before using the children of the spliced node as
-- a view.
--runChildrenWithCaper ::
--       [(Text, CaperSplice n)]
--    -- ^ List of splices to bind before running the param nodes.
--    -> CaperSplice n
--    -- ^ Returns the passed in view.
runChildrenWithCaper splices = localTS (bindCaperSplices splices) runChildrenCaper


-- ------------------------------------------------------------------------------
-- -- | Wrapper around runChildrenWithCaper that applies a transformation function to
-- -- the second item in each of the tuples before calling runChildrenWithCaper.
-- runChildrenWithTransCaper :: (b -> CaperSplice n)
--                           -- ^ Splice generating function
--                           -> [(Text, b)]
--                           -- ^ List of tuples to be bound
--                           -> CaperSplice n
-- runChildrenWithTransCaper f = runChildrenWithCaper . map (second f)
-- 
-- 
-- ------------------------------------------------------------------------------
-- runChildrenWithTextCaper :: [(Text, Text)]
--                          -- ^ List of tuples to be bound
--                          -> CaperSplice n
-- runChildrenWithTextCaper = runChildrenWithTransCaper textSplice


-- ------------------------------------------------------------------------------
-- {-
-- 
-- Example:
-- 
-- <blog:listRecentPosts>
--   <a href="${post:href}"><post:title/></a>
-- </blog:listRecentPosts>
-- 
-- getRecentPosts :: Int -> Snap [ PostInfo ]
-- getRecentPosts = undefined
-- 
-- 
-- ------------------------------------------------------------------------------
-- foo :: HeistT Snap ()
-- foo = do
--     postListPromise <- promise (getRecentPosts 10)
--     postPromise     <- newEmptyPromise
-- 
--     childTemplate <- localTS $ do
--                          bindSplices [ ("post:title", titleWith postPromise)
--                                      , ("post:href" , hrefWith  postPromise)
--                                      ]
-- 
--                      runChildren
-- 
--     let xxxx = withPromise postListPromise $ \postList -> do
--                    htmls <- mapM (\post -> putPromise postPromise post >> childTemplate) postList
--                    return $! mconcat htmls
-- 
--     yieldLater xxxx
-- 
--   where
--     titleWith p = yieldLater $ withPromise p (return . postTitle)
-- 
-- -}

