{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Heist.Compiled.Internal where


------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char.Utf8
import           Control.Arrow
import           Control.Monad
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.Text               as AP
import           Data.ByteString                    (ByteString)
import           Data.DList                         (DList)
import qualified Data.DList                         as DL
import qualified Data.HashMap.Strict                as H
import qualified Data.HashSet                       as S
import qualified Data.HeterogeneousEnvironment      as HE
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Vector                        as V
import           Text.Printf
import qualified Text.XmlHtml                       as X
import qualified Text.XmlHtml.HTML.Meta             as X
------------------------------------------------------------------------------
#if !MIN_VERSION_base(4,8,0)
import           Data.Foldable                      (Foldable)
#endif
import qualified Data.Foldable                      as Foldable
------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Internal.Types.HeistState
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | A compiled Splice is a HeistT computation that returns a @DList
-- (Chunk m)@.
--
-- The more interesting part of the type signature is what comes before the
-- return value.  The first type parameter in @'HeistT' n IO@ is the runtime
-- monad.  This reveals that the Chunks know about the runtime monad.  The
-- second type parameter in @HeistT n IO@ is @IO@.  This tells us that the
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


renderFragment :: Markup -> [X.Node] -> Builder
renderFragment markup ns =
    case markup of
      Html -> X.renderHtmlFragment X.UTF8 ns
      Xml  -> X.renderXmlFragment X.UTF8 ns


------------------------------------------------------------------------------
-- | Yields pure text known at load time.
pureTextChunk :: Text -> Chunk n
pureTextChunk t = Pure $ T.encodeUtf8 t
{-# INLINE pureTextChunk #-}


------------------------------------------------------------------------------
-- | Yields a pure Builder known at load time.  You should use this and
-- 'yieldPureText' as much as possible to maximize the parts of your page that
-- can be compiled to static ByteStrings.
yieldPure :: Builder -> DList (Chunk n)
yieldPure = DL.singleton . Pure . toByteString
{-# INLINE yieldPure #-}


------------------------------------------------------------------------------
-- | Yields a runtime action that returns a builder.
yieldRuntime :: RuntimeSplice n Builder -> DList (Chunk n)
yieldRuntime = DL.singleton . RuntimeHtml
{-# INLINE yieldRuntime #-}


------------------------------------------------------------------------------
-- | Yields a runtime action that returns no value and is only needed for its
-- side effect.
yieldRuntimeEffect :: Monad n => RuntimeSplice n () -> DList (Chunk n)
yieldRuntimeEffect = DL.singleton . RuntimeAction
{-# INLINE yieldRuntimeEffect #-}


------------------------------------------------------------------------------
-- | A convenience wrapper around yieldPure for working with Text.  Roughly
-- equivalent to 'textSplice' from Heist.Interpreted.
yieldPureText :: Text -> DList (Chunk n)
yieldPureText = DL.singleton . pureTextChunk
{-# INLINE yieldPureText #-}


------------------------------------------------------------------------------
-- | Convenience wrapper around yieldRuntime allowing you to work with Text.
yieldRuntimeText :: Monad n => RuntimeSplice n Text -> DList (Chunk n)
yieldRuntimeText = yieldRuntime .  liftM fromText
{-# INLINE yieldRuntimeText #-}


------------------------------------------------------------------------------
-- | Returns a computation that performs load-time splice processing on the
-- supplied list of nodes.
runNodeList :: Monad n => [X.Node] -> Splice n
runNodeList = mapSplices runNode


------------------------------------------------------------------------------
-- | Runs a DocumentFile with the appropriate template context set.
runDocumentFile :: Monad n
                => TPath
                -> DocumentFile
                -> Splice n
runDocumentFile tpath df = do
    let markup = case dfDoc df of
                   X.XmlDocument _ _ _ -> Xml
                   X.HtmlDocument _ _ _ -> Html
    modifyHS (\hs -> hs { _curMarkup = markup })
    let inDoctype = X.docType $ dfDoc df
    addDoctype $ maybeToList inDoctype
    modifyHS (setCurTemplateFile curPath .  setCurContext tpath)
    res <- runNodeList nodes
    dt <- getsHS (listToMaybe . _doctypes)
    let enc = X.docEncoding $ dfDoc df
    return $! (yieldPure (X.renderDocType enc dt) `mappend` res)
  where
    curPath     = dfFile df
    nodes       = X.docContent $! dfDoc df


------------------------------------------------------------------------------
compileTemplate
    :: Monad n
    => TPath
    -> DocumentFile
    -> HeistT n IO [Chunk n]
compileTemplate tpath df = do
    !chunks <- runDocumentFile tpath df
    return $! consolidate chunks


------------------------------------------------------------------------------
compileTemplates
    :: Monad n
    => (TPath -> Bool)
    -> HeistState n
    -> IO (Either [String] (HeistState n))
compileTemplates f hs = do
    (tmap, hs') <- runHeistT (compileTemplates' f) (X.TextNode "") hs
    let pre = _splicePrefix hs'
    let canError = _errorNotBound hs'
    let errs = _spliceErrors hs'
    let nsErr = if not (T.null pre) && (_numNamespacedTags hs' == 0)
                  then Left [noNamespaceSplicesMsg $ T.unpack pre]
                  else Right ()
    return $ if canError
               then case errs of
                     [] -> nsErr >>
                           (Right $! hs { _compiledTemplateMap = tmap })
                     es -> Left $ either (++) (const id) nsErr $
                           map (T.unpack . spliceErrorText) es
               else nsErr >> (Right $! hs { _compiledTemplateMap = tmap
                                          , _spliceErrors = errs
                                          })


------------------------------------------------------------------------------
noNamespaceSplicesMsg :: String -> String
noNamespaceSplicesMsg pre = unwords
    [ printf "You are using a namespace of '%s', but you don't have any" ns
    , printf "tags starting with '%s'.  If you have not defined any" pre
    , "splices, then change your namespace to the empty string to get rid"
    , "of this message."
    ]
  where
    ns = reverse $ drop 1 $ reverse pre


------------------------------------------------------------------------------
compileTemplates'
    :: Monad n
    => (TPath -> Bool)
    -> HeistT n IO (H.HashMap TPath ([Chunk n], MIMEType))
compileTemplates' f = do
    hs <- getHS
    let tpathDocfiles :: [(TPath, DocumentFile)]
        tpathDocfiles = filter (f . fst)
                            (H.toList $ _templateMap hs)
    foldM runOne H.empty tpathDocfiles
  where
    runOne tmap (tpath, df) = do
        modifyHS (\hs -> hs { _doctypes = []})
        !mHtml <- compileTemplate tpath df
        return $! H.insert tpath (mHtml, mimeType $! dfDoc df) tmap


------------------------------------------------------------------------------
-- | Consolidate consecutive Pure Chunks.
consolidate :: (Monad n) => DList (Chunk n) -> [Chunk n]
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
codeGen :: Monad n => DList (Chunk n) -> RuntimeSplice n Builder
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
lookupSplice nm = do
    pre <- getsHS _splicePrefix
    res <- getsHS (H.lookup nm . _compiledSpliceMap)
    if isNothing res && T.isPrefixOf pre nm && not (T.null pre)
      then do
          tellSpliceError $ "No splice bound for " `mappend` nm
          return Nothing
      else return res


------------------------------------------------------------------------------
-- | Runs a single node.  If there is no splice referenced anywhere in the
-- subtree, then it is rendered as a pure chunk, otherwise it calls
-- compileNode to generate the appropriate runtime computation.
runNode :: Monad n => X.Node -> Splice n
runNode node = localParamNode (const node) $ do
    hs <- getHS
    let pre = _splicePrefix hs
    let hasPrefix = (T.isPrefixOf pre `fmap` X.tagName node) == Just True
    when (not (T.null pre) && hasPrefix) incNamespacedTags
    isStatic <- subtreeIsStatic node
    markup <- getsHS _curMarkup
    if isStatic
      then return $! yieldPure $! renderFragment markup [parseAttrs node]
      else localHS (\hs' -> hs' {_splicePath =
                                 (_curContext hs', _curTemplateFile hs',
                                  X.elementTag node):(_splicePath hs')}) $
           compileNode node


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
-- | Given a 'X.Node' in the DOM tree, produces a \"runtime splice\" that will
-- generate html at runtime.
compileNode :: Monad n => X.Node -> Splice n
compileNode (X.Element nm attrs ch) = do
    msplice <- lookupSplice nm
    fromMaybe compileStaticElement msplice
  where
    tag0 = T.append "<" nm
    end = T.concat [ "</" , nm , ">"]
    -- If the tag is not a splice, but it contains dynamic children
    compileStaticElement = do
        -- Parse the attributes: we have Left for static and Right for runtime
        compiledAttrs <- runAttributes attrs

        childHtml <- runNodeList ch

        return $! if null (DL.toList childHtml) && nm `S.member` X.voidTags
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
-- |
parseAtt2 :: Monad n
          => (Text, Text)
          -> HeistT n IO (RuntimeSplice n [(Text, Text)])
parseAtt2 (k,v) = do
    mas <- getsHS (H.lookup k . _attrSpliceMap)
    maybe doInline (return . doAttrSplice) mas

  where
    cvt (Literal x) = return $ return x
    cvt (Ident x) =
        localParamNode (const $ X.Element x [] []) $ getAttributeSplice2 x

    -- Handles inline parsing of $() splice syntax in attributes
    doInline = do
        let ast = case AP.feed (AP.parse attParser v) "" of
                    (AP.Done _ res) -> res
                    (AP.Fail _ _ _) -> []
                    (AP.Partial _ ) -> []
        chunks <- mapM cvt ast
        return $ do
            list <- sequence chunks
            return [(k, T.concat list)]

    -- Handles attribute splices
    doAttrSplice splice = splice v


------------------------------------------------------------------------------
-- | Performs splice processing on a list of attributes.  This is useful in
-- situations where you need to stop recursion, but still run splice
-- processing on the node's attributes.
runAttributes :: Monad n
              => [(Text, Text)] -- ^ List of attributes
              -> HeistT n IO [DList (Chunk n)]
runAttributes = mapM parseAtt


------------------------------------------------------------------------------
-- | Performs splice processing on a list of attributes.  This is useful in
-- situations where you need to stop recursion, but still run splice
-- processing on the node's attributes.
runAttributesRaw :: Monad n
                 -- Note that this parameter should not be changed to Splices
                 => [(Text, Text)] -- ^ List of attributes
                 -> HeistT n IO (RuntimeSplice n [(Text, Text)])
runAttributesRaw attrs = do
    arrs <- mapM parseAtt2 attrs
    return $ liftM concat $ sequence arrs


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


getAttributeSplice2 :: Monad n => Text -> HeistT n IO (RuntimeSplice n Text)
getAttributeSplice2 name = do
    mSplice <- lookupSplice name
    case mSplice of
      Nothing -> return $ return $ T.concat ["${", name, "}"]
      Just splice -> do
        res <- splice
        return $ liftM (T.decodeUtf8 . toByteString) $ codeGen res
{-# INLINE getAttributeSplice2 #-}


------------------------------------------------------------------------------
-- | Promises are used for referencing the results of future runtime
-- computations during load time splice processing.
newtype Promise a = Promise (HE.Key a)


------------------------------------------------------------------------------
-- | Gets the result of a promised runtime computation.
getPromise :: (Monad n) => Promise a -> RuntimeSplice n a
getPromise (Promise k) = do
    mb <- gets (HE.lookup k)
    return $ fromMaybe e mb

  where
    e = error $ "getPromise: dereferenced empty key (id "
                ++ show (HE.getKeyId k) ++ ")"
{-# INLINE getPromise #-}


------------------------------------------------------------------------------
-- | Adds a promise to the runtime splice context.
putPromise :: (Monad n) => Promise a -> a -> RuntimeSplice n ()
putPromise (Promise k) x = modify (HE.insert k x)
{-# INLINE putPromise #-}


------------------------------------------------------------------------------
-- | Modifies a promise.
adjustPromise :: Monad n => Promise a -> (a -> a) -> RuntimeSplice n ()
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


------------------------------------------------------------------------------
-- | Binds a compiled splice.  This function should not be exported.
bindSplice :: Text             -- ^ tag name
           -> Splice n         -- ^ splice action
           -> HeistState n     -- ^ source state
           -> HeistState n
bindSplice n v ts =
    ts { _compiledSpliceMap = H.insert n' v (_compiledSpliceMap ts) }
  where
    n' = _splicePrefix ts `mappend` n

------------------------------------------------------------------------------
-- | Binds a list of compiled splices.  This function should not be exported.
bindSplices :: Splices (Splice n)  -- ^ splices to bind
            -> HeistState n        -- ^ source state
            -> HeistState n
bindSplices ss hs =
    hs { _compiledSpliceMap = applySpliceMap hs _compiledSpliceMap ss }


------------------------------------------------------------------------------
-- | Adds a list of compiled splices to the splice map.  This function is
-- useful because it allows compiled splices to bind other compiled splices
-- during load-time splice processing.
withLocalSplices :: Splices (Splice n)
                 -> Splices (AttrSplice n)
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
    runNodeList $ maybe (error err) (X.docContent . dfDoc . fst) $
      lookupTemplate nm hs _templateMap
  where
    err = "callTemplate: "++(T.unpack $ T.decodeUtf8 nm)++(" does not exist")


interpret :: Monad n => DList (Chunk n) -> n Builder
interpret = flip evalStateT HE.empty . unRT . codeGen


------------------------------------------------------------------------------
-- | Converts a pure text splice function to a pure Builder splice function.
textSplice :: (a -> Text) -> a -> Builder
textSplice f = fromText . f


                               ---------------
                               -- New Stuff --
                               ---------------


------------------------------------------------------------------------------
-- | This is the same as htmlNodeSplice.  
nodeSplice :: (a -> [X.Node]) -> a -> Builder
nodeSplice f = X.renderHtmlFragment X.UTF8 . f
{-# DEPRECATED nodeSplice
   "Use xmlNodeSplice or htmlNodeSplice, will be removed in Heist 1.1" #-}


------------------------------------------------------------------------------
-- | Converts a pure XML Node splice function to a pure Builder splice
-- function.
xmlNodeSplice :: (a -> [X.Node]) -> a -> Builder
xmlNodeSplice f = X.renderXmlFragment X.UTF8 . f


------------------------------------------------------------------------------
-- | Converts a pure HTML Node splice function to a pure Builder splice
-- function.
htmlNodeSplice :: (a -> [X.Node]) -> a -> Builder
htmlNodeSplice f = X.renderHtmlFragment X.UTF8 . f


------------------------------------------------------------------------------
-- | Converts a pure Builder splice function into a monadic splice function
-- of a RuntimeSplice.
pureSplice :: Monad n => (a -> Builder) -> RuntimeSplice n a -> Splice n
pureSplice f n = return $ yieldRuntime (return . f =<< n)


------------------------------------------------------------------------------
-- | Runs a splice, but first binds splices given by splice functions that
-- need some runtime data.
withSplices :: Monad n
            => Splice n
            -- ^ Splice to be run
            -> Splices (RuntimeSplice n a -> Splice n)
            -- ^ Splices to be bound first
            -> RuntimeSplice n a
            -- ^ Runtime data needed by the above splices
            -> Splice n
withSplices splice splices runtimeAction =
    withLocalSplices splices' mempty splice
  where
    splices' = mapV ($runtimeAction) splices


------------------------------------------------------------------------------
{-# INLINE foldMapM #-}
foldMapM :: (Monad f, Monoid m, Foldable list)
         => (a -> f m)
         -> list a
         -> f m
foldMapM f =
  Foldable.foldlM (\xs x -> xs `seq` liftM (xs <>) (f x)) mempty

------------------------------------------------------------------------------
-- | Like withSplices, but evaluates the splice repeatedly for each element in
-- a list generated at runtime.
manyWithSplices :: (Foldable f, Monad n)
                => Splice n
                -> Splices (RuntimeSplice n a -> Splice n)
                -> RuntimeSplice n (f a)
                -> Splice n
manyWithSplices splice splices runtimeAction =
    manyWith splice splices mempty runtimeAction


------------------------------------------------------------------------------
-- | More powerful version of manyWithSplices that lets you also define
-- attribute splices.
manyWith :: (Foldable f, Monad n)
         => Splice n
         -> Splices (RuntimeSplice n a -> Splice n)
         -> Splices (RuntimeSplice n a -> AttrSplice n)
         -> RuntimeSplice n (f a)
         -> Splice n
manyWith splice splices attrSplices runtimeAction = do
    p <- newEmptyPromise
    let splices' = mapV ($ getPromise p) splices
    let attrSplices' = mapV ($ getPromise p) attrSplices
    chunks <- withLocalSplices splices' attrSplices' splice
    return $ yieldRuntime $ do
        items <- runtimeAction
        foldMapM (\item -> putPromise p item >> codeGen chunks) items


------------------------------------------------------------------------------
-- | Similar to 'mapSplices' in interpreted mode.  Gets a runtime list of
-- items and applies a compiled runtime splice function to each element of the
-- list.
deferMany :: (Foldable f, Monad n)
          => (RuntimeSplice n a -> Splice n)
          -> RuntimeSplice n (f a)
          -> Splice n
deferMany f getItems = do
    promise <- newEmptyPromise
    chunks <- f $ getPromise promise
    return $ yieldRuntime $ do
        items <- getItems
        foldMapM (\item -> putPromise promise item >> codeGen chunks) items


------------------------------------------------------------------------------
-- | Saves the results of a runtme computation in a 'Promise' so they don't
-- get recalculated if used more than once.
deferMap :: Monad n
         => (a -> RuntimeSplice n b)
         -> (RuntimeSplice n b -> Splice n)
         -> RuntimeSplice n a -> Splice n
deferMap f pf n = do
    p2 <- newEmptyPromise
    let action = yieldRuntimeEffect $ putPromise p2 =<< f =<< n
    res <- pf $ getPromise p2
    return $ action `mappend` res


------------------------------------------------------------------------------
-- | Like deferMap, but only runs the result if a Maybe function of the
-- runtime value returns Just.  If it returns Nothing, then no output is
-- generated.
--
-- This is a good example of how to do more complex flow control with
-- promises.  The generalization of this abstraction is too complex to be
-- distilled to elegant high-level combinators.  If you need to implement your
-- own special flow control, then you should use functions from the
-- `Heist.Compiled.LowLevel` module similarly to how it is done in the
-- implementation of this function.
mayDeferMap :: Monad n
            => (a -> RuntimeSplice n (Maybe b))
            -> (RuntimeSplice n b -> Splice n)
            -> RuntimeSplice n a -> Splice n
mayDeferMap f pf n = do
    p2 <- newEmptyPromise
    action <- pf $ getPromise p2
    return $ yieldRuntime $ do
        mb <- f =<< n
        case mb of
          Nothing -> return mempty
          Just b -> do
            putPromise p2 b
            codeGen action


------------------------------------------------------------------------------
-- | Converts an RuntimeSplice into a Splice, given a helper function that
-- generates a Builder.
bindLater :: (Monad n)
          => (a -> RuntimeSplice n Builder)
          -> RuntimeSplice n a
          -> Splice n
bindLater f p = return $ yieldRuntime $ f =<< p


