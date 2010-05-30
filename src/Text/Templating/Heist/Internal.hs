{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Templating.Heist.Internal where

------------------------------------------------------------------------------
import           Control.Exception (SomeException)
import           Control.Monad.CatchIO
import           Control.Monad.RWS.Strict
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Either
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Typeable
import           Prelude hiding (catch)
import           System.Directory.Tree hiding (name)
import           Text.XML.Expat.Format
import qualified Text.XML.Expat.Tree as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Constants

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Heist templates are XML documents. The hexpat library is polymorphic over
-- the type of strings, so here we define a 'Node' alias to fix the string
-- types of the tag names and tag bodies to 'ByteString'.
type Node = X.Node ByteString ByteString


------------------------------------------------------------------------------
-- | A 'Template' is a forest of XML nodes.  Here we deviate from the "single
-- root node" constraint of well-formed XML because we want to allow templates
-- to contain fragments of a document that may not have a single root.
type Template = [Node]


------------------------------------------------------------------------------
-- | An 'InternalTemplate' carries a doctype with it that we get from the
-- template at load time.  The tricks that we're playing so templates don't
-- have to have a single root node screw up doctypes, so we have to handle
-- them manually.
data InternalTemplate = InternalTemplate {
    _itDoctype :: Maybe ByteString,
    _itNodes   :: [Node]
} deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
-- currently being processed.
type TPath = [ByteString]


------------------------------------------------------------------------------
-- | All templates are stored in a map.
type TemplateMap = Map TPath InternalTemplate


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing.  You will
-- build a @TemplateState@ using any of Heist's @TemplateState m ->
-- TemplateState m@ \"filter\" functions.  Then you use the resulting
-- @TemplateState@ in calls to @renderTemplate@.
data TemplateState m = TemplateState {
    -- | A mapping of splice names to splice actions
      _spliceMap      :: SpliceMap m
    -- | A mapping of template names to templates
    , _templateMap    :: TemplateMap
    -- | A flag to control splice recursion
    , _recurse        :: Bool
    -- | The path to the template currently being processed.
    , _curContext     :: TPath
    -- | A counter keeping track of the current recursion depth to prevent
    -- infinite loops.
    , _recursionDepth :: Int
    -- | A hook run on all templates at load time.
    , _onLoadHook     :: Template -> IO Template
    -- | A hook run on all templates just before they are rendered.
    , _preRunHook     :: Template -> m Template
    -- | A hook run on all templates just after they are rendered.
    , _postRunHook    :: Template -> m Template
    -- | The doctypes encountered during template processing.
    , _doctypes       :: [ByteString]
}


------------------------------------------------------------------------------
instance Eq (TemplateState m) where
    a == b = (_recurse a == _recurse b) &&
             (_templateMap a == _templateMap b) &&
             (_curContext a == _curContext b)


------------------------------------------------------------------------------
-- | 'TemplateMonad' is a monad transformer that gives you access to the 'Node'
--   being processed (using the 'MonadReader' instance) as well as holding the
--   'TemplateState' that contains splice and template mappings (accessible
--   using the 'MonadState' instance.
newtype TemplateMonad m a = TemplateMonad (RWST Node () (TemplateState m) m a)
  deriving ( Monad
           , MonadIO
           , MonadCatchIO
           , MonadReader Node
           , MonadState (TemplateState m) )


------------------------------------------------------------------------------
instance (Monad m) => Monoid (TemplateState m) where
    mempty = TemplateState Map.empty Map.empty True [] 0
                           return return return []

    (TemplateState s1 t1 r1 _ d1 o1 b1 a1 dt1) `mappend`
        (TemplateState s2 t2 r2 c2 d2 o2 b2 a2 dt2) =
        TemplateState s t r c2 d (o1 >=> o2) (b1 >=> b2) (a1 >=> a2)
            (dt1 `mappend` dt2)
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2
        r = r1 && r2
        d = max d1 d2


------------------------------------------------------------------------------
-- | Runs a splice in the underlying monad.  Splices require two
-- parameters, the template state, and an input node.
runTemplateMonad :: Monad m =>
             TemplateState m   -- ^ The initial template state
          -> Node              -- ^ The splice's input node
          -> TemplateMonad m a
          -> m a
runTemplateMonad ts node (TemplateMonad tm) = do
    (result,_,_) <- runRWST tm node ts
    return result


------------------------------------------------------------------------------
-- | Restores the components of TemplateState that can get modified in
-- template calls.  You should use this function instead of @put@ to restore
-- an old state.  Thas was needed because doctypes needs to be in a "global
-- scope" as opposed to the template call "local scope" of state items such
-- as recursionDepth, curContext, and spliceMap.
restoreState :: Monad m => TemplateState m -> TemplateMonad m ()
restoreState ts1 = 
    modify (\ts2 -> ts2
        { _recursionDepth = _recursionDepth ts1
        , _curContext = _curContext ts1
        , _spliceMap = _spliceMap ts1
        })


------------------------------------------------------------------------------
-- | Mappends a doctype to the state.
addDoctype :: Monad m => [ByteString] -> TemplateMonad m ()
addDoctype dt = do
    modify (\s -> s { _doctypes = _doctypes s `mappend` dt })


------------------------------------------------------------------------------
instance MonadTrans TemplateMonad where
  lift = TemplateMonad . lift


------------------------------------------------------------------------------
instance (Typeable1 m, Typeable a) => Typeable (TemplateMonad m a) where
    typeOf _ = mkTyConApp tCon [mRep, aRep]
      where
        tCon = mkTyCon "TemplateMonad"
        maRep = typeOf (undefined :: m a)
        (mCon, [aRep]) = splitTyConApp maRep
        mRep = mkTyConApp mCon []


------------------------------------------------------------------------------
-- | A Splice is a TemplateMonad computation that returns a 'Template'.
type Splice m = TemplateMonad m Template


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map ByteString (Splice m)


------------------------------------------------------------------------------
-- TemplateState functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Adds an on-load hook to a `TemplateState`.
addOnLoadHook :: (Monad m) =>
                 (Template -> IO Template)
              -> TemplateState m
              -> TemplateState m
addOnLoadHook hook ts = ts { _onLoadHook = _onLoadHook ts >=> hook }


------------------------------------------------------------------------------
-- | Adds a pre-run hook to a `TemplateState`.
addPreRunHook :: (Monad m) =>
                 (Template -> m Template)
              -> TemplateState m
              -> TemplateState m
addPreRunHook hook ts = ts { _preRunHook = _preRunHook ts >=> hook }


------------------------------------------------------------------------------
-- | Adds a post-run hook to a `TemplateState`.
addPostRunHook :: (Monad m) =>
                  (Template -> m Template)
               -> TemplateState m
               -> TemplateState m
addPostRunHook hook ts = ts { _postRunHook = _postRunHook ts >=> hook }


------------------------------------------------------------------------------
-- | Bind a new splice declaration to a tag name within a 'TemplateState'.
bindSplice :: Monad m =>
              ByteString        -- ^ tag name
           -> Splice m          -- ^ splice action
           -> TemplateState m   -- ^ source state
           -> TemplateState m
bindSplice n v ts = ts {_spliceMap = Map.insert n v (_spliceMap ts)}


------------------------------------------------------------------------------
-- | Convenience function for looking up a splice.
lookupSplice :: Monad m =>
                ByteString
             -> TemplateState m
             -> Maybe (Splice m)
lookupSplice nm ts = Map.lookup nm $ _spliceMap ts


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order.  If the
-- path is absolute, we need to remove the leading slash so the split doesn't
-- leave @\"\"@ as the last element of the TPath.
--
-- FIXME @\"..\"@ currently doesn't work in paths, the solution is non-trivial
splitPaths :: ByteString -> TPath
splitPaths p = if B.null p then [] else (reverse $ B.split '/' path)
  where
    path = if B.head p == '/' then B.tail p else p


------------------------------------------------------------------------------
-- | Does a single template lookup without cascading up.
singleLookup :: TemplateMap
             -> TPath
             -> ByteString
             -> Maybe (InternalTemplate, TPath)
singleLookup tm path name = fmap (\a -> (a,path)) $ Map.lookup (name:path) tm


------------------------------------------------------------------------------
-- | Searches for a template by looking in the full path then backing up into each
-- of the parent directories until the template is found.
traversePath :: TemplateMap
             -> TPath
             -> ByteString
             -> Maybe (InternalTemplate, TPath)
traversePath tm [] name = fmap (\a -> (a,[])) (Map.lookup [name] tm)
traversePath tm path name =
    singleLookup tm path name `mplus`
    traversePath tm (tail path) name


------------------------------------------------------------------------------
-- | Convenience function for looking up a template.
lookupTemplate :: Monad m =>
                  ByteString
               -> TemplateState m
               -> Maybe (InternalTemplate, TPath)
lookupTemplate nameStr ts = 
    f (_templateMap ts) path name
  where (name:p) = case splitPaths nameStr of
                       [] -> [""]
                       ps -> ps
        path = p ++ (_curContext ts)
        f = if '/' `B.elem` nameStr
                then singleLookup
                else traversePath


------------------------------------------------------------------------------
-- | Sets the templateMap in a TemplateState.
setTemplates :: Monad m => TemplateMap -> TemplateState m -> TemplateState m
setTemplates m ts = ts { _templateMap = m }


------------------------------------------------------------------------------
-- | Adds a template to the template state.
insertTemplate :: Monad m =>
               TPath
            -> InternalTemplate
            -> TemplateState m
            -> TemplateState m
insertTemplate p t st =
    setTemplates (Map.insert p t (_templateMap st)) st


------------------------------------------------------------------------------
-- | Adds a template to the template state.
addTemplate :: Monad m =>
               ByteString
            -> InternalTemplate
            -> TemplateState m
            -> TemplateState m
addTemplate n t st = insertTemplate (splitPaths n) t st


------------------------------------------------------------------------------
-- | Gets the node currently being processed.
--
--   > <speech author="Shakespeare">
--   >   To sleep, perchance to dream.
--   > </speech>
--
-- When you call @getParamNode@ inside the code for the @speech@ splice, it
-- returns the Node for the @speech@ tag and its children.  @getParamNode >>=
-- getChildren@ returns a list containing one 'Text' node containing part of
-- Hamlet's speech.  @getParamNode >>= getAttribute \"author\"@ would return
-- @Just "Shakespeare"@.
getParamNode :: Monad m => TemplateMonad m Node
getParamNode = ask


------------------------------------------------------------------------------
-- | Stops the recursive processing of splices.  Consider the following
-- example:
--
--   > <foo>
--   >   <bar>
--   >     ...
--   >   </bar>
--   > </foo>
--
-- Assume that @\"foo\"@ is bound to a splice procedure. Running the @foo@
-- splice will result in a list of nodes @L@.  Normally @foo@ will recursively
-- scan @L@ for splices and run them.  If @foo@ calls @stopRecursion@, @L@
-- will be included in the output verbatim without running any splices.
stopRecursion :: Monad m => TemplateMonad m ()
stopRecursion = modify (\st -> st { _recurse = False })


------------------------------------------------------------------------------
-- | Sets the current context
setContext :: Monad m => TPath -> TemplateMonad m ()
setContext c = modify (\st -> st { _curContext = c })


------------------------------------------------------------------------------
-- | Gets the current context
getContext :: Monad m => TemplateMonad m TPath
getContext = gets _curContext
  

------------------------------------------------------------------------------
-- | Performs splice processing on a single node.
runNode :: Monad m => Node -> Splice m
runNode n@(X.Text _)          = return [n]
runNode n@(X.Element nm _ ch) = do
    s <- liftM (lookupSplice nm) get
    maybe runChildren (recurseSplice n) s

  where
    runChildren = do
        newKids <- runNodeList ch
        return [X.modifyChildren (const newKids) n]


------------------------------------------------------------------------------
-- | Performs splice processing on a list of nodes.
runNodeList :: Monad m => [Node] -> Splice m
runNodeList nodes = liftM concat $ sequence (map runNode nodes)


------------------------------------------------------------------------------
-- | The maximum recursion depth.  (Used to prevent infinite loops.)
mAX_RECURSION_DEPTH :: Int
mAX_RECURSION_DEPTH = 50


------------------------------------------------------------------------------
-- | Checks the recursion flag and recurses accordingly.  Does not recurse
-- deeper than mAX_RECURSION_DEPTH to avoid infinite loops.
recurseSplice :: Monad m => Node -> Splice m -> Splice m
recurseSplice node splice = do
    result <- local (const node) splice
    ts' <- get
    if _recurse ts' && _recursionDepth ts' < mAX_RECURSION_DEPTH
        then do modRecursionDepth (+1)
                res <- runNodeList result
                restoreState ts'
                return res
        else return result
  where
    modRecursionDepth :: Monad m => (Int -> Int) -> TemplateMonad m ()
    modRecursionDepth f =
        modify (\st -> st { _recursionDepth = f (_recursionDepth st) })


------------------------------------------------------------------------------
-- | Looks up a template name runs a TemplateMonad computation on it.
lookupAndRun :: Monad m
             => ByteString
             -> ((InternalTemplate, TPath) -> TemplateMonad m (Maybe a))
             -> TemplateMonad m (Maybe a)
lookupAndRun name k = do
    ts <- get
    maybe (return Nothing) k
          (lookupTemplate name ts)


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.
evalTemplate :: Monad m
            => ByteString
            -> TemplateMonad m (Maybe Template)
evalTemplate name = lookupAndRun name
    (\(t,ctx) -> do
        ts <- get
        put (ts {_curContext = ctx})
        res <- runNodeList $ _itNodes t
        restoreState ts
        return $ Just res)


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.  This also
-- executes pre- and post-run hooks and adds the doctype.
evalWithHooks :: Monad m
            => ByteString
            -> TemplateMonad m (Maybe Template)
evalWithHooks name = lookupAndRun name
    (\(t,ctx) -> do
        addDoctype $ maybeToList $ _itDoctype t
        ts <- get
        nodes <- lift $ _preRunHook ts $ _itNodes t
        put (ts {_curContext = ctx})
        res <- runNodeList nodes
        restoreState ts
        return . Just =<< lift (_postRunHook ts res))


------------------------------------------------------------------------------
-- | Binds a list of constant string splices
bindStrings :: Monad m
            => [(ByteString, ByteString)]
            -> TemplateState m
            -> TemplateState m
bindStrings pairs ts = foldr add ts pairs
  where
    add (n,v) = bindSplice n (return [X.Text v])


------------------------------------------------------------------------------
-- | Renders a template with the specified parameters.  This is the function
-- to use when you want to "call" a template and pass in parameters from code.
callTemplate :: Monad m
             => ByteString                 -- ^ The name of the template
             -> [(ByteString, ByteString)] -- ^ Association list of
                                           -- (name,value) parameter pairs
             -> TemplateMonad m (Maybe Template)
callTemplate name params = do
    modify $ bindStrings params
    evalTemplate name


------------------------------------------------------------------------------
-- | Converts a Template to an InternalTemplate.  This can only be done inside
-- TemplateMonad where the doctype is available.
toInternalTemplate :: Monad m => Template -> TemplateMonad m InternalTemplate
toInternalTemplate t = do
    dts <- gets _doctypes
    return $ InternalTemplate {
        _itDoctype = listToMaybe dts,
        _itNodes = t
    }


------------------------------------------------------------------------------
-- | Renders an internal template by prepending the appropriate doctype.
renderInternal :: Monad m => InternalTemplate -> TemplateMonad m ByteString
renderInternal (InternalTemplate dt nodes) =
    return $ maybe bs (flip B.append bs) dt
  where
    bs = formatList' nodes


------------------------------------------------------------------------------
-- | Renders a template from the specified TemplateState.
renderTemplate :: Monad m
               => TemplateState m
               -> ByteString
               -> m (Maybe ByteString)
renderTemplate ts name = do
    runTemplateMonad ts (X.Text "") $ do
        mt <- evalWithHooks name
        maybe (return Nothing)
              (\t -> liftM Just $ renderInternal =<< toInternalTemplate t)
              mt
        

------------------------------------------------------------------------------
-- Template loading
------------------------------------------------------------------------------

-- | Reads an XML document from disk.
getDoc :: String -> IO (Either String InternalTemplate)
getDoc f = do
    bs <- catch (liftM Right $ B.readFile f)
                (\(e::SomeException) -> return $ Left $ show e)
    return $ do
        (doctype, rest) <- liftM extractDoctype bs
        let wrap b = "<snap:root>\n" `B.append` b `B.append` "\n</snap:root>"
            toTemplate t = InternalTemplate {
                _itDoctype = doctype,
                _itNodes = t
            }
        mapRight (toTemplate . X.getChildren) .
            mapLeft genErrorMsg .
            X.parse' heistExpatOptions . wrap $ rest
  where
    genErrorMsg (X.XMLParseError str loc) = f ++ " " ++ locMsg loc ++ ": " ++ translate str
    locMsg (X.XMLParseLocation line col _ _) =
        "(line " ++ show (line-1) ++ ", col " ++ show col ++ ")"
    translate "junk after document element" = "document must have a single root element"
    translate s = s


------------------------------------------------------------------------------
-- | Checks whether the bytestring has a doctype.
hasDoctype :: ByteString -> Bool
hasDoctype bs = "<!DOCTYPE" `B.isPrefixOf` bs


------------------------------------------------------------------------------
-- | Converts a ByteString into a tuple containing a possible doctype
-- ByteString and the rest of the document.
extractDoctype :: ByteString -> (Maybe ByteString, ByteString)
extractDoctype bs = 
    if hasDoctype bs
        then (Just $ B.snoc (B.takeWhile p bs) '>', B.tail $ B.dropWhile p bs)
        else (Nothing, bs)
  where
    p = (/='>')

------------------------------------------------------------------------------
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft g = either (Left . g) Right
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight g = either Left (Right . g)


------------------------------------------------------------------------------
-- | Loads a template with the specified path and filename.  The
-- template is only loaded if it has a ".tpl" extension.
loadTemplate :: String -- ^ path of the template root
             -> String -- ^ full file path (includes the template root)
             -> IO [Either String (TPath, InternalTemplate)] --TemplateMap
loadTemplate templateRoot fname
    | ".tpl" `isSuffixOf` fname = do
        c <- getDoc fname
        return [fmap (\t -> (splitPaths $ B.pack tName, t)) c]
    | otherwise = return []
  where -- tName is path relative to the template root directory
        correction = if last templateRoot == '/' then 0 else 1
        tName = drop ((length templateRoot)+correction) $
                -- We're only dropping the template root, not the whole path
                take ((length fname) - 4) fname


------------------------------------------------------------------------------
-- | Traverses the specified directory structure and builds a
-- TemplateState by loading all the files with a ".tpl" extension.
loadTemplates :: Monad m => FilePath -> TemplateState m -> IO (Either String (TemplateState m))
loadTemplates dir ts = do
    d <- readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
    case errs of
        [] -> liftM Right $ foldM loadHook ts $ rights tlist
        _  -> return $ Left $ unlines errs


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
runHookInternal :: Monad m => (Template -> m Template)
                -> InternalTemplate
                -> m InternalTemplate
runHookInternal f t = do
    n <- f $ _itNodes t
    return $ t { _itNodes = n }


------------------------------------------------------------------------------
-- | Runs the onLoad hook on the template and returns the `TemplateState`
-- with the result inserted.
loadHook :: Monad m => TemplateState m -> (TPath, InternalTemplate) -> IO (TemplateState m)
loadHook ts (tp, t) = do
    t' <- runHookInternal (_onLoadHook ts) t
    return $ insertTemplate tp t' ts


------------------------------------------------------------------------------
-- These are here until we can get them into hexpat.
------------------------------------------------------------------------------

formatList :: (X.GenericXMLString tag, X.GenericXMLString text) =>
              [X.Node tag text]
           -> L.ByteString
formatList nodes = foldl L.append L.empty $ map formatNode nodes

formatList' :: (X.GenericXMLString tag, X.GenericXMLString text) =>
               [X.Node tag text]
            -> B.ByteString
formatList' = B.concat . L.toChunks . formatList

