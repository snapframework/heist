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
import           Prelude hiding (catch)
import           System.Directory.Tree hiding (name)
import           Text.XML.Expat.Format
import qualified Text.XML.Expat.Tree as X

------------------------------------------------------------------------------
import           Text.Templating.Heist.Constants

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | Heist templates are XML documents. The hexpat library is polymorphic over
-- the type of strings, so here we define a 'Node' alias to fix the string
-- types of the tag names and tag bodies to 'ByteString'.
type Node = X.Node ByteString ByteString


------------------------------------------------------------------------------
-- | A 'Template' is a forest of XML nodes.
type Template = [Node]


------------------------------------------------------------------------------
-- | Reversed list of directories
type TPath = [ByteString]


------------------------------------------------------------------------------
type TemplateMap = Map TPath Template


------------------------------------------------------------------------------
-- | Holds all the state information needed for template processing:
--
--     * a collection of named templates. If you use the @\<apply
--       template=\"foo\"\>@ tag to include another template by name, @\"foo\"@
--       is looked up in here.
--
--     * the mapping from tag names to 'Splice's.
--
--     * a flag to control whether we will recurse during splice processing.
--
-- We'll illustrate the recursion flag with a small example template:
--
--   > <foo>
--   >   <bar>
--   >     ...
--   >   </bar>
--   > </foo>
--
-- Assume that @\"foo\"@ is bound to a splice procedure. Running the @foo@
-- splice will result in a list of nodes @L@; if the recursion flag is on we
-- will recursively scan @L@ for splices, otherwise @L@ will be included in the
-- output verbatim.
data TemplateState m = TemplateState {
    -- | A mapping of splice names to splice actions
      _spliceMap      :: SpliceMap m
    -- | A mapping of template names to templates
    , _templateMap    :: TemplateMap
    -- | A flag to control splice recursion
    , _recurse        :: Bool
    , _curContext     :: TPath
    , _recursionDepth :: Int
    , _onLoadHook     :: Template -> IO Template
    , _preRunHook     :: Template -> m Template
    , _postRunHook    :: Template -> m Template
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
                           return return return

    (TemplateState s1 t1 r1 _ d1 o1 b1 a1) `mappend`
        (TemplateState s2 t2 r2 c2 d2 o2 b2 a2) =
        TemplateState s t r c2 d (o1 >=> o2) (b1 >=> b2) (a1 >=> a2)
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2
        r = r1 && r2
        d = max d1 d2


------------------------------------------------------------------------------
instance MonadTrans TemplateMonad where
  lift = TemplateMonad . lift

------------------------------------------------------------------------------
-- | A Splice is a TemplateMonad computation that returns [Node].
type Splice m = TemplateMonad m Template


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map ByteString (Splice m)


------------------------------------------------------------------------------
-- TemplateState functions
------------------------------------------------------------------------------

-- | An empty template state, with Heist's default splices (@\<bind\>@ and
-- @\<apply\>@) mapped.
emptyTemplateState :: Monad m => TemplateState m
emptyTemplateState = TemplateState defaultSpliceMap Map.empty True [] 0
                                   return return return


------------------------------------------------------------------------------
-- | Adds an onLoad hook to a `TemplateState`.
addOnLoadHook :: (Monad m) =>
                 (Template -> IO Template)
              -> TemplateState m
              -> TemplateState m
addOnLoadHook hook ts = ts { _onLoadHook = _onLoadHook ts >=> hook }


------------------------------------------------------------------------------
-- | Adds a preRun hook to a `TemplateState`.
addPreRunHook :: (Monad m) =>
                 (Template -> m Template)
              -> TemplateState m
              -> TemplateState m
addPreRunHook hook ts = ts { _preRunHook = _preRunHook ts >=> hook }


------------------------------------------------------------------------------
-- | Adds a postRun hook to a `TemplateState`.
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
-- | Converts a path into an array of the elements in reverse order.
splitPaths :: ByteString -> TPath
splitPaths = reverse . B.split '/'


------------------------------------------------------------------------------
-- | Does a single template lookup without cascading up.
singleLookup :: TemplateMap
             -> TPath
             -> ByteString
             -> Maybe (Template, TPath)
singleLookup tm path name = fmap (\a -> (a,path)) $ Map.lookup (name:path) tm


------------------------------------------------------------------------------
-- | Searches for a template by looking in the full path then backing up into each
-- of the parent directories until the template is found.
traversePath :: TemplateMap
             -> TPath
             -> ByteString
             -> Maybe (Template, TPath)
traversePath tm [] name = fmap (\a -> (a,[])) (Map.lookup [name] tm)
traversePath tm path name =
    singleLookup tm path name `mplus`
    traversePath tm (tail path) name


------------------------------------------------------------------------------
-- | Convenience function for looking up a template.
lookupTemplate :: Monad m =>
                  ByteString
               -> TemplateState m
               -> Maybe (Template, TPath)
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
            -> Template
            -> TemplateState m
            -> TemplateState m
insertTemplate p t st =
    setTemplates (Map.insert p t (_templateMap st)) st


------------------------------------------------------------------------------
-- | Adds a template to the template state.
addTemplate :: Monad m =>
               ByteString
            -> Template
            -> TemplateState m
            -> TemplateState m
addTemplate n t st = insertTemplate (splitPaths n) t st


------------------------------------------------------------------------------
-- | Gets the node currently being processed.
getParamNode :: Monad m => TemplateMonad m Node
getParamNode = ask


------------------------------------------------------------------------------
-- | Stops the recursive processing of splices.
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
-- | Performs splice processing on a list of nodes.
runNodeList :: Monad m => [Node] -> Splice m
runNodeList nodes = liftM concat $ sequence (map runNode nodes)


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
-- | The maximum recursion depth.  (Used to prevent infinite loops.)
mAX_RECURSION_DEPTH :: Int
mAX_RECURSION_DEPTH = 20


------------------------------------------------------------------------------
-- | Checks the recursion flag and recurses accordingly.
recurseSplice :: Monad m => Node -> Splice m -> Splice m
recurseSplice node splice = do
    result <- local (const node) splice
    ts' <- get
    if _recurse ts' && _recursionDepth ts' < mAX_RECURSION_DEPTH
        then do modify (\st -> st { _recursionDepth = _recursionDepth st + 1 })
                res <- runNodeList result
                put ts'
                return res
        else return result


------------------------------------------------------------------------------
-- | Runs a splice in the underlying monad.  Splices require two
-- parameters, the template state, and an input node.
runSplice :: Monad m =>
             TemplateState m -- ^ The initial template state
          -> Node            -- ^ The splice's input node
          -> Splice m        -- ^ The splice
          -> m [Node]
runSplice ts node (TemplateMonad splice) = do
    (result,_,_) <- runRWST splice node ts
    return result


------------------------------------------------------------------------------
-- | Runs a template in the underlying monad.  Similar to runSplice
-- except that templates don't require a Node as a parameter.
runRawTemplate :: Monad m => TemplateState m -> Template -> m [Node]
runRawTemplate ts template =
    _preRunHook ts template >>=
    runSplice ts (X.Text "") . runNodeList >>=
    _postRunHook ts


------------------------------------------------------------------------------
-- | Looks up a template name in the supplied 'TemplateState' and runs
-- it in the underlying monad.
runTemplate :: Monad m
            => TemplateState m
            -> ByteString
            -> m (Maybe [Node])
runTemplate ts name =
    maybe (return Nothing)
          (\(t,ctx) ->
              return . Just =<<
              runRawTemplate (ts {_curContext = ctx}) t)
          (lookupTemplate name ts)


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it.  Same as runTemplate except it
-- runs in TemplateMonad instead of m.
evalTemplate :: Monad m
            => ByteString
            -> TemplateMonad m (Maybe [Node])
evalTemplate name = do
    ts <- get
    lift $ runTemplate ts name


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
-- | Renders a template from the specified TemplateState.
renderTemplate :: Monad m
               => TemplateState m
               -> ByteString
               -> m (Maybe ByteString)
renderTemplate ts name = do
    ns <- runTemplate ts name
    return $ (Just . formatList') =<< ns


--callTemplate2 :: Monad m
--             => ByteString                 -- ^ The name of the template
--             -> [(ByteString, ByteString)] -- ^ Association list of
--                                           -- (name,value) parameter pairs
--             -> TemplateMonad m (Maybe Template)
--callTemplate2 name params = do
--    ts <- get
--    runTemplate (bindStrings params ts) name


------------------------------------------------------------------------------
-- Heist built-in splices implementing core template functionality.
------------------------------------------------------------------------------

-- The bind splice

-- | Default name for the bind splice.
bindTag :: ByteString
bindTag = "bind"


------------------------------------------------------------------------------
-- | Default attribute name for the bind tag.
bindAttr :: ByteString
bindAttr = "tag"


------------------------------------------------------------------------------
-- | Implementation of the bind splice.
bindImpl :: Monad m => Splice m
bindImpl = do
    node <- getParamNode
    maybe (return ())
          (add node)
          (X.getAttribute node bindAttr)
    return []

  where
    add node nm = modify $ bindSplice nm (return $ X.getChildren node)


------------------------------------------------------------------------------
-- The apply splice
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Default name for the apply splice.
applyTag :: ByteString
applyTag = "apply"


------------------------------------------------------------------------------
-- | Default attribute name for the apply tag.
applyAttr :: ByteString
applyAttr = "template"


------------------------------------------------------------------------------
-- | Implementation of the apply splice.
applyImpl :: Monad m => Splice m
applyImpl = do
    node <- getParamNode
    case X.getAttribute node applyAttr of
        Nothing   -> return [] -- TODO: error handling
        Just attr -> do 
            st <- get
            processedChildren <- runNodeList $ X.getChildren node
            modify (bindSplice "content" $ return processedChildren)
            maybe (return []) -- TODO: error handling
                  (\(t,ctx) -> do setContext ctx
                                  result <- runNodeList t
                                  put st
                                  return result)
                  (lookupTemplate attr (st {_curContext = nextCtx attr st}))
  where nextCtx name st
            | B.isPrefixOf "/" name = []
            | otherwise             = _curContext st


------------------------------------------------------------------------------
-- | Default name for the ignore splice.
ignoreTag :: ByteString
ignoreTag = "ignore"


------------------------------------------------------------------------------
-- | The ignore tag and everything it surrounds disappears in the
-- rendered output.
ignoreImpl :: Monad m => Splice m
ignoreImpl = return []


------------------------------------------------------------------------------
-- | The default set of built-in splices.
defaultSpliceMap :: Monad m => SpliceMap m
defaultSpliceMap = Map.fromList
    [(applyTag, applyImpl)
    ,(bindTag, bindImpl)
    ,(ignoreTag, ignoreImpl)
    ]


------------------------------------------------------------------------------
heistExpatOptions :: X.ParserOptions ByteString ByteString
heistExpatOptions =
    X.defaultParserOptions {
           X.parserEncoding = Just X.UTF8
         , X.entityDecoder  = Just (\k -> Map.lookup k htmlEntityLookupTable)
         }

------------------------------------------------------------------------------
-- Template loading
------------------------------------------------------------------------------

-- | Reads an XML document from disk.
getDoc :: String -> IO (Either String Template)
getDoc f = do
    bs <- catch (liftM Right $ B.readFile f)
                (\(e::SomeException) -> return $ Left $ show e)
    let wrap b = "<snap:root>\n" `B.append` b `B.append` "\n</snap:root>"
    return $ (mapRight X.getChildren .
              mapLeft genErrorMsg .
              X.parse' heistExpatOptions . wrap) =<< bs
  where
    genErrorMsg (X.XMLParseError str loc) = f ++ " " ++ locMsg loc ++ ": " ++ translate str
    locMsg (X.XMLParseLocation line col _ _) =
        "(line " ++ show (line-1) ++ ", col " ++ show col ++ ")"
    translate "junk after document element" = "document must have a single root element"
    translate s = s

------------------------------------------------------------------------------
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft g = either (Left . g) Right
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight g = either Left (Right . g)


------------------------------------------------------------------------------
-- | Loads a template with the specified path and filename.  The
-- template is only loaded if it has a ".tpl" extension.
loadTemplate :: String -> String -> IO [Either String (TPath, Template)] --TemplateMap
loadTemplate path fname
    | ".tpl" `isSuffixOf` fname = do
        c <- getDoc fname
        return [fmap (\t -> (splitPaths $ B.pack tName, t)) c]
    | otherwise = return []
  where tName = drop ((length path)+1) $
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
-- | Runs the onLoad hook on the template and returns the `TemplateState`
-- with the result inserted.
loadHook :: Monad m => TemplateState m -> (TPath, Template) -> IO (TemplateState m)
loadHook ts (tp, t) = do
    t' <- _onLoadHook ts t
    return $ insertTemplate tp t' ts


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.
renderTemplate' :: FilePath -> ByteString -> IO (Maybe ByteString)
renderTemplate' baseDir name = do
    etm <- loadTemplates baseDir emptyTemplateState
    let ts = either (const emptyTemplateState) id etm
    ns <- runTemplate ts name
    return $ (Just . formatList') =<< ns

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

{-

p :: ByteString -> Node
p t = X.Element "p" [] [X.Text t]

hookG :: Monad m => ByteString -> Template -> m Template
hookG str t = return $ (p str) : t

onLoad = hookG "Inserted on load"
preRun = hookG "Inserted on preRun"
postRun = hookG "Inserted on postRun"

ts :: IO (Either String (TemplateState IO))
ts = loadTemplates "test/templates" $
    foldr ($) emptyTemplateState
    [setOnLoadHook onLoad
    ,setPreRunHook preRun
    ,setPostRunHook postRun
    ]

r name etm = do
    let ts = either (error "Danger Will Robinson!") id etm
    ns <- runTemplate ts name
    return $ (Just . formatList') =<< ns


-}
