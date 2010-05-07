{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

{-|

  This module contains the core definitions for the Heist template system.

  FIXME: this intro could be a lot better

  The Heist template system is based on XML\/xhtml; templates are parsed in as
  XML trees, and we define substitutions (or \"splices\") on certain tag names
  that take 'Node' values in and substitute them with replacement text.

  In Heist nomenclature a \"splice\" is a program that, given a 'Node' from a
  template's XML tree, transforms it and gives you a result to be \"spliced\"
  back into the document. Each tag name in a template XML document is looked up
  in a \"splice table\", held within a 'TemplateState', and if there's a match
  the tag and its contents are passed to the 'Splice' for processing.

  In the following example, we'll define a substitution for @\<foo/\>@ nodes that
  causes the node to be replaced by the text \"Testing 1-2-3\":

  >
  > import Text.XML.Expat.Tree
  >
  > fooSplice :: Monad m => Splice m
  > fooSplice = return $ Text "Testing 1-2-3"
  >
  > go :: Monad m => m [Node]
  > go = runTemplate st template
  >   where
  >     st = bindSplice "foo" fooSplice emptyTemplateState 
  >     template = Element "root" [] [Element "foo" [] []]
  >

  Running \"go\" will result in the tree

  > Element "root" [] [Text "Testing 1-2-3"]

  'Splice' is a type synonym:

  >
  > type Splice m = TemplateMonad m [Node]
  >

  where 'TemplateMonad' is a monad transformer that gives you access to the
  'Node' being processed (it's a \"Reader\" monad) as well as holding the
  'TemplateState' that contains splice and template mappings.

  TODO:

  > * describe template loading and mapping
  >
  > * template contexts and subtrees
  >
  > * describe recursion / substitution process
-}

module Text.Templating.Heist
  (
    -- * Types
    Node
  , Splice
  , Template
  , TemplateMonad

  -- ** FIXME: don't export state fields
  , TemplateState(..)

    -- * Functions and declarations on TemplateState values
  , addTemplate
  , emptyTemplateState
  , bindSplice
  , lookupSplice
  , lookupTemplate

    -- * TemplateMonad functions
  , stopRecursion
  , getParamNode
  , runNodeList
  , getContext

    -- * Functions for running splices and templates
  , runSplice
  , runTemplate
  , runBareTemplate
  , getDoc
  , loadTemplates
  , renderTemplate
  , renderTemplate'

  , heistExpatOptions
  , module Text.Templating.Heist.Constants
  ) where

------------------------------------------------------------------------------
import           Control.Monad.RWS.Strict
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Data.Either
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
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
      _spliceMap   :: SpliceMap m
    -- | A mapping of template names to templates
    , _templateMap :: TemplateMap
    -- | A flag to control splice recursion
    , _recurse     :: Bool
    , _curContext  :: TPath
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
  deriving (Monad, MonadIO, MonadReader Node, MonadState (TemplateState m))


------------------------------------------------------------------------------
-- | A Splice is a TemplateMonad computation that returns [Node].
type Splice m = TemplateMonad m [Node]


------------------------------------------------------------------------------
-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map ByteString (Splice m)


------------------------------------------------------------------------------
instance Monoid (TemplateState m) where
    mempty = TemplateState Map.empty Map.empty True []

    (TemplateState s1 t1 r1 _) `mappend` (TemplateState s2 t2 r2 c2) =
        TemplateState s t r c2
      where
        s = s1 `mappend` s2
        t = t1 `mappend` t2
        r = r1 && r2

------------------------------------------------------------------------------
-- TemplateState functions
------------------------------------------------------------------------------

-- | An empty template state, with Heist's default splices (@\<bind\>@ and
-- @\<apply\>@) mapped.
emptyTemplateState :: Monad m => TemplateState m
emptyTemplateState = TemplateState defaultSpliceMap Map.empty True []


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
-- | Adds a template to the template state.
addTemplate :: Monad m =>
               ByteString
            -> Template
            -> TemplateState m
            -> TemplateState m
addTemplate n t st =
    st {_templateMap = Map.insert (splitPaths n) t (_templateMap st)}


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
-- | Sets the current context
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
-- | Checks the recursion flag and recurses accordingly.
recurseSplice :: Monad m => Node -> Splice m -> Splice m
recurseSplice node splice = do
    result <- local (const node) splice
    ts' <- get
    if _recurse ts'
        then runNodeList result
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
runTemplate :: Monad m => TemplateState m -> Template -> m [Node]
runTemplate ts template = runSplice ts (X.Text "") (runNodeList template)


------------------------------------------------------------------------------
-- | Looks up a template name in the supplied 'TemplateState' and runs
-- it in the underlying monad.
runTemplateByName :: Monad m =>
                     TemplateState m
                  -> ByteString
                  -> m (Maybe [Node])
runTemplateByName ts name = do
    let mt = lookupTemplate name ts
    maybe (return Nothing)
          (\(t,ctx) ->
              return . Just =<<
              runTemplate (ts {_curContext = ctx}) t)
          mt


------------------------------------------------------------------------------
-- | Runs a template with an empty TemplateState.
runBareTemplate :: Monad m => Template -> m [Node]
runBareTemplate = runTemplate emptyTemplateState

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
            put $ st { _spliceMap = defaultSpliceMap }
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
-- | Default name for the ignore splice.
childrenTag :: ByteString
childrenTag = "children"


------------------------------------------------------------------------------
-- | The ignore tag and everything it surrounds disappears in the
-- rendered output.
childrenImpl :: Monad m => Splice m
childrenImpl = return . X.getChildren =<< getParamNode


------------------------------------------------------------------------------
-- | The default set of built-in splices.
defaultSpliceMap :: Monad m => SpliceMap m
defaultSpliceMap = Map.fromList
    [(applyTag, applyImpl)
    ,(bindTag, bindImpl)
    ,(ignoreTag, ignoreImpl)
    ,(childrenTag, childrenImpl)
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
    bs <- catch (liftM Right $ B.readFile f) (\e -> return $ Left $ show e)
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
loadTemplates :: Monad m => FilePath -> IO (Either String (TemplateState m))
loadTemplates dir = do
    d <- readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
    return $ case errs of
        [] -> Right $ emptyTemplateState { _templateMap = Map.fromList $ rights tlist }
        _  -> Left $ unlines errs


------------------------------------------------------------------------------
-- | Renders a template from the specified TemplateState.
renderTemplate :: Monad m => TemplateState m -> ByteString -> m (Maybe ByteString)
renderTemplate ts name = do
    ns <- runTemplateByName ts name
    return $ (Just . formatList') =<< ns


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.
renderTemplate' :: FilePath -> ByteString -> IO (Maybe ByteString)
renderTemplate' baseDir name = do
    etm <- loadTemplates baseDir
    let ts = either (const emptyTemplateState) id etm
    ns <- runTemplateByName ts name
    return $ (Just . formatList') =<< ns

------------------------------------------------------------------------------
-- These are here until we can get them into hexpat.
------------------------------------------------------------------------------

formatList :: (X.GenericXMLString tag, X.GenericXMLString text) =>
              [X.Node tag text]
           -> L.ByteString
formatList nodes = foldl L.append (L.fromChunks [xmlHeader]) $
                       map formatNode nodes

formatList' :: (X.GenericXMLString tag, X.GenericXMLString text) =>
               [X.Node tag text]
            -> B.ByteString
formatList' = B.concat . L.toChunks . formatList

