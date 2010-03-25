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

    -- * Functions for running splices and templates
  , runSplice
  , runTemplate
  , runBareTemplate

    -- * Temporary (FIXME)
  , getDoc
  , loadTemplates
  , renderTemplate
  , tShow
--  , test
--  , evalFile

  , module Text.Templating.Heist.Constants
  ) where

import           Control.Monad.RWS.Strict

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid
import           System.Directory.Tree hiding (name)
import           System.FilePath

import qualified Text.XML.Expat.Tree as X

import Text.XML.Expat.Format
import Debug.Trace

import           Text.Templating.Heist.Constants
------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | Heist templates are XML documents. The hexpat library is polymorphic over
-- the type of strings, so here we define a 'Node' alias to fix the string
-- types of the tag names and tag bodies to 'ByteString'.
type Node = X.Node ByteString ByteString

-- | A 'Template' is a forest of XML nodes.
type Template = [Node]

-- |Reversed list of directories
type TPath = [ByteString]

type TemplateMap = Map TPath Template

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
--  , _curContext  :: TPath
}

instance Eq (TemplateState m) where
  a == b = (_recurse a == _recurse b) &&
           (_templateMap a == _templateMap b)

-- | 'TemplateMonad' is a monad transformer that gives you access to the 'Node'
--   being processed (using the 'MonadReader' instance) as well as holding the
--   'TemplateState' that contains splice and template mappings (accessible
--   using the 'MonadState' instance.
newtype TemplateMonad m a = TemplateMonad (RWST Node () (TemplateState m) m a)
  deriving (Monad, MonadIO, MonadReader Node, MonadState (TemplateState m))

-- | A Splice is a TemplateMonad computation that returns [Node].
type Splice m = TemplateMonad m [Node]

-- | SpliceMap associates a name and a Splice.
type SpliceMap m = Map ByteString (Splice m)

instance Monoid (TemplateState m) where
    mempty = TemplateState Map.empty Map.empty True -- [] []

    (TemplateState s1 t1 r1) `mappend` (TemplateState s2 t2 r2) =
        TemplateState s t r 
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
emptyTemplateState = TemplateState defaultSpliceMap Map.empty True

-- | Bind a new splice declaration to a tag name within a 'TemplateState'.
bindSplice :: Monad m =>
              ByteString        -- ^ tag name
           -> Splice m          -- ^ splice action
           -> TemplateState m   -- ^ source state
           -> TemplateState m
bindSplice n v ts = ts {_spliceMap = Map.insert n v (_spliceMap ts)}


-- |convenience function for looking up a splice.
lookupSplice :: Monad m => ByteString -> TemplateState m -> Maybe (Splice m)
lookupSplice nm ts = Map.lookup nm $ _spliceMap ts

-- |Converts a path into an array of the elements in reverse order.
splitPaths :: ByteString -> TPath
splitPaths = reverse . B.split '/'

{-|
  Searches for a template by looking in the full path then backing up into each
  of the parent directories until the template is found.
-}
traversePath :: TemplateMap -> TPath -> ByteString -> Maybe Template
traversePath tm [] name = Map.lookup [name] tm
traversePath tm path name =
  --trace ("traversePath "++(B.unpack name)++" "++(show path)++" returned "++(show $ Map.lookup (name:path) tm))
  (Map.lookup (name:path) tm) `mplus` traversePath tm (tail path) name

-- |Convenience function for looking up a template.
lookupTemplate :: Monad m => ByteString -> TemplateState m -> Maybe Template
lookupTemplate nameStr ts = traversePath (_templateMap ts) path name
  where (name:path) = splitPaths nameStr

-- |Adds a template to the template state.
addTemplate :: Monad m =>
               ByteString
            -> Template
            -> TemplateState m
            -> TemplateState m
addTemplate n t st = st {_templateMap = Map.insert (splitPaths n) t (_templateMap st)}

-- |Gets the node currently being processed.
getParamNode :: Monad m => TemplateMonad m Node
getParamNode = ask

-- |Stops the recursive processing of splices.
stopRecursion :: Monad m => TemplateMonad m ()
stopRecursion = modify (\st -> st { _recurse = False })

-- |Performs splice processing on a list of nodes.
runNodeList :: Monad m => [Node] -> Splice m
runNodeList nodes = liftM concat $ sequence (map runNode nodes)

-- |Performs splice processing on a single node.
runNode :: Monad m => Node -> Splice m
runNode n@(X.Text _)          = return [n]
runNode n@(X.Element nm _ ch) = do
    s <- liftM (lookupSplice nm) get
    maybe runChildren (recurseSplice n) s

  where
    runChildren = do
        newKids <- runNodeList ch
        return [X.modifyChildren (const newKids) n]

-- |Checks the recursion flag and recurses accordingly.
recurseSplice :: Monad m => Node -> Splice m -> Splice m
recurseSplice node splice = do
  result <- local (const node) splice
  ts' <- get
  if _recurse ts'
    then runNodeList result
    else return result

{-|
  Runs a splice in the underlying monad.  Splices require two
  parameters, the template state, and an input node.
-}
runSplice :: Monad m =>
             TemplateState m -- ^ The initial template state
          -> Node            -- ^ The splice's input node
          -> Splice m        -- ^ The splice
          -> m [Node]
runSplice ts node (TemplateMonad splice) = do
  (result,_,_) <- runRWST splice node ts
  return result

{-|
  Runs a template in the underlying monad.  Similar to runSplice
  except that templates don't require a Node as a parameter.
-}
runTemplate :: Monad m => TemplateState m -> Template -> m [Node]
runTemplate ts template = runSplice ts (X.Text "") (runNodeList template)

-- |Runs a template with an empty TemplateState.
runBareTemplate :: Monad m => Template -> m [Node]
runBareTemplate = runTemplate emptyTemplateState

{-
 - Heist built-in splices implementing core template functionality.
 -}

-- The bind splice

-- |Default name for the bind splice.
bindTag :: ByteString
bindTag = "bind"

-- |Default attribute name for the bind tag.
bindAttr :: ByteString
bindAttr = "tag"

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

-- The apply splice

-- | Default name for the apply splice.
applyTag :: ByteString
applyTag = "apply"

-- | Default attribute name for the apply tag.
applyAttr :: ByteString
applyAttr = "template"

-- | Implementation of the bind splice.
applyImpl :: Monad m => Splice m
applyImpl = do
  stopRecursion
  node <- getParamNode
  case X.getAttribute node applyAttr of
    Nothing   -> return [] -- TODO: error handling
    Just attr -> do 
      st <- get
      let template = lookupTemplate attr st
      modify $ \s -> s { _spliceMap = defaultSpliceMap }
      processedChildren <- runNodeList $ X.getChildren node
      modify (bindSplice "content" $ return processedChildren)
      maybe (return []) -- TODO: error handling
            (\t -> do result <- runNodeList t
                      put st
                      return result)
            template
      
-- | The default set of built-in splices.
defaultSpliceMap :: Monad m => SpliceMap m
defaultSpliceMap = Map.fromList $ map (\(a,b) -> (B.append "snap:" a, b))
  [(applyTag, applyImpl)
  ,(bindTag, bindImpl)
  ]

expatOptions :: X.ParserOptions ByteString ByteString
expatOptions =
    X.defaultParserOptions {
           X.parserEncoding = Just X.UTF8
         , X.entityDecoder  = Just (\k -> Map.lookup k htmlEntityLookupTable)
         }

{-
 - Template loading
-}
getDoc :: String -> IO (Either String Node)
getDoc f = do
  bs <- catch (liftM Right $ B.readFile f) (\e -> return $ Left $ show e)
  return $ (mapLeft show . X.parse' expatOptions) =<< bs

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft g = either (Left . g) Right

loadTemplate :: String -> String -> IO TemplateMap
loadTemplate path fname
  | ".tpl" `isSuffixOf` fname = do
--    putStrLn $ "Reading "++fname++" as "++tName
    c <- getDoc fname
    return $ either (const Map.empty) (Map.singleton (splitPaths $ B.pack tName) . (:[])) c
  | otherwise = do
--    putStrLn $ "Skipping "++fname
    return Map.empty
  where tName = drop ((length path)+1) $ take ((length fname) - 4) fname

loadTemplates :: Monad m => FilePath -> IO (TemplateState m)
loadTemplates dir = do
  d <- readDirectoryWith (loadTemplate dir) dir
  let tm = F.fold (free d)
  return $ TemplateState defaultSpliceMap tm True

renderTemplate :: FilePath -> ByteString -> IO [Node]
renderTemplate baseDir name = do
  ts <- loadTemplates baseDir
  let t = lookupTemplate name ts
  runTemplate ts $ maybe [] id t

tShow :: (X.GenericXMLString tag, X.GenericXMLString text)
      => [X.Node tag text] -> IO ()
tShow = L.putStrLn . L.concat . map formatNode

