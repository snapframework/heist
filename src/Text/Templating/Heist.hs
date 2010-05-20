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
  > go = runRawTemplate st template
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
  , TemplateState

    -- * Functions and declarations on TemplateState values
  , addTemplate
  , emptyTemplateState
  , bindSplice
  , lookupSplice
  , lookupTemplate
  , addOnLoadHook
  , addPreRunHook
  , addPostRunHook
  , setTemplates
  , loadTemplates

    -- * TemplateMonad functions
  , stopRecursion
  , getParamNode
  , runNodeList
  , getContext

    -- * Functions for running splices and templates
  , runTemplate
  , evalTemplate
  , callTemplate
  , renderTemplate
  , bindStrings

    -- * Misc functions
  , runSplice
  , runRawTemplate
  , getDoc
  , bindStaticTag

  , heistExpatOptions
  , module Text.Templating.Heist.Constants
  ) where

import qualified Data.Map as Map
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Constants
import           Text.Templating.Heist.Splices


------------------------------------------------------------------------------
-- | The default set of built-in splices.
defaultSpliceMap :: Monad m => SpliceMap m
defaultSpliceMap = Map.fromList
    [(applyTag, applyImpl)
    ,(bindTag, bindImpl)
    ,(ignoreTag, ignoreImpl)
    ]


------------------------------------------------------------------------------
-- | An empty template state, with Heist's default splices (@\<bind\>@ and
-- @\<apply\>@) mapped.
emptyTemplateState :: Monad m => TemplateState m
emptyTemplateState = TemplateState defaultSpliceMap Map.empty True [] 0
                                   return return return


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.  (Old convenience code.)
--renderTemplate' :: FilePath -> ByteString -> IO (Maybe ByteString)
--renderTemplate' baseDir name = do
--    etm <- loadTemplates baseDir emptyTemplateState
--    let ts = either (const emptyTemplateState) id etm
--    ns <- runTemplate ts name
--    return $ (Just . formatList') =<< ns


