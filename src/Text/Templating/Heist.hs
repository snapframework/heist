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

import Text.Templating.Heist.Internal
import Text.Templating.Heist.Constants
