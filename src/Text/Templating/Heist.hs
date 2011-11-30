{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

  This module contains the core definitions for the Heist template system.

  The Heist template system is based on HTML and XML.  It allows you to build
  custom HTML and XML based markup languages.  With Heist you can define your
  own domain-specific HTML and XML tags implemented with Haskell and use them
  in your templates.

  The most important concept in Heist is the 'Splice'.  Splices can be thought
  of as functions that transform a node into a list of nodes.  Heist then
  substitutes the resulting list of nodes into your template in place of the
  input node.  'Splice' is implemented as a type synonym @type Splice m =
  HeistT m [Node]@, and 'HeistT' has a function 'getParamNode'
  that lets you get the input node.

  Suppose you have a place on your page where you want to display a link with
  the text \"Logout username\" if the user is currently logged in or a link to
  the login page if no user is logged in.  Assume you have a function
  @getUser :: MyAppMonad (Maybe Text)@ that gets the current user.
  You can implement this functionality with a 'Splice' as follows:

  > import             Blaze.ByteString.Builder
  > import             Data.ByteString.Char8 (ByteString)
  > import qualified   Data.ByteString.Char8 as B
  > import             Data.Text (Text)
  > import qualified   Data.Text as T
  > import qualified   Text.XmlHtml as X
  >
  > import             Text.Templating.Heist
  >
  > link :: Text -> Text -> X.Node
  > link target text = X.Element "a" [("href", target)] [X.TextNode text]
  >
  > loginLink :: X.Node
  > loginLink = link "/login" "Login"
  >
  > logoutLink :: Text -> X.Node
  > logoutLink user = link "/logout" (T.append "Logout " user)
  >
  > loginLogoutSplice :: Splice MyAppMonad
  > loginLogoutSplice = do
  >     user <- lift getUser
  >     return [maybe loginLink logoutLink user]
  >

  Next, you need to bind that splice to a tag.  Heist stores information
  about splices and templates in the 'HeistState' data structure.  The
  following code demonstrates how this splice would be used.

  > mySplices = [ ("loginLogout", loginLogoutSplice) ]
  >
  > main = do
  >     ets <- loadTemplates "templates" $
  >            bindSplices mySplices emptyHeistState
  >     let ts = either error id ets
  >     t <- runMyAppMonad $ renderTemplate ts "index"
  >     print $ maybe "Page not found" (toByteString . fst) t

  Here we build up our 'HeistState' by starting with emptyHeistState and
  applying bindSplice for all the splices we want to add.  Then we pass this
  to loadTemplates our final 'HeistState' wrapped in an Either to handle
  errors.  Then we use this 'HeistState' to render our templates.

-}

module Text.Templating.Heist
  (
    -- * Types
    Template
  , MIMEType
  , Splice
  , TemplateMonad
  , HeistT
  , HeistState
  , TemplateState
  , templateNames
  , spliceNames

    -- * Functions and declarations on HeistState values
  , addTemplate
  , addXMLTemplate
  , emptyTemplateState
  , defaultHeistState
  , bindSplice
  , bindSplices
  , lookupSplice
  , setTemplates
  , loadTemplates
  , hasTemplate
  , addTemplatePathPrefix

    -- * Hook functions
    -- $hookDoc
  , addOnLoadHook
  , addPreRunHook
  , addPostRunHook

    -- * HeistT functions
  , stopRecursion
  , getParamNode
  , runNodeList
  , getContext
  , getTemplateFilePath

  , localParamNode
  , getsTS
  , getTS
  , putTS
  , modifyTS
  , restoreTS
  , localTS

    -- * Functions for running splices and templates
  , evalTemplate
  , callTemplate
  , callTemplateWithText
  , renderTemplate
  , renderWithArgs
  , bindStrings
  , bindString

    -- * Functions for creating splices
  , textSplice
  , runChildren
  , runChildrenWith
  , runChildrenWithTrans
  , runChildrenWithTemplates
  , runChildrenWithText
  , mapSplices

    -- * Misc functions
  , getDoc
  , getXMLDoc
  , mkCacheTag

    -- * Temporary functions
  , useOldAttributeSyntax
  ) where

import           Control.Monad.Trans
import qualified Data.Map as Map
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Splices
import           Text.Templating.Heist.Types


------------------------------------------------------------------------------
-- | The default set of built-in splices.
defaultSpliceMap :: MonadIO m => SpliceMap m
defaultSpliceMap = Map.fromList
    [(applyTag, applyImpl)
    ,(bindTag, bindImpl)
    ,(ignoreTag, ignoreImpl)
    ,(markdownTag, markdownSplice)
    ]


------------------------------------------------------------------------------
-- | An empty heist state, with Heist's default splices (@\<apply\>@,
-- @\<bind\>@, @\<ignore\>@, and @\<markdown\>@) mapped.  The cache tag is
-- not mapped here because it must be mapped manually in your application.
defaultHeistState :: MonadIO m => HeistState m
defaultHeistState =
    HeistState (defaultSpliceMap) Map.empty True [] 0
               return return return [] Nothing False


{-# DEPRECATED emptyTemplateState "NOTICE: The name TemplateState is changing to HeistState.  Use defaultHeistState instead of emptyTemplateState." #-}
------------------------------------------------------------------------------
-- | An empty template state, with Heist's default splices (@\<apply\>@,
-- @\<bind\>@, @\<ignore\>@, and @\<markdown\>@) mapped.  The static tag is
-- not mapped here because it must be mapped manually in your application.
emptyTemplateState :: MonadIO m => HeistState m
emptyTemplateState = defaultHeistState


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

