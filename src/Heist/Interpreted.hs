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
  >            bindSplices mySplices defaultHeistState
  >     let ts = either error id ets
  >     t <- runMyAppMonad $ renderTemplate ts "index"
  >     print $ maybe "Page not found" (toByteString . fst) t

  Here we build up our 'HeistState' by starting with defaultHeistState and
  applying bindSplice for all the splices we want to add.  Then we pass this
  to loadTemplates our final 'HeistState' wrapped in an Either to handle
  errors.  Then we use this 'HeistState' to render our templates.

-}

module Heist.Interpreted
  (
    Splice

  -- * HeistState Functions
  , addTemplate
  , addXMLTemplate
  , lookupSplice
  , bindSplice
  , bindSplices

  -- * Functions for creating splices
  , textSplice
  , runChildren
  , runChildrenWith
  , runChildrenWithTrans
  , runChildrenWithTemplates
  , runChildrenWithText
  , mapSplices

  -- * HeistT functions
  , stopRecursion
  , runNode
  , runNodeList
  , evalTemplate
  , bindStrings
  , bindString
  , callTemplate
  , callTemplateWithText
  , renderTemplate
  , renderWithArgs
  ) where

import Heist.Interpreted.Internal
import Heist.Common (mapSplices)

