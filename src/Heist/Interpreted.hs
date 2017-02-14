{-|

This module defines the API for writing and working with interpreted splices.
It exports some of the same symbols as "Heist.Compiled", so you will probably
want to import it qualified.

Interpreted splices can be thought of as a function @Node -> m [Node]@.  Heist
then substitutes the resulting list of nodes into your template in place of
the input node.  'Splice' is implemented as a type synonym @type Splice m =
HeistT m [Node]@, and 'HeistT' has a function 'getParamNode' that lets you get
the input node.

Suppose you have a place on your page where you want to display a link with
the text \"Logout username\" if the user is currently logged in or a link to
the login page if no user is logged in.  Assume you have a function
@getUser :: MyAppMonad (Maybe Text)@ that gets the current user.
You can implement this functionality with a 'Splice' as follows:

> import           Blaze.ByteString.Builder
> import           Data.ByteString.Char8 (ByteString)
> import qualified Data.ByteString.Char8 as B
> import           Data.Text (Text)
> import qualified Data.Text as T
> import qualified Text.XmlHtml as X
>
> import qualified Heist.Interpreted as I
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
> loginLogoutSplice :: I.Splice MyAppMonad
> loginLogoutSplice = do
>     user <- lift getUser
>     return [maybe loginLink logoutLink user]
>

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
  , bindAttributeSplices

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
  , runAttributes
  , runNodeList
  , evalTemplate
  , bindStrings
  , bindString
  , callTemplate
  , callTemplateWithText
  , renderTemplate
  , renderTemplateWithEncoding
  , renderWithArgs
  ) where

import Heist.Interpreted.Internal
import Heist.Common (mapSplices, bindAttributeSplices)

