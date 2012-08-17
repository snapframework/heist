{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Heist.Compiled.Splices.Apply where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Compiled.Internal
import           Heist.Types


------------------------------------------------------------------------------
-- | Default name for the apply splice.
applyTag :: Text
applyTag = "apply"


------------------------------------------------------------------------------
-- | Raw core of apply functionality.  This is abstracted for use in other
-- places like an enhanced (from the original) bind
rawApply :: (Monad m)
         => Template
         -> TPath
         -> Template
         -> Splice m
rawApply templateNodes newContext contentNodes = localTS id $ do
    modifyTS (setCurContext newContext . 
              bindSplice "content" (runNodeList contentNodes))

    runNodeList templateNodes


------------------------------------------------------------------------------
-- | Applies a template as if the supplied nodes were the children of the
-- <apply> tag.
applyNodes :: (Monad m) => [X.Node] -> Text -> Splice m
applyNodes contentNodes template = do
    hs <- getTS

    -- TODO: better error handling
    maybe (err $ "Can't find template " ++ show template)
          (\(t,ctx) -> rawApply (X.docContent $ dfDoc t) ctx contentNodes)
          (lookupTemplate (T.encodeUtf8 template) hs _templateMap)


------------------------------------------------------------------------------
-- | Implementation of the apply splice.
applyImpl :: (Monad m) => Splice m
applyImpl = do
    node <- getParamNode
    case X.getAttribute "template" node of
        -- TODO: better error handling
        Nothing       -> err "apply tag does not specify a template"
        Just template -> applyNodes (X.childNodes node) template


------------------------------------------------------------------------------
-- | Show a nice error message with the current template context so the user
-- has a clue which template caused the problem.
err :: Monad m => String -> HeistT n m b
err msg = do
    inames <- getsTS templateNames
    cnames <- getsTS compiledTemplateNames
    ctx <- getContext
    error $ "in template " ++ (showTPath ctx) ++ ": " ++ msg
            ++ "\ninames"
            ++ concatMap (\t -> "\n"++show t) inames
            ++ "\ncnames"
            ++ concatMap (\t -> "\n"++show t) cnames

dbg msg = do
    ctx <- getContext
    liftIO $ putStrLn $ "in template " ++ (showTPath ctx) ++ ": " ++ msg

