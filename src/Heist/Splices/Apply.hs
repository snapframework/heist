{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Heist.Splices.Apply where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Interpreted.Internal
import           Heist.Types


------------------------------------------------------------------------------
-- | Default name for the apply splice.
applyTag :: Text
applyTag = "apply"


------------------------------------------------------------------------------
-- | Default attribute name for the apply tag.
applyAttr :: Text
applyAttr = "template"


------------------------------------------------------------------------------
-- | 
rawApply :: (Monad n)
         => Text
         -> [X.Node]
         -> Maybe FilePath
         -> TPath
         -> [X.Node]
         -> Splice n
rawApply paramTag calledNodes templateFile newContext paramNodes = do
    hs <- getHS  -- Can't use localHS here because the modifier is not pure
    processedParams <- runNodeList paramNodes

    -- apply should do a bottom-up traversal, so we run the called nodes
    -- before doing <content/> substitution.
    modifyHS (setCurContext newContext . setCurTemplateFile templateFile)

    let process = concatMap (treeMap processedParams)
    if _recursionDepth hs < mAX_RECURSION_DEPTH
      then do modRecursionDepth (+1)
              res <- runNodeList calledNodes
              restoreHS hs
              return $! process res
      else do restoreHS hs
              (return []) `orError` err
  where
    err = "template recursion exceeded max depth, "++
          "you probably have infinite splice recursion!"
    treeMap :: [X.Node] -> X.Node -> [X.Node]
    treeMap ns n@(X.Element nm _ cs)
      | nm == paramTag = ns
      | otherwise = [n { X.elementChildren = cs' }]
      where
        !cs' = concatMap (treeMap ns) cs
    treeMap _ n = [n]


------------------------------------------------------------------------------
-- | Applies a template as if the supplied nodes were the children of the
-- <apply> tag.
applyNodes :: MonadIO n => Template -> Text -> Splice n
applyNodes nodes template = do
    hs <- getHS
    maybe (return [] `orError` err)
          (\(t,ctx) -> do
              addDoctype $ maybeToList $ X.docType $ dfDoc t
              rawApply "apply-content" (X.docContent $ dfDoc t)
                       (dfFile t) ctx nodes)
          (lookupTemplate (T.encodeUtf8 template) hs _templateMap)
  where
    err = "apply tag cannot find template \""++(T.unpack template)++"\""


------------------------------------------------------------------------------
-- | Implementation of the apply splice.
applyImpl :: MonadIO n => Splice n
applyImpl = do
    node <- getParamNode
    let err = "must supply \"" ++ T.unpack applyAttr ++
              "\" attribute in <" ++ T.unpack (X.elementTag node) ++ ">"
    case X.getAttribute applyAttr node of
        Nothing   -> return [] `orError` err
        Just template -> applyNodes (X.childNodes node) template


------------------------------------------------------------------------------
-- | This splice crashes with an error message.  Its purpose is to provide a
-- load-time warning to anyone still using the old content tag in their
-- templates.  In Heist 0.10, tho content tag was replaced by two separate
-- apply-content and bind-content tags used by the apply and bind splices
-- respectively.
deprecatedContentCheck :: Monad m => Splice m
deprecatedContentCheck =
    return [] `orError` unwords
      ["<content> tag deprecated.  Use"
      ,"<apply-content> or <bind-content>"
      ]

