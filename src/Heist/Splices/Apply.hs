{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Heist.Splices.Apply where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.UUID.V4
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
-- | Raw core of apply functionality.  This is abstracted for use in other
-- places like an enhanced (from the original) bind
--rawApply :: (MonadIO n)
--         => Text
--         -> [X.Node]
--         -> TPath
--         -> [X.Node]
--         -> Splice n
--rawApply paramTag calledNodes newContext paramNodes = do
--    u <- liftIO uuid
--    let newName = T.concat [paramTag, "-", (T.pack $ show u)]
--    hs <- getHS  -- Can't use localHS here because the modifier is not pure
--    processedParams <- runNodeList paramNodes
--    modifyHS (bindSplice newName (return processedParams) .
--              setCurContext newContext)
--
--    if _recursionDepth hs < mAX_RECURSION_DEPTH
--        then do modRecursionDepth (+1)
--                result <- runNodeList $ map (idContent newName) calledNodes
--                restoreHS hs
--                return result
--        else do restoreHS hs
--                return [] `orError` err
--  where
--    err = "template recursion exceeded max depth, "++
--          "you probably have infinite splice recursion!"
--    idContent :: Text -> X.Node -> X.Node
--    idContent newName n@(X.Element nm _ cs)
--      | nm == paramTag = n { X.elementTag = newName }
--      | otherwise = n { X.elementChildren = map (idContent newName) cs}
--    idContent _ n = n


------------------------------------------------------------------------------
-- I'm not sure which of these two approaches is better.  Benchmarks indicate
-- that the above approach rewriting tag names with a uuid might be a
-- little faster with the cache tag, but might be a little slower by itself.
-- It also seems like the uuid aproach might be incorrect in the presence of
-- multiple bind tags that use parameters.
------------------------------------------------------------------------------


rawApply :: (Monad n)
         => Text
         -> [X.Node]
         -> TPath
         -> [X.Node]
         -> Splice n
rawApply paramTag calledNodes newContext paramNodes = do
    hs <- getHS  -- Can't use localHS here because the modifier is not pure
    processedParams <- runNodeList paramNodes

    -- apply should do a bottom-up traversal, so we run the called nodes
    -- before doing <content/> substitution.
    modifyHS (setCurContext newContext)

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
              rawApply "apply-content" (X.docContent $ dfDoc t) ctx nodes)
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


deprecatedContentCheck :: Monad m => Splice m
deprecatedContentCheck =
    return [] `orError` unwords
      ["<content> tag deprecated.  Use"
      ,"<apply-content> or <bind-content>"
      ]

