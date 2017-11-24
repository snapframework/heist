{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heist.Common where

------------------------------------------------------------------------------
import           Control.Applicative      (Alternative (..))
import           Control.Exception        (SomeException)
import qualified Control.Exception.Lifted as C
import           Control.Monad            (liftM, mplus)
import qualified Data.Attoparsec.Text     as AP
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.List                (isSuffixOf)
import           Data.Map.Syntax
import           Data.Maybe               (isJust)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Heist.Internal.Types.HeistState
import           System.FilePath          (pathSeparator)
import qualified Text.XmlHtml             as X
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative      (Applicative (..), (<$>))
import           Data.Monoid              (Monoid (..))
#endif
------------------------------------------------------------------------------


------------------------------------------------------------------------------
runHashMap
    :: Splices s
    -> Either [String] (HashMap T.Text s)
runHashMap ms =
    case runMapSyntax Map.lookup Map.insert ms of
      Left keys -> Left $ map (T.unpack . mkMsg) keys
      Right hm -> Right hm
  where
    mkMsg k = "You tried to bind "<>k<>" more than once!"


------------------------------------------------------------------------------
runMapNoErrors :: (Eq k, Hashable k) => MapSyntaxM k v a -> HashMap k v
runMapNoErrors = either (const mempty) id .
    runMapSyntax' (\_ new _ -> Just new) Map.lookup Map.insert

applySpliceMap :: HeistState n
                -> (HeistState n -> HashMap Text v)
                -> MapSyntaxM Text v a
                -> HashMap Text v
applySpliceMap hs f =  (flip Map.union (f hs)) .
    runMapNoErrors .
    mapK (mappend pre)
  where
    pre = _splicePrefix hs

------------------------------------------------------------------------------
-- | If Heist is running in fail fast mode, then this function will throw an
-- exception with the second argument as the error message.  Otherwise, the
-- first argument will be executed to represent silent failure.
--
-- This behavior allows us to fail quickly if an error crops up during
-- load-time splice processing or degrade more gracefully if the error occurs
-- while a user request is being processed.
orError :: Monad m => HeistT n m b -> String -> HeistT n m b
orError silent msg = do
    hs <- getHS
    if _preprocessingMode hs
      then do fullMsg <- heistErrMsg (T.pack msg)
              error $ T.unpack fullMsg
      else silent


------------------------------------------------------------------------------
-- | Prepends the location of the template currently being processed to an
-- error message.
heistErrMsg :: Monad m => Text -> HeistT n m Text
heistErrMsg msg = do
    tf <- getsHS _curTemplateFile
    return $ (maybe "" ((`mappend` ": ") . T.pack) tf) `mappend` msg


------------------------------------------------------------------------------
-- | Adds an error message to the list of splice processing errors.
tellSpliceError :: Monad m => Text -> HeistT n m ()
tellSpliceError msg = do
    hs <- getHS
    node <- getParamNode
    let spliceError = SpliceError
                      { spliceHistory = _splicePath hs
                      , spliceTemplateFile = _curTemplateFile hs
                      , visibleSplices = Map.keys $ _compiledSpliceMap hs
                      , contextNode = node
                      , spliceMsg = msg
                      }
    modifyHS (\hs' -> hs { _spliceErrors = spliceError : _spliceErrors hs' })


------------------------------------------------------------------------------
-- | Function for showing a TPath.
showTPath :: TPath -> String
showTPath = BC.unpack . (`BC.append` ".tpl") . tpathName


------------------------------------------------------------------------------
-- | Convert a TPath into a ByteString path.
tpathName :: TPath -> ByteString
tpathName = BC.intercalate "/" . reverse


------------------------------------------------------------------------------
-- | Sets the current template file.
setCurTemplateFile :: Maybe FilePath -> HeistState n -> HeistState n
setCurTemplateFile Nothing ts = ts
setCurTemplateFile fp ts = ts { _curTemplateFile = fp }


------------------------------------------------------------------------------
setCurContext :: TPath -> HeistState n -> HeistState n
setCurContext tp ts = ts { _curContext = tp }


------------------------------------------------------------------------------
-- | Parser for attribute variable substitution.
attParser :: AP.Parser [AttAST]
attParser = liftM ($! []) (loop id)
  where
    append !dl !x = dl . (x:)

    loop !dl = go id
      where
        finish subDL = let !txt = T.concat $! subDL []
                           lit  = Literal $! T.concat $! subDL []
                       in return $! if T.null txt
                                      then dl
                                      else append dl lit

        go !subDL = (gobbleText >>= go . append subDL)
                    <|> (AP.endOfInput *> finish subDL)
                    <|> (do
                            idp <- identParser
                            dl' <- finish subDL
                            loop $! append dl' idp)

    gobbleText = AP.takeWhile1 (AP.notInClass "$")

    identParser = AP.char '$' *> (ident <|> return (Literal "$"))
    ident = (AP.char '{' *> (Ident <$> AP.takeWhile (/='}')) <* AP.string "}")


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order.  If the
-- path is absolute, we need to remove the leading slash so the split doesn't
-- leave @\"\"@ as the last element of the TPath.
--
-- FIXME @\"..\"@ currently doesn't work in paths, the solution is non-trivial
splitPathWith :: Char -> ByteString -> TPath
splitPathWith s p = if BC.null p then [] else (reverse $ BC.split s path)
  where
    path = if BC.head p == s then BC.tail p else p


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order using the
-- path separator of the local operating system. See 'splitPathWith' for more
-- details.
splitLocalPath :: ByteString -> TPath
splitLocalPath = splitPathWith pathSeparator


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order using a
-- forward slash (/) as the path separator. See 'splitPathWith' for more
-- details.
splitTemplatePath :: ByteString -> TPath
splitTemplatePath = splitPathWith '/'


------------------------------------------------------------------------------
-- | Convenience function for looking up a template.
lookupTemplate :: ByteString
               -> HeistState n
               -> (HeistState n -> HashMap TPath t)
               -> Maybe (t, TPath)
lookupTemplate nameStr ts tm = f (tm ts) path name
  where
    (name:p) = case splitTemplatePath nameStr of
                   [] -> [""]
                   ps -> ps
    ctx = if B.isPrefixOf "/" nameStr then [] else _curContext ts
    path = p ++ ctx
    f = if '/' `BC.elem` nameStr
            then singleLookup
            else traversePath


------------------------------------------------------------------------------
-- | Returns 'True' if the given template can be found in the heist state.
hasTemplate :: ByteString -> HeistState n -> Bool
hasTemplate nameStr ts =
    isJust $ lookupTemplate nameStr ts _templateMap


------------------------------------------------------------------------------
-- | Does a single template lookup without cascading up.
singleLookup :: (Eq a, Hashable a)
             => HashMap [a] t -> [a] -> a -> Maybe (t, [a])
singleLookup tm path name = fmap (\a -> (a,path)) $ Map.lookup (name:path) tm


------------------------------------------------------------------------------
-- | Searches for a template by looking in the full path then backing up into
-- each of the parent directories until the template is found.
traversePath :: (Eq a, Hashable a)
             => HashMap [a] t -> [a] -> a -> Maybe (t, [a])
traversePath tm [] name = fmap (\a -> (a,[])) (Map.lookup [name] tm)
traversePath tm path name =
    singleLookup tm path name `mplus`
    traversePath tm (tail path) name


------------------------------------------------------------------------------
-- | Maps a splice generating function over a list and concatenates the
-- results.  This function now has a more general type signature so it works
-- with both compiled and interpreted splices.  The old type signature was
-- this:
--
-- > mapSplices :: (Monad n)
-- >         => (a -> Splice n n)
-- >         -> [a]
-- >         -> Splice n n
mapSplices :: (Monad m, Monoid b)
           => (a -> m b)
           -- ^ Splice generating function
           -> [a]
           -- ^ List of items to generate splices for
           -> m b
           -- ^ The result of all splices concatenated together.
mapSplices f vs = liftM mconcat $ mapM f vs
{-# INLINE mapSplices #-}


------------------------------------------------------------------------------
-- | Gets the current context
getContext :: Monad m => HeistT n m TPath
getContext = getsHS _curContext


------------------------------------------------------------------------------
-- | Gets the full path to the file holding the template currently being
-- processed.  Returns Nothing if the template is not associated with a file
-- on disk or if there is no template being processed.
getTemplateFilePath :: Monad m => HeistT n m (Maybe FilePath)
getTemplateFilePath = getsHS _curTemplateFile


------------------------------------------------------------------------------
-- | Loads a template with the specified path and filename.  The
-- template is only loaded if it has a ".tpl" or ".xtpl" extension.
loadTemplate :: String -- ^ path of the template root
             -> String -- ^ full file path (includes the template root)
             -> IO [Either String (TPath, DocumentFile)] --TemplateMap
loadTemplate templateRoot fname = do
    c <- loadTemplate' fname
    return $ map (fmap (\t -> (splitLocalPath $ BC.pack tName, t))) c
  where -- tName is path relative to the template root directory
    isHTMLTemplate = ".tpl"  `isSuffixOf` fname
    correction = if last templateRoot == '/' then 0 else 1
    extLen     = if isHTMLTemplate then 4 else 5
    tName = drop ((length templateRoot)+correction) $
            -- We're only dropping the template root, not the whole path
            take ((length fname) - extLen) fname


------------------------------------------------------------------------------
-- | Loads a template at the specified path, choosing the appropriate parser
-- based on the file extension.  The template is only loaded if it has a
-- \".tpl\" or \".xtpl\" extension.  Returns an empty list if the extension
-- doesn't match.
loadTemplate' :: String -> IO [Either String DocumentFile]
loadTemplate' fullDiskPath
    | isHTMLTemplate = liftM (:[]) $ getDoc fullDiskPath
    | isXMLTemplate = liftM (:[]) $ getXMLDoc fullDiskPath
    | otherwise = return []
  where
    isHTMLTemplate = ".tpl"  `isSuffixOf` fullDiskPath
    isXMLTemplate  = ".xtpl" `isSuffixOf` fullDiskPath


------------------------------------------------------------------------------
-- | Type synonym for parsers.
type ParserFun = String -> ByteString -> Either String X.Document


------------------------------------------------------------------------------
-- | Reads an HTML or XML template from disk.
getDocWith :: ParserFun -> String -> IO (Either String DocumentFile)
getDocWith parser f = do
    bs <- C.catch (liftM Right $ B.readFile f)
                (\(e::SomeException) -> return $ Left $ show e)

    let eitherDoc = either Left (parser f) bs
    return $ either (\s -> Left $ f ++ " " ++ s)
                    (\d -> Right $ DocumentFile d (Just f)) eitherDoc


------------------------------------------------------------------------------
-- | Reads an HTML template from disk.
getDoc :: String -> IO (Either String DocumentFile)
getDoc = getDocWith X.parseHTML


------------------------------------------------------------------------------
-- | Reads an XML template from disk.
getXMLDoc :: String -> IO (Either String DocumentFile)
getXMLDoc = getDocWith X.parseXML


------------------------------------------------------------------------------
-- | Sets the templateMap in a HeistState.
setTemplates :: HashMap TPath DocumentFile -> HeistState n -> HeistState n
setTemplates m ts = ts { _templateMap = m }


------------------------------------------------------------------------------
-- | Adds a template to the heist state.
insertTemplate :: TPath
               -> DocumentFile
               -> HeistState n
               -> HeistState n
insertTemplate p t st =
    setTemplates (Map.insert p t (_templateMap st)) st


------------------------------------------------------------------------------
-- Gives the MIME type for a 'X.Document'
mimeType :: X.Document -> MIMEType
mimeType d = case d of
    (X.HtmlDocument e _ _) -> "text/html;charset=" `BC.append` enc e
    (X.XmlDocument  e _ _) -> "text/xml;charset="  `BC.append` enc e
  where
    enc X.UTF8    = "utf-8"
    -- Should not include byte order designation for UTF-16 since
    -- rendering will include a byte order mark. (RFC 2781, Sec. 3.3)
    enc X.UTF16BE = "utf-16"
    enc X.UTF16LE = "utf-16"
    enc X.ISO_8859_1 = "iso-8859-1"


------------------------------------------------------------------------------
-- | Binds a set of new splice declarations within a 'HeistState'.
bindAttributeSplices :: Splices (AttrSplice n) -- ^ splices to bind
                     -> HeistState n           -- ^ start state
                     -> HeistState n
bindAttributeSplices ss hs =
    hs { _attrSpliceMap = applySpliceMap hs _attrSpliceMap ss }

------------------------------------------------------------------------------
-- | Mappends a doctype to the state.
addDoctype :: Monad m => [X.DocType] -> HeistT n m ()
addDoctype dt = do
    modifyHS (\s -> s { _doctypes = _doctypes s `mappend` dt })


