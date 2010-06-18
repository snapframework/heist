{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Templating.Heist.Internal where

------------------------------------------------------------------------------
import             Control.Applicative
import             Control.Exception (SomeException)
import             Control.Monad.CatchIO
import "monads-fd" Control.Monad.RWS.Strict
import qualified   Data.Attoparsec.Char8 as AP
import             Data.ByteString.Char8 (ByteString)
import qualified   Data.ByteString.Char8 as B
import qualified   Data.ByteString.Lazy as L
import             Data.Either
import qualified   Data.Foldable as F
import             Data.List
import qualified   Data.Map as Map
import             Data.Maybe
import             Prelude hiding (catch)
import             System.Directory.Tree hiding (name)
import             System.FilePath
import             Text.XML.Expat.Format
import qualified   Text.XML.Expat.Tree as X

------------------------------------------------------------------------------
import             Text.Templating.Heist.Constants
import             Text.Templating.Heist.Types


------------------------------------------------------------------------------
-- | Mappends a doctype to the state.
addDoctype :: Monad m => [ByteString] -> TemplateMonad m ()
addDoctype dt = do
    modifyTS (\s -> s { _doctypes = _doctypes s `mappend` dt })


------------------------------------------------------------------------------
-- TemplateState functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Adds an on-load hook to a `TemplateState`.
addOnLoadHook :: (Monad m) =>
                 (Template -> IO Template)
              -> TemplateState m
              -> TemplateState m
addOnLoadHook hook ts = ts { _onLoadHook = _onLoadHook ts >=> hook }


------------------------------------------------------------------------------
-- | Adds a pre-run hook to a `TemplateState`.
addPreRunHook :: (Monad m) =>
                 (Template -> m Template)
              -> TemplateState m
              -> TemplateState m
addPreRunHook hook ts = ts { _preRunHook = _preRunHook ts >=> hook }


------------------------------------------------------------------------------
-- | Adds a post-run hook to a `TemplateState`.
addPostRunHook :: (Monad m) =>
                  (Template -> m Template)
               -> TemplateState m
               -> TemplateState m
addPostRunHook hook ts = ts { _postRunHook = _postRunHook ts >=> hook }


------------------------------------------------------------------------------
-- | Binds a new splice declaration to a tag name within a 'TemplateState'.
bindSplice :: Monad m =>
              ByteString        -- ^ tag name
           -> Splice m          -- ^ splice action
           -> TemplateState m   -- ^ source state
           -> TemplateState m
bindSplice n v ts = ts {_spliceMap = Map.insert n v (_spliceMap ts)}


------------------------------------------------------------------------------
-- | Binds a set of new splice declarations within a 'TemplateState'.
bindSplices :: Monad m =>
               [(ByteString, Splice m)] -- ^ splices to bind action
            -> TemplateState m   -- ^ source state
            -> TemplateState m
bindSplices ss ts = foldl' (flip id) ts acts 
  where
    acts = map (uncurry bindSplice) ss

------------------------------------------------------------------------------
-- | Convenience function for looking up a splice.
lookupSplice :: Monad m =>
                ByteString
             -> TemplateState m
             -> Maybe (Splice m)
lookupSplice nm ts = Map.lookup nm $ _spliceMap ts


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order.  If the
-- path is absolute, we need to remove the leading slash so the split doesn't
-- leave @\"\"@ as the last element of the TPath.
--
-- FIXME @\"..\"@ currently doesn't work in paths, the solution is non-trivial
splitPathWith :: Char -> ByteString -> TPath
splitPathWith s p = if B.null p then [] else (reverse $ B.split s path)
  where
    path = if B.head p == s then B.tail p else p

-- | Converts a path into an array of the elements in reverse order using the
-- path separator of the local operating system. See 'splitPathWith' for more
-- details.
splitLocalPath :: ByteString -> TPath
splitLocalPath = splitPathWith pathSeparator

-- | Converts a path into an array of the elements in reverse order using a
-- forward slash (/) as the path separator. See 'splitPathWith' for more
-- details.
splitTemplatePath :: ByteString -> TPath
splitTemplatePath = splitPathWith '/'


------------------------------------------------------------------------------
-- | Does a single template lookup without cascading up.
singleLookup :: TemplateMap
             -> TPath
             -> ByteString
             -> Maybe (InternalTemplate, TPath)
singleLookup tm path name = fmap (\a -> (a,path)) $ Map.lookup (name:path) tm


------------------------------------------------------------------------------
-- | Searches for a template by looking in the full path then backing up into each
-- of the parent directories until the template is found.
traversePath :: TemplateMap
             -> TPath
             -> ByteString
             -> Maybe (InternalTemplate, TPath)
traversePath tm [] name = fmap (\a -> (a,[])) (Map.lookup [name] tm)
traversePath tm path name =
    singleLookup tm path name `mplus`
    traversePath tm (tail path) name


------------------------------------------------------------------------------
-- | Convenience function for looking up a template.
lookupTemplate :: Monad m =>
                  ByteString
               -> TemplateState m
               -> Maybe (InternalTemplate, TPath)
lookupTemplate nameStr ts = 
    f (_templateMap ts) path name
  where (name:p) = case splitTemplatePath nameStr of
                       [] -> [""]
                       ps -> ps
        path = p ++ (_curContext ts)
        f = if '/' `B.elem` nameStr
                then singleLookup
                else traversePath


------------------------------------------------------------------------------
-- | Sets the templateMap in a TemplateState.
setTemplates :: Monad m => TemplateMap -> TemplateState m -> TemplateState m
setTemplates m ts = ts { _templateMap = m }


------------------------------------------------------------------------------
-- | Adds a template to the template state.
insertTemplate :: Monad m =>
               TPath
            -> InternalTemplate
            -> TemplateState m
            -> TemplateState m
insertTemplate p t st =
    setTemplates (Map.insert p t (_templateMap st)) st


------------------------------------------------------------------------------
-- | Adds a template to the template state.
addTemplate :: Monad m =>
               ByteString
            -> InternalTemplate
            -> TemplateState m
            -> TemplateState m
addTemplate n t st = insertTemplate (splitTemplatePath n) t st


------------------------------------------------------------------------------
-- | Stops the recursive processing of splices.  Consider the following
-- example:
--
--   > <foo>
--   >   <bar>
--   >     ...
--   >   </bar>
--   > </foo>
--
-- Assume that @\"foo\"@ is bound to a splice procedure. Running the @foo@
-- splice will result in a list of nodes @L@.  Normally @foo@ will recursively
-- scan @L@ for splices and run them.  If @foo@ calls @stopRecursion@, @L@
-- will be included in the output verbatim without running any splices.
stopRecursion :: Monad m => TemplateMonad m ()
stopRecursion = modifyTS (\st -> st { _recurse = False })


------------------------------------------------------------------------------
-- | Sets the current context
setContext :: Monad m => TPath -> TemplateMonad m ()
setContext c = modifyTS (\st -> st { _curContext = c })


------------------------------------------------------------------------------
-- | Gets the current context
getContext :: Monad m => TemplateMonad m TPath
getContext = getsTS _curContext
  

------------------------------------------------------------------------------
-- | Performs splice processing on a single node.
runNode :: Monad m => Node -> Splice m
runNode n@(X.Text _)          = return [n]
runNode n@(X.Element nm at ch) = do
    s <- liftM (lookupSplice nm) getTS
    maybe runChildren (recurseSplice n) s
    
  where
    runChildren = do
        newKids <- runNodeList ch
        newAtts <- mapM attSubst at
        return [X.Element nm newAtts newKids]


------------------------------------------------------------------------------
-- | Helper function for substituting a parsed attribute into an attribute
-- tuple.
attSubst :: (Monad m) => (t, ByteString) -> TemplateMonad m (t, ByteString)
attSubst (n,v) = do
    v' <- parseAtt v
    return (n,v')


------------------------------------------------------------------------------
-- | Parses an attribute for any identifier expressions and performs
-- appropriate substitution.
parseAtt :: (Monad m) => ByteString -> TemplateMonad m ByteString
parseAtt bs = do
    let ast = case AP.feed (AP.parse attParser bs) "" of
            (AP.Fail _ _ _) -> []
            (AP.Done _ res) -> res
            (AP.Partial _)  -> []
    chunks <- mapM cvt ast
    return $ B.concat chunks
  where
    cvt (Literal x) = return x
    cvt (Ident x) = getAttributeSplice x


------------------------------------------------------------------------------
-- | AST to hold attribute parsing structure.  This is necessary because
-- attoparsec doesn't support parsers running in another monad.
data AttAST = Literal ByteString |
              Ident ByteString
    deriving (Show)


------------------------------------------------------------------------------
-- | Parser for attribute variable substitution.
attParser :: AP.Parser [AttAST]
attParser = AP.many1 (identParser <|> litParser)
  where
    escChar = (AP.char '\\' *> AP.anyChar) <|>
              AP.satisfy (AP.notInClass "\\$")
    litParser = Literal <$> (B.pack <$> AP.many1 escChar)
    identParser = AP.string "$(" *>
        (Ident <$> AP.takeWhile (/=')')) <* AP.string ")"


------------------------------------------------------------------------------
-- | Get's the attribute value.  If the splice's result list contains non-text
-- nodes, this will translate them into text nodes with textContent and
-- concatenate them together.
--
-- Originally, this only took the first node from the splices's result list,
-- and only if it was a text node. This caused problems when the splice's
-- result contained HTML entities, as they would split a text node. This was
-- then fixed to take the first consecutive bunch of text nodes, and return
-- their concatenation. This was seen as more useful than throwing an error,
-- and more intuitive than trying to render all the nodes as text.
-- 
-- However, it was decided in the end to render all the nodes as text, and
-- then concatenate them. If a splice returned
-- \"some \<b\>text\<\/b\> foobar\", the user would almost certainly want
-- \"some text foobar\" to be rendered, and Heist would probably seem
-- annoyingly limited for not being able to do this. If the user really did
-- want it to render \"some \", it would probably be easier for them to
-- accept that they were silly to pass more than that to be substituted than
-- it would be for the former user to accept that
-- \"some \<b\>text\<\/b\> foobar\" is being rendered as \"some \" because
-- it's \"more intuitive\".
getAttributeSplice :: Monad m => ByteString -> TemplateMonad m ByteString
getAttributeSplice name = do
    s <- liftM (lookupSplice name) getTS
    nodes <- maybe (return []) id s
    return $ B.concat $ map X.textContent nodes

------------------------------------------------------------------------------
-- | Performs splice processing on a list of nodes.
runNodeList :: Monad m => [Node] -> Splice m
runNodeList nodes = liftM concat $ sequence (map runNode nodes)


------------------------------------------------------------------------------
-- | The maximum recursion depth.  (Used to prevent infinite loops.)
mAX_RECURSION_DEPTH :: Int
mAX_RECURSION_DEPTH = 50


------------------------------------------------------------------------------
-- | Checks the recursion flag and recurses accordingly.  Does not recurse
-- deeper than mAX_RECURSION_DEPTH to avoid infinite loops.
recurseSplice :: Monad m => Node -> Splice m -> Splice m
recurseSplice node splice = do
    result <- localParamNode (const node) splice
    ts' <- getTS
    if _recurse ts' && _recursionDepth ts' < mAX_RECURSION_DEPTH
        then do modRecursionDepth (+1)
                res <- runNodeList result
                restoreTS ts'
                return res
        else return result
  where
    modRecursionDepth :: Monad m => (Int -> Int) -> TemplateMonad m ()
    modRecursionDepth f =
        modifyTS (\st -> st { _recursionDepth = f (_recursionDepth st) })


------------------------------------------------------------------------------
-- | Looks up a template name runs a TemplateMonad computation on it.
lookupAndRun :: Monad m
             => ByteString
             -> ((InternalTemplate, TPath) -> TemplateMonad m (Maybe a))
             -> TemplateMonad m (Maybe a)
lookupAndRun name k = do
    ts <- getTS
    maybe (return Nothing) k
          (lookupTemplate name ts)


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.
evalTemplate :: Monad m
            => ByteString
            -> TemplateMonad m (Maybe Template)
evalTemplate name = lookupAndRun name
    (\(t,ctx) -> do
        ts <- getTS
        putTS (ts {_curContext = ctx})
        res <- runNodeList $ _itNodes t
        restoreTS ts
        return $ Just res)


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.  This also
-- executes pre- and post-run hooks and adds the doctype.
evalWithHooks :: Monad m
            => ByteString
            -> TemplateMonad m (Maybe Template)
evalWithHooks name = lookupAndRun name
    (\(t,ctx) -> do
        addDoctype $ maybeToList $ _itDoctype t
        ts <- getTS
        nodes <- lift $ _preRunHook ts $ _itNodes t
        putTS (ts {_curContext = ctx})
        res <- runNodeList nodes
        restoreTS ts
        return . Just =<< lift (_postRunHook ts res))


------------------------------------------------------------------------------
-- | Binds a list of constant string splices
bindStrings :: Monad m
            => [(ByteString, ByteString)]
            -> TemplateState m
            -> TemplateState m
bindStrings pairs ts = foldr add ts pairs
  where
    add (n,v) = bindSplice n (return [X.Text v])


------------------------------------------------------------------------------
-- | Renders a template with the specified parameters.  This is the function
-- to use when you want to "call" a template and pass in parameters from code.
callTemplate :: Monad m
             => ByteString                 -- ^ The name of the template
             -> [(ByteString, ByteString)] -- ^ Association list of
                                           -- (name,value) parameter pairs
             -> TemplateMonad m (Maybe Template)
callTemplate name params = do
    modifyTS $ bindStrings params
    evalTemplate name


------------------------------------------------------------------------------
-- | Converts a Template to an InternalTemplate.  This can only be done inside
-- TemplateMonad where the doctype is available.
toInternalTemplate :: Monad m => Template -> TemplateMonad m InternalTemplate
toInternalTemplate t = do
    dts <- getsTS _doctypes
    return $ InternalTemplate {
        _itDoctype = listToMaybe dts,
        _itNodes = t
    }


------------------------------------------------------------------------------
-- | Renders an internal template by prepending the appropriate doctype.
renderInternal :: Monad m => InternalTemplate -> TemplateMonad m ByteString
renderInternal (InternalTemplate dt nodes) =
    return $ maybe bs (flip B.append bs) dt
  where
    bs = formatList' nodes


------------------------------------------------------------------------------
-- | Renders a template from the specified TemplateState.
renderTemplate :: Monad m
               => TemplateState m
               -> ByteString
               -> m (Maybe ByteString)
renderTemplate ts name = do
    evalTemplateMonad
        (do mt <- evalWithHooks name
            maybe (return Nothing)
                (\t -> liftM Just $ renderInternal =<< toInternalTemplate t)
                mt
        ) (X.Text "") ts

------------------------------------------------------------------------------
-- Template loading
------------------------------------------------------------------------------

-- | Turns an in-memory XML/XHTML bytestring into a (doctype,'[Node]') pair.
parseDoc :: ByteString -> IO (Either String (Maybe ByteString,[Node]))
parseDoc bs = do
    let (doctype,rest) = extractDoctype bs
    let wrap b = B.concat ["<snap:root>\n", b, "\n</snap:root>"]

    return $
      mapRight (\n -> (doctype,X.getChildren n)) $
      mapLeft genErrorMsg $
      X.parse' heistExpatOptions (wrap rest)

  where
    genErrorMsg (X.XMLParseError str loc) = locMsg loc ++ ": " ++ translate str

    locMsg (X.XMLParseLocation line col _ _) =
        "(line " ++ show (line-1) ++ ", col " ++ show col ++ ")"

    translate "junk after document element" = "document must have a single root element"
    translate s = s


-- | Reads an XML document from disk.
getDoc :: String -> IO (Either String InternalTemplate)
getDoc f = do
    bs <- catch (liftM Right $ B.readFile f)
                (\(e::SomeException) -> return $ Left $ show e)

    d' <- either (return . Left)
                 parseDoc
                 bs

    let d = mapLeft (\s -> f ++ " " ++ s) d'

    return $ either Left
               (\(doctype, nodes) -> Right $ InternalTemplate {
                    _itDoctype = doctype,
                    _itNodes = nodes
                })
               d


------------------------------------------------------------------------------
-- | Checks whether the bytestring has a doctype.
hasDoctype :: ByteString -> Bool
hasDoctype bs = "<!DOCTYPE" `B.isPrefixOf` bs


------------------------------------------------------------------------------
-- | Converts a ByteString into a tuple containing a possible doctype
-- ByteString and the rest of the document.
extractDoctype :: ByteString -> (Maybe ByteString, ByteString)
extractDoctype bs = 
    if hasDoctype bs
        then (Just $ B.snoc (B.takeWhile p bs) '>',B.tail $ B.dropWhile p bs)
        else (Nothing, bs)
  where
    p = (/='>')

------------------------------------------------------------------------------
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft g = either (Left . g) Right
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight g = either Left (Right . g)


------------------------------------------------------------------------------
-- | Loads a template with the specified path and filename.  The
-- template is only loaded if it has a ".tpl" extension.
loadTemplate :: String -- ^ path of the template root
             -> String -- ^ full file path (includes the template root)
             -> IO [Either String (TPath, InternalTemplate)] --TemplateMap
loadTemplate templateRoot fname
    | ".tpl" `isSuffixOf` fname = do
        c <- getDoc fname
        return [fmap (\t -> (splitLocalPath $ B.pack tName, t)) c]
    | otherwise = return []
  where -- tName is path relative to the template root directory
        correction = if last templateRoot == '/' then 0 else 1
        tName = drop ((length templateRoot)+correction) $
                -- We're only dropping the template root, not the whole path
                take ((length fname) - 4) fname


------------------------------------------------------------------------------
-- | Traverses the specified directory structure and builds a
-- TemplateState by loading all the files with a ".tpl" extension.
loadTemplates :: Monad m => FilePath -> TemplateState m -> IO (Either String (TemplateState m))
loadTemplates dir ts = do
    d <- readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
    case errs of
        [] -> liftM Right $ foldM loadHook ts $ rights tlist
        _  -> return $ Left $ unlines errs


------------------------------------------------------------------------------
-- | Reversed list of directories.  This holds the path to the template
runHookInternal :: Monad m => (Template -> m Template)
                -> InternalTemplate
                -> m InternalTemplate
runHookInternal f t = do
    n <- f $ _itNodes t
    return $ t { _itNodes = n }


------------------------------------------------------------------------------
-- | Runs the onLoad hook on the template and returns the `TemplateState`
-- with the result inserted.
loadHook :: Monad m => TemplateState m -> (TPath, InternalTemplate) -> IO (TemplateState m)
loadHook ts (tp, t) = do
    t' <- runHookInternal (_onLoadHook ts) t
    return $ insertTemplate tp t' ts


------------------------------------------------------------------------------
-- These are here until we can get them into hexpat.
------------------------------------------------------------------------------

formatList :: (X.GenericXMLString tag, X.GenericXMLString text) =>
              [X.Node tag text]
           -> L.ByteString
formatList nodes = foldl L.append L.empty $ map formatNode nodes

formatList' :: (X.GenericXMLString tag, X.GenericXMLString text) =>
               [X.Node tag text]
            -> B.ByteString
formatList' = B.concat . L.toChunks . formatList

