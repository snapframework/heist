{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Heist.Interpreted.Internal where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Arrow hiding (loop)
import           Control.Monad
import qualified Data.Attoparsec.Text as AP
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.List
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text (Text)
import           Prelude hiding (catch)
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Types


type Splice n = HeistT n n Template


------------------------------------------------------------------------------
-- | Mappends a doctype to the state.
addDoctype :: Monad m => [X.DocType] -> HeistT n m ()
addDoctype dt = do
    modifyTS (\s -> s { _doctypes = _doctypes s `mappend` dt })


------------------------------------------------------------------------------
-- HeistState functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Adds an on-load hook to a `HeistState`.
addOnLoadHook :: (Template -> IO Template)
              -> HeistState n
              -> HeistState n
addOnLoadHook hook ts = ts { _onLoadHook = _onLoadHook ts >=> hook }


------------------------------------------------------------------------------
-- | Binds a new splice declaration to a tag name within a 'HeistState'.
bindSplice :: Text              -- ^ tag name
           -> Splice n          -- ^ splice action
           -> HeistState n    -- ^ source state
           -> HeistState n
bindSplice n v ts = ts {_spliceMap = Map.insert n v (_spliceMap ts)}


------------------------------------------------------------------------------
-- | Binds a set of new splice declarations within a 'HeistState'.
bindSplices :: [(Text, Splice n)] -- ^ splices to bind
            -> HeistState n       -- ^ start state
            -> HeistState n
bindSplices ss ts = foldl' (flip id) ts acts
  where
    acts = map (uncurry bindSplice) ss


------------------------------------------------------------------------------
-- | Converts 'Text' to a splice returning a single 'TextNode'.
textSplice :: Monad n => Text -> Splice n
textSplice t = return [X.TextNode t]


------------------------------------------------------------------------------
-- | Runs the parameter node's children and returns the resulting node list.
-- By itself this function is a simple passthrough splice that makes the
-- spliced node disappear.  In combination with locally bound splices, this
-- function makes it easier to pass the desired view into your splices.
runChildren :: Monad n => Splice n
runChildren = runNodeList . X.childNodes =<< getParamNode


------------------------------------------------------------------------------
-- | Binds a list of splices before using the children of the spliced node as
-- a view.
runChildrenWith :: (Monad n)
                => [(Text, Splice n)]
                -- ^ List of splices to bind before running the param nodes.
                -> Splice n
                -- ^ Returns the passed in view.
runChildrenWith splices = localTS (bindSplices splices) runChildren


------------------------------------------------------------------------------
-- | Wrapper around runChildrenWith that applies a transformation function to
-- the second item in each of the tuples before calling runChildrenWith.
runChildrenWithTrans :: (Monad n)
          => (b -> Splice n)
          -- ^ Splice generating function
          -> [(Text, b)]
          -- ^ List of tuples to be bound
          -> Splice n
runChildrenWithTrans f = runChildrenWith . map (second f)


------------------------------------------------------------------------------
-- | Like runChildrenWith but using constant templates rather than dynamic
-- splices.
runChildrenWithTemplates :: (Monad n) => [(Text, Template)] -> Splice n
runChildrenWithTemplates = runChildrenWithTrans return


------------------------------------------------------------------------------
-- | Like runChildrenWith but using literal text rather than dynamic splices.
runChildrenWithText :: (Monad n) => [(Text, Text)] -> Splice n
runChildrenWithText = runChildrenWithTrans textSplice


------------------------------------------------------------------------------
-- | Convenience function for looking up a splice.
lookupSplice :: Text
             -> HeistState n
             -> Maybe (Splice n)
lookupSplice nm ts = Map.lookup nm $ _spliceMap ts
{-# INLINE lookupSplice #-}


------------------------------------------------------------------------------
-- | Adds an HTML format template to the heist state.
addTemplate :: ByteString
            -- ^ Path that the template will be referenced by
            -> Template
            -- ^ The template's DOM nodes
            -> Maybe FilePath
            -- ^ An optional path to the actual file on disk where the
            -- template is stored
            -> HeistState n
            -> HeistState n
addTemplate n t mfp st =
    insertTemplate (splitTemplatePath n) doc st
  where
    doc = DocumentFile (X.HtmlDocument X.UTF8 Nothing t) mfp


------------------------------------------------------------------------------
-- | Adds an XML format template to the heist state.
addXMLTemplate :: ByteString
               -- ^ Path that the template will be referenced by
               -> Template
               -- ^ The template's DOM nodes
               -> Maybe FilePath
               -- ^ An optional path to the actual file on disk where the
               -- template is stored
               -> HeistState n
               -> HeistState n
addXMLTemplate n t mfp st =
    insertTemplate (splitTemplatePath n) doc st
  where
    doc = DocumentFile (X.XmlDocument X.UTF8 Nothing t) mfp


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
stopRecursion :: Monad m => HeistT n m ()
stopRecursion = modifyTS (\st -> st { _recurse = False })


------------------------------------------------------------------------------
-- | Performs splice processing on a single node.
runNode :: Monad n => X.Node -> Splice n
runNode (X.Element nm at ch) = do
    newAtts <- mapM attSubst at
    let n = X.Element nm newAtts ch
    s <- liftM (lookupSplice nm) getTS
    maybe (runKids newAtts) (recurseSplice n) s
  where
    runKids newAtts = do
        newKids <- runNodeList ch
        return [X.Element nm newAtts newKids]
runNode n                    = return [n]


------------------------------------------------------------------------------
-- | Helper function for substituting a parsed attribute into an attribute
-- tuple.
attSubst :: (Monad n) => (t, Text) -> HeistT n n (t, Text)
attSubst (n,v) = do
    v' <- parseAtt v
    return (n,v')


------------------------------------------------------------------------------
-- | Parses an attribute for any identifier expressions and performs
-- appropriate substitution.
parseAtt :: (Monad n) => Text -> HeistT n n Text
parseAtt bs = do
    let ast = case AP.feed (AP.parse attParser bs) "" of
                (AP.Done _ res) -> res
                (AP.Fail _ _ _) -> []
                (AP.Partial _)  -> []
    chunks <- mapM cvt ast
    return $ T.concat chunks
  where
    cvt (Literal x) = return x
    cvt (Escaped c) = renderEscaped c
    cvt (Ident x)   =
        localParamNode (const $ X.Element x [] []) $ getAttributeSplice x

    renderEscaped c = do
        hs <- getTS
        if _preprocessingMode hs
          then return $ T.snoc "\\" c
          else return $ T.singleton c


------------------------------------------------------------------------------
-- | Gets the attribute value.  If the splice's result list contains non-text
-- nodes, this will translate them into text nodes with nodeText and
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
getAttributeSplice :: Monad n => Text -> HeistT n n Text
getAttributeSplice name = do
    hs <- getTS
    let noSplice = if _preprocessingMode hs
                     then return $ T.concat ["${", name, "}"]
                     else return ""
    let s = lookupSplice name hs
    maybe noSplice (liftM (T.concat . map X.nodeText)) s

------------------------------------------------------------------------------
-- | Performs splice processing on a list of nodes.
runNodeList :: Monad n => [X.Node] -> Splice n
runNodeList = mapSplices runNode
{-# INLINE runNodeList #-}


------------------------------------------------------------------------------
-- | The maximum recursion depth.  (Used to prevent infinite loops.)
mAX_RECURSION_DEPTH :: Int
mAX_RECURSION_DEPTH = 50


------------------------------------------------------------------------------
-- | Checks the recursion flag and recurses accordingly.  Does not recurse
-- deeper than mAX_RECURSION_DEPTH to avoid infinite loops.
recurseSplice :: Monad n => X.Node -> Splice n -> Splice n
recurseSplice node splice = do
    result <- localParamNode (const node) splice
    ts' <- getTS
    if _recurse ts' && _recursionDepth ts' < mAX_RECURSION_DEPTH
        then do modRecursionDepth (+1)
                res <- runNodeList result
                restoreTS ts'
                return res
        else return result


------------------------------------------------------------------------------
-- | Looks up a template name runs a 'HeistT' computation on it.
lookupAndRun :: Monad m
             => ByteString
             -> ((DocumentFile, TPath) -> HeistT n m (Maybe a))
             -> HeistT n m (Maybe a)
lookupAndRun name k = do
    ts <- getTS
    let mt = lookupTemplate name ts _templateMap
    let curPath = join $ fmap (dfFile . fst) mt
    modifyTS (setCurTemplateFile curPath)
    maybe (return Nothing) k mt


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.
evalTemplate :: Monad n
             => ByteString
             -> HeistT n n (Maybe Template)
evalTemplate name = lookupAndRun name
    (\(t,ctx) -> localTS (\ts -> ts {_curContext = ctx})
                         (liftM Just $ runNodeList $ X.docContent $ dfDoc t))


------------------------------------------------------------------------------
-- | Sets the document type of a 'X.Document' based on the 'HeistT'
-- value.
fixDocType :: Monad m => X.Document -> HeistT n m X.Document
fixDocType d = do
    dts <- getsTS _doctypes
    return $ d { X.docType = listToMaybe dts }


------------------------------------------------------------------------------
-- | Same as evalWithHooks, but returns the entire 'X.Document' rather than
-- just the nodes.  This is the right thing to do if we are starting at the
-- top level.
evalWithHooksInternal :: Monad n
                      => ByteString
                      -> HeistT n n (Maybe X.Document)
evalWithHooksInternal name = lookupAndRun name $ \(t,ctx) -> do
    addDoctype $ maybeToList $ X.docType $ dfDoc t
    ts <- getTS
    let nodes = X.docContent $ dfDoc t
    putTS (ts {_curContext = ctx})
    newNodes <- runNodeList nodes
    restoreTS ts
    newDoc   <- fixDocType $ (dfDoc t) { X.docContent = newNodes }
    return (Just newDoc)


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.  This also
-- executes pre- and post-run hooks and adds the doctype.
evalWithHooks :: Monad n
              => ByteString
              -> HeistT n n (Maybe Template)
evalWithHooks name = liftM (liftM X.docContent) (evalWithHooksInternal name)


------------------------------------------------------------------------------
-- | Binds a list of constant string splices.
bindStrings :: Monad n
            => [(Text, Text)]
            -> HeistState n
            -> HeistState n
bindStrings pairs ts = foldr (uncurry bindString) ts pairs


------------------------------------------------------------------------------
-- | Binds a single constant string splice.
bindString :: Monad n
           => Text
           -> Text
           -> HeistState n
           -> HeistState n
bindString n = bindSplice n . textSplice


------------------------------------------------------------------------------
-- | Renders a template with the specified parameters.  This is the function
-- to use when you want to "call" a template and pass in parameters from
-- inside a splice.  If the template does not exist, this version simply
-- returns an empty list.
callTemplate :: Monad n
             => ByteString         -- ^ The name of the template
             -> [(Text, Splice n)] -- ^ Association list of
                                   -- (name,value) parameter pairs
             -> HeistT n n Template
callTemplate name params = do
    modifyTS $ bindSplices params
    liftM (maybe [] id) $ evalTemplate name


------------------------------------------------------------------------------
-- | Like callTemplate except the splices being bound are constant text
-- splices.
callTemplateWithText :: Monad n
                     => ByteString     -- ^ The name of the template
                     -> [(Text, Text)] -- ^ Association list of
                                       -- (name,value) parameter pairs
                     -> HeistT n n Template
callTemplateWithText name params = do
    modifyTS $ bindStrings params
    liftM (maybe [] id) $ evalTemplate name


------------------------------------------------------------------------------
-- Gives the MIME type for a 'X.Document'
mimeType :: X.Document -> ByteString
mimeType d = case d of
    (X.HtmlDocument e _ _) -> "text/html;charset=" `BC.append` enc e
    (X.XmlDocument  e _ _) -> "text/xml;charset="  `BC.append` enc e
  where
    enc X.UTF8    = "utf-8"
    -- Should not include byte order designation for UTF-16 since
    -- rendering will include a byte order mark. (RFC 2781, Sec. 3.3)
    enc X.UTF16BE = "utf-16"
    enc X.UTF16LE = "utf-16"


------------------------------------------------------------------------------
-- | Renders a template from the specified HeistState to a 'Builder'.  The
-- MIME type returned is based on the detected character encoding, and whether
-- the root template was an HTML or XML format template.  It will always be
-- @text/html@ or @text/xml@.  If a more specific MIME type is needed for a
-- particular XML application, it must be provided by the application.
renderTemplate :: Monad n
               => HeistState n
               -> ByteString
               -> n (Maybe (Builder, MIMEType))
renderTemplate ts name = evalHeistT tpl (X.TextNode "") ts
  where tpl = do mt <- evalWithHooksInternal name
                 case mt of
                    Nothing  -> return Nothing
                    Just doc -> return $ Just $ (X.render doc, mimeType doc)


------------------------------------------------------------------------------
-- | Renders a template with the specified arguments passed to it.  This is a
-- convenience function for the common pattern of calling renderTemplate after
-- using bindString, bindStrings, or bindSplice to set up the arguments to the
-- template.
renderWithArgs :: Monad n
               => [(Text, Text)]
               -> HeistState n
               -> ByteString
               -> n (Maybe (Builder, MIMEType))
renderWithArgs args ts = renderTemplate (bindStrings args ts)


------------------------------------------------------------------------------
-- Template loading
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Adds a path prefix to all the templates in the 'HeistState'.  If you
-- want to add multiple levels of directories, separate them with slashes as
-- in "foo/bar".  Using an empty string as a path prefix will leave the
-- 'HeistState' unchanged.
addTemplatePathPrefix :: ByteString -> HeistState n -> HeistState n
addTemplatePathPrefix dir ts
  | B.null dir = ts
  | otherwise  = ts { _templateMap = Map.fromList $
                                     map (\(x,y) -> (f x, y)) $
                                     Map.toList $
                                     _templateMap ts
                    }
  where
    f ps = ps++splitTemplatePath dir


