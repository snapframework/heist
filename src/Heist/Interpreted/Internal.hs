{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Heist.Interpreted.Internal where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Attoparsec.Text          as AP
import           Data.ByteString               (ByteString)
import qualified Data.HashMap.Strict           as Map
import qualified Data.HeterogeneousEnvironment as HE
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Text.XmlHtml                  as X
------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Internal.Types.HeistState
------------------------------------------------------------------------------


type Splice n = HeistT n n Template


------------------------------------------------------------------------------
-- HeistState functions
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Binds a new splice declaration to a tag name within a 'HeistState'.
bindSplice :: Text            -- ^ tag name
           -> Splice n        -- ^ splice action
           -> HeistState n    -- ^ source state
           -> HeistState n
bindSplice n v hs = hs {_spliceMap = Map.insert n v (_spliceMap hs)}


------------------------------------------------------------------------------
-- | Binds a set of new splice declarations within a 'HeistState'.
bindSplices :: Splices (Splice n) -- ^ splices to bind
            -> HeistState n       -- ^ start state
            -> HeistState n
bindSplices ss hs =
    hs { _spliceMap = applySpliceMap hs _spliceMap ss }


------------------------------------------------------------------------------
-- | Converts 'Text' to a splice returning a single 'TextNode'.
textSplice :: Monad m => Text -> HeistT n m Template
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
                => Splices (Splice n)
                -- ^ List of splices to bind before running the param nodes.
                -> Splice n
                -- ^ Returns the passed in view.
runChildrenWith splices = localHS (bindSplices splices) runChildren


------------------------------------------------------------------------------
-- | Wrapper around runChildrenWith that applies a transformation function to
-- the second item in each of the tuples before calling runChildrenWith.
runChildrenWithTrans :: (Monad n)
          => (b -> Splice n)
          -- ^ Splice generating function
          -> Splices b
          -- ^ List of tuples to be bound
          -> Splice n
runChildrenWithTrans f = runChildrenWith . mapV f


------------------------------------------------------------------------------
-- | Like runChildrenWith but using constant templates rather than dynamic
-- splices.
runChildrenWithTemplates :: (Monad n) => Splices Template -> Splice n
runChildrenWithTemplates = runChildrenWithTrans return


------------------------------------------------------------------------------
-- | Like runChildrenWith but using literal text rather than dynamic splices.
runChildrenWithText :: (Monad n) => Splices Text -> Splice n
runChildrenWithText = runChildrenWithTrans textSplice


------------------------------------------------------------------------------
-- | Convenience function for looking up a splice.
lookupSplice :: Text
             -> HeistState n
             -> Maybe (Splice n)
lookupSplice nm hs = Map.lookup nm $ _spliceMap hs
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
stopRecursion = modifyHS (\st -> st { _recurse = False })


------------------------------------------------------------------------------
-- | Performs splice processing on a single node.
runNode :: Monad n => X.Node -> Splice n
runNode (X.Element nm at ch) = do
    newAtts <- runAttributes at
    let n = X.Element nm newAtts ch
    s <- liftM (lookupSplice nm) getHS
    maybe (runKids newAtts) (recurseSplice n) s
  where
    runKids newAtts = do
        newKids <- runNodeList ch
        return [X.Element nm newAtts newKids]
runNode n                    = return [n]


------------------------------------------------------------------------------
-- | Performs splice processing on a list of attributes.  This is useful in
-- situations where you need to stop recursion, but still run splice
-- processing on the node's attributes.
runAttributes :: Monad n => [(Text, Text)] -> HeistT n n [(Text, Text)]
runAttributes attrs = (return . concat) =<< mapM runAttrSplice attrs


------------------------------------------------------------------------------
-- | Runs the attribute splice if it exists, otherwise it does inline $()
-- substitution.
runAttrSplice :: (Monad n) => (Text, Text) -> HeistT n n [(Text, Text)]
runAttrSplice a@(k,v) = do
    splice <- getsHS (Map.lookup k . _attrSpliceMap)
    maybe (liftM (:[]) $ attSubst a)
          (lift . flip evalStateT HE.empty . unRT . ($v)) splice


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
    cvt (Ident x)   =
        localParamNode (const $ X.Element x [] []) $ getAttributeSplice x


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
    hs <- getHS
    let noSplice = return $ T.concat ["${", name, "}"]
        s = lookupSplice name hs
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
    hs <- getHS
    if _recurse hs
        then if _recursionDepth hs < mAX_RECURSION_DEPTH
               then do modRecursionDepth (+1)
                       res <- runNodeList result
                       restoreHS hs
                       return res
               else return result `orError` err
        else do modifyHS (\st -> st { _recurse = True })
                return result
  where
    err = unwords
        ["Recursion limit reached in node"
        ,"<"++(T.unpack $ X.elementTag node)++">.  You"
        ,"probably have infinite splice recursion!"
        ]



------------------------------------------------------------------------------
-- | Looks up a template name runs a 'HeistT' computation on it.
lookupAndRun :: Monad m
             => ByteString
             -> ((DocumentFile, TPath) -> HeistT n m (Maybe a))
             -> HeistT n m (Maybe a)
lookupAndRun name k = do
    hs <- getHS
    let mt = lookupTemplate name hs _templateMap
    let curPath = join $ fmap (dfFile . fst) mt
    modifyHS (setCurTemplateFile curPath)
    maybe (return Nothing) k mt


------------------------------------------------------------------------------
-- | Looks up a template name evaluates it by calling runNodeList.
evalTemplate :: Monad n
             => ByteString
             -> HeistT n n (Maybe Template)
evalTemplate name = lookupAndRun name
    (\(t,ctx) -> localHS (\hs -> hs {_curContext = ctx})
                         (liftM Just $ runNodeList $ X.docContent $ dfDoc t))


------------------------------------------------------------------------------
-- | Sets the document type of a 'X.Document' based on the 'HeistT'
-- value.
fixDocType :: Monad m => X.Document -> HeistT n m X.Document
fixDocType d = do
    dts <- getsHS _doctypes
    return $ d { X.docType = listToMaybe dts }


------------------------------------------------------------------------------
-- | Runs a template and sets the doctype properly.  This is the right thing
-- to do if we are starting at the top level.
evalWithDoctypes :: Monad n
                 => ByteString
                 -> HeistT n n (Maybe X.Document)
evalWithDoctypes name = lookupAndRun name $ \(t,ctx) -> do
    addDoctype $ maybeToList $ X.docType $ dfDoc t
    hs <- getHS
    let nodes = X.docContent $ dfDoc t
    putHS (hs {_curContext = ctx})
    newNodes <- runNodeList nodes
    restoreHS hs
    newDoc   <- fixDocType $ (dfDoc t) { X.docContent = newNodes }
    return (Just newDoc)


------------------------------------------------------------------------------
-- | Binds a list of constant string splices.
bindStrings :: Monad n
            => Splices Text
            -> HeistState n
            -> HeistState n
bindStrings splices = bindSplices (mapV textSplice splices)


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
             -> Splices (Splice n) -- ^ Splices to call the template with
             -> HeistT n n Template
callTemplate name splices = do
    modifyHS $ bindSplices splices
    liftM (maybe [] id) $ evalTemplate name


------------------------------------------------------------------------------
-- | Like callTemplate except the splices being bound are constant text
-- splices.
callTemplateWithText :: Monad n
                     => ByteString     -- ^ The name of the template
                     -> Splices Text -- ^ Splices to call the template with
                     -> HeistT n n Template
callTemplateWithText name splices = callTemplate name $ mapV textSplice splices


------------------------------------------------------------------------------
-- | Renders a template from the specified HeistState to a 'Builder'.  The
-- MIME type returned is based on the detected character encoding, and whether
-- the root template was an HTML or XML format template.  It will always be
-- @text/html@ or @text/xml@.  If a more specific MIME type is needed for a
-- particular XML application, it must be provided by the application.
--
-- Note that template names should not include the .tpl extension:
--
-- @renderTemplate hs "index"@
renderTemplate :: Monad n
               => HeistState n
               -> ByteString
               -> n (Maybe (Builder, MIMEType))
renderTemplate hs name = renderTemplateWithEncoding hs name X.UTF8


------------------------------------------------------------------------------
-- | A generalized version of 'renderTemplate' that allows overriding the
-- render encoding, for example to allow encoding to ISO-8859-1 with
-- html entity escape sequences
renderTemplateWithEncoding :: Monad n
                           => HeistState n
                           -> ByteString
                           -> X.Encoding
                           -> n (Maybe (Builder, MIMEType))
renderTemplateWithEncoding hs name enc = evalHeistT tpl (X.TextNode "") hs
  where tpl = do mt <- evalWithDoctypes name
                 case mt of
                     Nothing  -> return Nothing
                     Just doc -> let doc' = doc { X.docEncoding = enc }
                                 in  return $ Just (X.render doc', mimeType doc')


------------------------------------------------------------------------------
-- | Renders a template with the specified arguments passed to it.  This is a
-- convenience function for the common pattern of calling renderTemplate after
-- using bindString, bindStrings, or bindSplice to set up the arguments to the
-- template.
renderWithArgs :: Monad n
               => Splices Text
               -> HeistState n
               -> ByteString
               -> n (Maybe (Builder, MIMEType))
renderWithArgs args hs = renderTemplate (bindStrings args hs)


