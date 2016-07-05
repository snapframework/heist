{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Heist.Splices.Json (
  bindJson
) where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8       as S
import qualified Data.ByteString.Lazy.Char8  as L
import qualified Data.HashMap.Strict         as Map
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Vector                 as V
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as B
import           Text.Blaze.Renderer.XmlHtml
import           Text.XmlHtml
------------------------------------------------------------------------------
import           Heist.Interpreted.Internal
import           Heist.Internal.Types.HeistState
------------------------------------------------------------------------------

                                 ------------
                                 -- public --
                                 ------------

------------------------------------------------------------------------------
-- | This splice binds convenience tags for the given JSON (or
-- JSON-convertible) value and runs the tag's child nodes using the new
-- bindings.
--
-- /Tags bound when you pass in an object/
--
-- Tags bound for an object looking like this:
--
-- > { "k_1": v_1, ..., "k_N": v_N }
--
-- @\<value:{k_i}\>@    -- treats v_i as text
-- @\<snippet:{k_i}\>@  -- treats v_i as HTML
-- @\<with:{k_i}\>@     -- explodes v_i and runs its children
--
-- @\<value var=\"foo.bar.baz\"\/>@ -- walks the JSON tree to find
-- \"foo.bar.baz\", and interprets it as a string
-- @\<snippet var=\"foo.bar.baz\"\/\>@
-- @\<with var=\"foo.bar.baz\"\>...\<with\>@
--
-- /Tags bound when you pass in anything else/
--
-- @\<value\/\>@    --  the given JSON value, as a string
-- @\<snippet\/\>@  --  the given JSON value, parsed and spliced in as HTML
--
bindJson :: (ToJSON a, Monad n) => a -> Splice n
bindJson = runReaderT explodeTag . toJSON


                                 -------------
                                 -- private --
                                 -------------

------------------------------------------------------------------------------
errorMessage :: String -> [Node]
errorMessage s = renderHtmlNodes $
                     B.strong ! B.customAttribute "class" "error" $
                     B.toHtml s


------------------------------------------------------------------------------
type JsonMonad n m a = ReaderT Value (HeistT n m) a


------------------------------------------------------------------------------
withValue :: (Monad m) => Value -> JsonMonad n m a -> HeistT n m a
withValue = flip runReaderT


------------------------------------------------------------------------------
boolToText :: Bool -> Text
boolToText b = if b then "true" else "false"


------------------------------------------------------------------------------
numToText :: ToJSON a => a -> Text
numToText = T.decodeUtf8 . S.concat . L.toChunks . encode


------------------------------------------------------------------------------
findExpr :: Text -> Value -> Maybe Value
findExpr t = go (T.split (=='.') t)
  where
    go [] !value     = Just value
    go (x:xs) !value = findIn value >>= go xs
      where
        findIn (Object obj) = Map.lookup x obj
        findIn (Array arr)  = tryReadIndex >>= \i -> arr V.!? i
        findIn _            = Nothing

        tryReadIndex = fmap fst . listToMaybe . reads . T.unpack $ x


------------------------------------------------------------------------------
asHtml :: Monad m => Text -> m [Node]
asHtml t =
    case (parseHTML "" $ T.encodeUtf8 t) of
      Left e  -> return $ errorMessage $
                 "Template error turning JSON into HTML: " ++ e
      Right d -> return $! docContent d


------------------------------------------------------------------------------
snippetTag :: Monad m => JsonMonad n m [Node]
snippetTag = ask >>= snip
  where
    txt t = lift $ asHtml t

    snip Null       = txt ""
    snip (Bool b)   = txt $ boolToText b
    snip (Number n) = txt $ numToText n
    snip (String t) = txt t
    snip _          = lift $ do
        node <- getParamNode
        return $ errorMessage $ concat [
                     "error processing tag <"
                   , T.unpack $ fromMaybe "???" $ tagName node
                   , ">: can't interpret JSON arrays or objects as HTML."
                   ]


------------------------------------------------------------------------------
valueTag :: Monad m => JsonMonad n m [Node]
valueTag = ask >>= go
  where
    go Null       = txt ""
    go (Bool b)   = txt $ boolToText b
    go (Number n) = txt $ numToText n
    go (String t) = txt t
    go _          = lift $ do
        node <- getParamNode
        return $ errorMessage $ concat [
                     "error processing tag <"
                   , T.unpack $ fromMaybe "???" $ tagName node
                   , ">: can't interpret JSON arrays or objects as text."
                   ]


    txt t = return [TextNode t]


------------------------------------------------------------------------------
explodeTag :: forall n. (Monad n) => JsonMonad n n [Node]
explodeTag = ask >>= go
  where
    --------------------------------------------------------------------------
    go Null       = goText ""
    go (Bool b)   = goText $ boolToText b
    go (Number n) = goText $ numToText n
    go (String t) = goText t
    go (Array a)  = goArray a
    go (Object o) = goObject o

    --------------------------------------------------------------------------
    goText t = lift $ runChildrenWith $ do
        "value"   ## return [TextNode t]
        "snippet" ## asHtml t

    --------------------------------------------------------------------------
    goArray :: V.Vector Value -> JsonMonad n n [Node]
    goArray a = do
        lift stopRecursion
        dl <- V.foldM f id a
        return $! dl []
      where
        f dl jsonValue = do
            tags <- go jsonValue
            return $! dl . (tags ++)

    --------------------------------------------------------------------------
    -- search the param node for attribute \"var=expr\", search the given JSON
    -- object for the expression, and if it's found run the JsonMonad action m
    -- using the restricted JSON object.
    varAttrTag :: Value -> (JsonMonad n n [Node]) -> Splice n
    varAttrTag v m = do
        node <- getParamNode
        maybe (noVar node) (hasVar node) $ getAttribute "var" node
      where
        noVar node = return $ errorMessage $
                     concat [ "expression error: no var attribute in <"
                            , T.unpack $ fromMaybe "???" $ tagName node
                            , "> tag"
                            ]

        hasVar node expr = maybe (return $ errorMessage $
                                  concat [
                                    "expression error: can't find \""
                                  , T.unpack expr
                                  , "\" in JSON object (<"
                                  , T.unpack $ fromMaybe "???" $ tagName node
                                  , "> tag)"
                                  ])
                                 (runReaderT m)
                                 (findExpr expr v)

    --------------------------------------------------------------------------
    genericBindings :: JsonMonad n n (Splices (Splice n))
    genericBindings = ask >>= \v -> return $ do
        "with"     ## varAttrTag v explodeTag
        "snippet"  ## varAttrTag v snippetTag
        "value"    ## varAttrTag v valueTag


    --------------------------------------------------------------------------
    goObject obj = do
        start <- genericBindings
        let bindings = Map.foldlWithKey' bindKvp start obj
        lift $ runChildrenWith bindings

    --------------------------------------------------------------------------
    bindKvp bindings k v =
        let newBindings = do
                T.append "with:" k    ## withValue v explodeTag
                T.append "snippet:" k ## withValue v snippetTag
                T.append "value:" k   ## withValue v valueTag
        in  bindings >> newBindings
