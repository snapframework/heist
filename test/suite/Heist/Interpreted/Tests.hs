{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Heist.Interpreted.Tests
  ( tests
  , quickRender
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Error
import           Control.Monad.State
import           Data.Aeson
import           Data.ByteString.Char8                (ByteString)
import qualified Data.ByteString.Char8                as B
import qualified Data.ByteString.Lazy.Char8           as L
import qualified Data.HashMap.Strict                  as Map
import           Data.Map.Syntax
import           Data.Maybe
import           Data.Monoid
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           System.IO.Unsafe
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit                           as H
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


------------------------------------------------------------------------------
import           Heist
import           Heist.Common
import           Heist.Internal.Types
import           Heist.Interpreted.Internal
import           Heist.Splices.Apply
import           Heist.Splices.Ignore
import           Heist.Splices.Json
import           Heist.Splices.Markdown
import           Heist.TestCommon
import qualified Text.XmlHtml                         as X
import qualified Text.XmlHtml.Cursor                  as X


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testProperty "heist/simpleBind"            simpleBindTest
        , testProperty "heist/simpleApply"           simpleApplyTest
        , testCase     "heist/templateAdd"           addTest
        , testCase     "heist/hasTemplate"           hasTemplateTest
        , testCase     "heist/getDoc"                getDocTest
        , testCase     "heist/load"                  loadTest
        , testCase     "heist/fsLoad"                fsLoadTest
        , testCase     "heist/renderNoName"          renderNoNameTest
        , testCase     "heist/doctype"               doctypeTest
        , testCase     "heist/attributeSubstitution" attrSubstTest
        , testCase     "heist/bindAttribute"         bindAttrTest
        , testCase     "heist/markdown"              markdownTest
        , testCase     "heist/pandoc"                pandocTest
        , testCase     "heist/pandoc_div"            pandocDivTest
        , testCase     "heist/title_expansion"       titleExpansion
        , testCase     "heist/textarea_expansion"    textareaExpansion
        , testCase     "heist/div_expansion"         divExpansion
        , testCase     "heist/bind_param"            bindParam
        , testCase     "heist/markdownText"          markdownTextTest
        , testCase     "heist/apply"                 applyTest
        , testCase     "heist/ignore"                ignoreTest
        , testCase     "heist/lookupTemplateContext" lookupTemplateTest
        , testCase     "heist/attrSpliceContext"     attrSpliceContext
        , testCase     "heist/json/values"           jsonValueTest
        , testCase     "heist/json/object"           jsonObjectTest
        , testCase     "heist/renderXML"             xmlNotHtmlTest
        ]


------------------------------------------------------------------------------
simpleBindTest :: Property
simpleBindTest = monadicIO $ forAllM arbitrary prop
  where
    prop :: Bind -> PropertyM IO ()
    prop bind = do
        let template = buildBindTemplate bind
        let result   = buildResult bind

        spliceResult <- run $ do
            hs <- loadEmpty defaultLoadTimeSplices mempty mempty mempty
            evalHeistT (runNodeList template)
                       (X.TextNode "") hs

        assert $ result == spliceResult


------------------------------------------------------------------------------
simpleApplyTest :: Property
simpleApplyTest = monadicIO $ forAllM arbitrary prop
  where
    prop :: Apply -> PropertyM IO ()
    prop apply = do
        let correct = calcCorrect apply
        result <- run $ calcResult apply
        assert $ correct == result


------------------------------------------------------------------------------
addTest :: IO ()
addTest = do
    es <- loadEmpty mempty mempty mempty mempty
    let hs = addTemplate "aoeu" [] Nothing es
    H.assertEqual "lookup test" (Just []) $
        fmap (X.docContent . dfDoc . fst) $
        lookupTemplate "aoeu" hs _templateMap


------------------------------------------------------------------------------
hasTemplateTest :: H.Assertion
hasTemplateTest = do
    ets <- loadIO "templates" mempty mempty mempty mempty
    let tm = either (error . unlines) _templateMap ets
    hs <- loadEmpty mempty mempty mempty mempty
    let hs's = setTemplates tm hs
    H.assertBool "hasTemplate hs's" (hasTemplate "index" hs's)


------------------------------------------------------------------------------
getDocTest :: H.Assertion
getDocTest = do
    d <- getDoc "bkteoar"
    H.assertBool "non-existent doc" $ isLeft d
    f <- getDoc "templates/index.tpl"
    H.assertBool "index doc" $ not $ isLeft f


------------------------------------------------------------------------------
loadTest :: H.Assertion
loadTest = do
    ets <- loadIO "templates" mempty mempty mempty mempty
    either (error "Error loading templates")
           (\ts -> do let tm = _templateMap ts
                      H.assertEqual "loadTest size" 41 $ Map.size tm
           ) ets


------------------------------------------------------------------------------
fsLoadTest :: H.Assertion
fsLoadTest = do
    ets <- loadIO "templates" mempty mempty mempty mempty
    let tm = either (error "Error loading templates") _templateMap ets
    es <- loadEmpty mempty mempty mempty mempty
    let hs = setTemplates tm es
    let f  = g hs

    f isNothing "abc/def/xyz"
    f isJust "a"
    f isJust "bar/a"
    f isJust "/bar/a"

  where
    g ts p n = H.assertBool ("loading template " ++ n) $ p $
               lookupTemplate (B.pack n) ts _templateMap

------------------------------------------------------------------------------
renderNoNameTest :: H.Assertion
renderNoNameTest = do
    ets <- loadT "templates" mempty mempty mempty mempty
    either (error "Error loading templates")
           (\ts -> do t <- renderTemplate ts ""
                      H.assertBool "renderNoName" $ isNothing t
           ) ets


------------------------------------------------------------------------------
doctypeTest :: H.Assertion
doctypeTest = do
    ets <- loadT "templates" mempty mempty mempty mempty
    let ts = either (error "Error loading templates") id ets
    Just (indexDoc, _) <- renderTemplate ts "index"
    H.assertEqual "index doctype test" indexRes $ toByteString $ indexDoc
    Just (_, _) <- renderTemplate ts "ioc"
    H.assertEqual "ioc doctype test" indexRes $ toByteString $ indexDoc
  where
    indexRes = B.concat
        [doctype
        ,"\n\n<html>\n<div id='pre_{att}_post'>\n/index\n</div>\n</html>\n"
        ]

------------------------------------------------------------------------------
attrSubstTest :: H.Assertion
attrSubstTest = do
    ets <- loadT "templates" mempty mempty mempty mempty
    let ts = either (error "Error loading templates") id ets
    check "attr subst 1" (bindSplices splices ts) out1
    check "attr subst 2" ts out2

  where
    splices = defaultLoadTimeSplices `mappend`
        ("foo" ## return [X.TextNode "meaning_of_everything"])

    check str ts expected = do
        Just (resDoc, _) <- renderTemplate ts "attrs"
        H.assertEqual str expected $ toByteString $ resDoc

    out1 = B.unlines
        ["<mytag flag>Empty attribute</mytag>"
        ,"<mytag flag='abc${bar}'>No ident capture</mytag>"
        ,"<div id='pre_meaning_of_everything_post'></div>"
        ]
    out2 = B.unlines
        ["<mytag flag>Empty attribute</mytag>"
        ,"<mytag flag='abc${bar}'>No ident capture</mytag>"
        ,"<div id='pre_${foo}_post'></div>"
        ]


------------------------------------------------------------------------------
bindAttrTest :: H.Assertion
bindAttrTest = do
    ets <- loadT "templates" mempty mempty mempty mempty
    let ts = either (error "Error loading templates") id ets
    check ts "<div id=\'zzzzz\'"

  where
    check ts str = do
        Just (resDoc, _) <- renderTemplate ts "bind-attrs"
        H.assertBool ("attr subst " ++ (show str)) $ not $ B.null $
            snd $ B.breakSubstring str $ toByteString $ resDoc
        H.assertBool ("attr subst bar") $ B.null $
            snd $ B.breakSubstring "${bar}" $ toByteString $ resDoc


------------------------------------------------------------------------------
markdownHtmlExpected :: ByteString
markdownHtmlExpected =
    "<div class='markdown'><p>This <em>is</em> a test.</p></div>"

------------------------------------------------------------------------------
-- | Markdown test on a file
markdownTest :: H.Assertion
markdownTest = renderTest "markdown" markdownHtmlExpected

-----------------------------------------------------------------------------

-- | Pandoc test on a file
pandocTest :: H.Assertion
pandocTest = renderTest "pandoc" pandocNoDivHtmlExpected

pandocDivTest :: H.Assertion
pandocDivTest = renderTest "pandocdiv" pandocDivHtmlExpected

pandocNoDivHtmlExpected :: ByteString
pandocNoDivHtmlExpected = "<p>This <em>is</em> a test.</p>"

pandocDivHtmlExpected :: ByteString
pandocDivHtmlExpected =
  "<div class='foo test' id='pandoc'><p>This <em>is</em> a test.</p></div>"
  -- Implementaton dependent. Class is prepended in current implementation,
  -- it will be first attribute

pandocTestSplices :: Splices (Splice IO)
pandocTestSplices = do
    "pandocnodiv" ## pandocSplice optsNoDiv
    "pandocdiv"   ## pandocSplice optsDiv
  where
    optsNoDiv = setPandocWrapDiv Nothing defaultPandocOptions
    optsDiv = setPandocWrapDiv (Just "test") defaultPandocOptions

------------------------------------------------------------------------------
jsonValueTest :: H.Assertion
jsonValueTest = do
    renderTest "json" jsonExpected1
    renderTest "json_snippet" jsonExpected2

  where
    jsonExpected1 = B.concat [ "<i>&lt;b&gt;ok&lt;/b&gt;</i><i>1</i>"
                             , "<i></i><i>false</i><i>foo</i>" ]
    jsonExpected2 = "<i><b>ok</b></i><i>1</i><i></i><i>false</i><i>foo</i>"



------------------------------------------------------------------------------
jsonObjectTest :: H.Assertion
jsonObjectTest = do
    renderTest "json_object" jsonExpected
  where
    jsonExpected = "<i>1</i><i><b>ok</b></i>12quuxquux1<b>ok</b>"


------------------------------------------------------------------------------
-- | Render a template and assert that it matches an expected result
renderTest  :: ByteString   -- ^ template name
            -> ByteString   -- ^ expected result
            -> H.Assertion
renderTest templateName expectedResult = do
    ets <- loadT "templates" mempty pandocTestSplices mempty mempty
    let ts = either (error "Error loading templates") id ets

    check ts expectedResult

  where
    bind txt = bindJson v
      where
        v :: Value
        v = fromJust $ decode txt

    check ts0 str = do
        let splices = do
                "json" ## bind "[\"<b>ok</b>\", 1, null, false, \"foo\"]"
                "jsonObject" ##
                       (bind $ mconcat [
                                 "{\"foo\": 1, \"bar\": \"<b>ok</b>\", "
                                , "\"baz\": { \"baz1\": 1, \"baz2\": 2 }, "
                                , "\"quux\": \"quux\" }"
                                ])
        let ts = bindSplices splices ts0
        Just (doc, _) <- renderTemplate ts templateName
        let result = B.filter (/= '\n') (toByteString doc)
        H.assertEqual ("Should match " ++ (show str)) str result


------------------------------------------------------------------------------
-- | Expansion of a bound name inside a title-tag
titleExpansion :: H.Assertion
titleExpansion = renderTest "title_expansion" expected
  where
    expected = "<title>foo</title>"


------------------------------------------------------------------------------
-- | Expansion of a bound name inside a textarea-tag
textareaExpansion :: H.Assertion
textareaExpansion = renderTest "textarea_expansion" expected
  where
    expected = B.concat
        [ "<textarea>foo</textarea>" ]


------------------------------------------------------------------------------
-- | Expansion of a bound name inside a div-tag
divExpansion :: H.Assertion
divExpansion = renderTest "div_expansion" expected
  where
    expected = "<div>foo</div>"


------------------------------------------------------------------------------
-- | Handling of <content> and bound parameters in a bound tag.
bindParam :: H.Assertion
bindParam = renderTest "bind_param" "<li>Hi there world</li>"


------------------------------------------------------------------------------
-- | Handling of <content> and bound parameters in a bound tag.
attrSpliceContext :: H.Assertion
attrSpliceContext = renderTest "attrsubtest2"
    "<a href='asdf'>link</a><a href='before$after'>foo</a>"


------------------------------------------------------------------------------
-- | Markdown test on supplied text
markdownTextTest :: H.Assertion
markdownTextTest = do
    hs <- loadEmpty mempty mempty mempty mempty
    result <- evalHeistT markdownSplice
                         (X.TextNode "This *is* a test.")
                         hs
    H.assertEqual "Markdown text" markdownHtmlExpected
      (B.filter (/= '\n') $ toByteString $
        X.render (X.HtmlDocument X.UTF8 Nothing result))


------------------------------------------------------------------------------
applyTest :: H.Assertion
applyTest = do
    es <- loadEmpty mempty mempty mempty mempty
    res <- evalHeistT applyImpl
        (X.Element "apply" [("template", "nonexistant")] []) es

    H.assertEqual "apply nothing" [] res


------------------------------------------------------------------------------
ignoreTest :: H.Assertion
ignoreTest = do
    es <- loadEmpty mempty mempty mempty mempty
    res <- evalHeistT ignoreImpl
        (X.Element "ignore" [("tag", "ignorable")]
          [X.TextNode "This should be ignored"]) es
    H.assertEqual "<ignore> tag" [] res


lookupTemplateTest :: IO ()
lookupTemplateTest = do
    hs <- loadHS "templates"
    let k = do
            modifyHS (\st -> st { _curContext = ["foo"] })
            getsHS $ (\hs' -> lookupTemplate "/user/menu" hs' _templateMap)
    res <- runHeistT k (X.TextNode "") hs
    H.assertBool "lookup context test" $ isJust $ fst res

------------------------------------------------------------------------------
xmlNotHtmlTest :: H.Assertion
xmlNotHtmlTest = renderTest "rss" expected where
  expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss><channel><link>http://www.devalot.com/</link></channel></rss>"

------------------------------------------------------------------------------
identStartChar :: [Char]
identStartChar = ['a'..'z']


------------------------------------------------------------------------------
identChar :: [Char]
identChar = '_' : identStartChar


------------------------------------------------------------------------------
textGen :: Gen [Char]
textGen = listOf $ elements ((replicate 5 ' ') ++ identStartChar)


------------------------------------------------------------------------------
limitedDepth :: Int -> Gen X.Node
limitedDepth 0 = liftM (X.TextNode . T.pack) textGen
limitedDepth n =
    oneof [ liftM (X.TextNode . T.pack) textGen
          , liftM3 X.Element arbitrary
                       (liftM (take 2) arbitrary)
                       (liftM (take 3) $ listOf $ limitedDepth (n - 1))
          ]


------------------------------------------------------------------------------
-- | Returns the number of unique insertion points in the tree.
-- If h = insertAt f n g", the following property holds:
-- insSize h == (insSize f) + (insSize g) - 1
insSize :: [X.Node] -> Int
insSize ns = 1 + (sum $ map nodeSize ns)
  where nodeSize (X.Element _ _ c) = 1 + (insSize c)
        nodeSize _                 = 1


------------------------------------------------------------------------------
insertAt :: [X.Node] -> Int -> [X.Node] -> [X.Node]
insertAt elems 0 ns = elems ++ ns
insertAt elems _ [] = elems
insertAt elems n list = maybe [] X.topNodes $
    evalState (processNode elems $ fromJust $ X.fromNodes list) n


------------------------------------------------------------------------------
move :: Insert ()
move = modify (\x -> x - 1)


------------------------------------------------------------------------------
processNode :: [X.Node] -> X.Cursor -> Insert (Maybe X.Cursor)
processNode elems loc =
    liftM2 mplus (move >> goDown loc) (move >> goRight loc)

  where
    goDown l =
        case X.current l of
          X.TextNode _    -> modify (+1) >> return Nothing
          X.Element _ _ _ -> doneCheck (X.insertManyFirstChild elems)
                                       X.firstChild
                                       l
          X.Comment _     -> return Nothing

    goRight = doneCheck (Just . X.insertManyRight elems) X.right

    doneCheck insertFunc next l = do
      s <- get
      if s == 0
        then return $ insertFunc l
        else maybe (return Nothing) (processNode elems) $ next l


------------------------------------------------------------------------------
newtype Name = Name { unName :: Text } deriving (Show)

instance Arbitrary Name where
  arbitrary = do
    x     <- elements identStartChar
    n     <- choose (4,10)
    rest  <- vectorOf n $ elements identChar
    return $ Name $ T.pack (x:rest)

instance Arbitrary X.Node where
  arbitrary = limitedDepth 3
  shrink (X.Element _ [] []) = []
  shrink (X.Element n [] (_:cs)) = [X.Element n [] cs]
  shrink (X.Element n (_:as) []) = [X.Element n as []]
  shrink (X.Element n as cs) = [X.Element n as (tail cs), X.Element n (tail as) cs]
  shrink _ = []

instance Arbitrary T.Text where
  arbitrary = liftM unName arbitrary

--
-- Code for inserting nodes into any point of a tree
--
type Insert a = State Int a


------------------------------------------------------------------------------
-- <bind> tests

-- Data type encapsulating the parameters for a bind operation
data Bind = Bind
    { _bindElemName :: Name
    , _bindChildren :: [X.Node]
    , _bindDoc :: [X.Node]
    , _bindPos :: Int
    , _bindRefPos :: Int
    } -- deriving (Show)


instance Arbitrary Bind where
  arbitrary = do
    name <- arbitrary
    kids <- liftM (take 3) arbitrary
    doc <- liftM (take 5) arbitrary
    let s = insSize doc
    loc <- choose (0, s - 1)
    loc2 <- choose (0, s - loc - 1)
    return $ Bind name kids doc loc loc2
  shrink (Bind e [c] (_:ds) p r) = [Bind e [c] ds p r]
  shrink (Bind e (_:cs) d p r) = [Bind e cs d p r]
  shrink _ = []


instance Show Bind where
  show b@(Bind e c d p r) = unlines
    [ "\n"
    , "Bind element name: " ++ (show e)
    , "Bind pos: " ++ (show p)
    , "Bind ref pos: " ++ (show r)
    , "Bind document:"
    , L.unpack $ L.concat $ map formatNode d
    , "Bind children:"
    , L.unpack $ L.concat $ map formatNode c
    , "Result:"
    , L.unpack $ L.concat $ map formatNode $ buildResult b
    , "Splice result:"
    , L.unpack $ L.concat $ map formatNode $ unsafePerformIO $ do
        hs <- loadEmpty mempty mempty mempty mempty
        evalHeistT (runNodeList $ buildBindTemplate b)
                          (X.TextNode "") hs
    , "Template:"
    , L.unpack $ L.concat $ map formatNode $ buildBindTemplate b
    ]
    where
      formatNode n = toLazyByteString $ X.render
                                      $ X.HtmlDocument X.UTF8 Nothing [n]

------------------------------------------------------------------------------
buildNode :: Text -> Text -> Bind -> X.Node
buildNode tag attr (Bind s c _ _ _) = X.Element tag [(attr, unName s)] c


------------------------------------------------------------------------------
buildBind :: Bind -> X.Node
buildBind = buildNode "bind" "tag"


------------------------------------------------------------------------------
empty :: Text -> X.Node
empty n = X.Element n [] []


------------------------------------------------------------------------------
buildBindTemplate :: Bind -> [X.Node]
buildBindTemplate s@(Bind n _ d b r) =
    insertAt [empty $ unName $ n] pos $ withBind
  where bind = [buildBind s]
        bindSize = insSize bind
        withBind = insertAt bind b d
        pos = b + bindSize - 1 + r


------------------------------------------------------------------------------
buildResult :: Bind -> [X.Node]
buildResult (Bind _ c d b r) = insertAt c (b + r) d


------------------------------------------------------------------------------
-- <apply> tests

data Apply = Apply
    { _applyName :: Name
    , _applyCaller :: [X.Node]
    , _applyCallee :: Template
    , _applyChildren :: [X.Node]
    , _applyPos :: Int
    } deriving (Show)


instance Arbitrary Apply where
    arbitrary = do
      name <- arbitrary
      kids <- liftM (take 3) $ listOf $ limitedDepth 2
      caller <- liftM (take 5) arbitrary
      callee <- liftM (take 1) $ listOf $ limitedDepth 3
      let s = insSize caller
      loc <- choose (0, s - 1)
      return $ Apply name caller callee kids loc


------------------------------------------------------------------------------
buildApplyCaller :: Apply -> [X.Node]
buildApplyCaller (Apply name caller _ kids pos) =
    insertAt [X.Element "apply" [("template", unName name)] kids] pos caller


------------------------------------------------------------------------------
calcCorrect :: Apply -> [X.Node]
calcCorrect (Apply _ caller callee _ pos) = insertAt callee pos caller


------------------------------------------------------------------------------
calcResult :: Apply -> IO [X.Node]
calcResult apply@(Apply name _ callee _ _) = do
    hs <- loadEmpty defaultLoadTimeSplices mempty mempty mempty
    let hs' = setTemplates (Map.singleton [T.encodeUtf8 $ unName name]
                           (DocumentFile (X.HtmlDocument X.UTF8 Nothing callee)
                                         Nothing)) hs
    evalHeistT (runNodeList $ buildApplyCaller apply)
        (X.TextNode "") hs'




{-
 - Convenience code for manual ghci experimentation
 -}

--html :: [Node] -> Node
--html c = X.Element "html" [] [hhead, body c]
--hhead :: Node
--hhead = X.Element "head" [] [title, X.Element "script" [] []]
--title :: Node
--title = X.Element "title" [] [X.Text "Test Page"]
--body :: [Node] -> Node
--body = X.Element "body" []
--
--para :: Int -> Node
--para n = X.Element "p" [] [X.Text $ B.pack $ "This is paragraph " ++ show n]
--para2 :: B.ByteString -> Node
--para2 c = X.Element "p" [] [X.Text c]
--para3 :: Node
--para3 = X.Element "p" [] [X.Text "AHA!"]
--
--foo :: Int -> [Node]
--foo n = insertAt [X.Element "NEW" [] []] n [html [para 1, para 2]]
--
--tdoc :: [Node]
--tdoc = [para 1, para 2, para 3, para 4]
--
--bindElem :: [Node] -> Int -> Int -> Bind
--bindElem = Bind (Name "mytag") [para2 "bound paragraph"]
--
--addBind :: Bind -> [Node] -> [Node]
--addBind b = insertAt [buildBind b] 0 . insertAt [empty $ unName $ _bindElemName b] 2
--
--prn :: Node -> IO ()
--prn = L.putStrLn . formatNode
--runTests :: IO ()
--runTests = defaultMain tests
