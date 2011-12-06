{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Text.Templating.Heist.Tests
  ( tests
  , quickRender
  ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Control.Monad.State
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           System.IO.Unsafe
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


------------------------------------------------------------------------------
import           Text.Templating.Heist
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Types
import           Text.Templating.Heist.Splices.Apply
import           Text.Templating.Heist.Splices.Ignore
import           Text.Templating.Heist.Splices.Markdown
import qualified Text.XmlHtml        as X
import qualified Text.XmlHtml.Cursor as X


------------------------------------------------------------------------------
tests :: [Test]
tests = [ testProperty "heist/simpleBind"            simpleBindTest
        , testProperty "heist/simpleApply"           simpleApplyTest
        , testCase     "heist/stateMonoid"           monoidTest
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
        , testCase     "heist/title_expansion"       titleExpansion
        , testCase     "heist/textarea_expansion"    textareaExpansion
        , testCase     "heist/div_expansion"         divExpansion
        , testCase     "heist/bind_param"            bindParam
        , testCase     "heist/markdownText"          markdownTextTest
        , testCase     "heist/apply"                 applyTest
        , testCase     "heist/ignore"                ignoreTest
        , testCase     "heist/lookupTemplateContext" lookupTemplateTest
        , testCase     "heist/attrSpliceContext"     attrSpliceContext
        ]


------------------------------------------------------------------------------
simpleBindTest :: Property
simpleBindTest = monadicIO $ forAllM arbitrary prop
  where
    prop :: Bind -> PropertyM IO ()
    prop bind = do
        let template = buildBindTemplate bind
        let result   = buildResult bind

        spliceResult <- run $ evalTemplateMonad (runNodeList template)
                                                (X.TextNode "")
                                                emptyTemplateState
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
monoidTest :: IO ()
monoidTest = do
    H.assertBool "left monoid identity" $ mempty `mappend` es == es
    H.assertBool "right monoid identity" $ es `mappend` mempty == es
  where es = emptyTemplateState :: TemplateState IO


------------------------------------------------------------------------------
addTest :: IO ()
addTest = do
    H.assertEqual "lookup test" (Just []) $
        fmap (X.docContent . dfDoc . fst) $ lookupTemplate "aoeu" ts

    H.assertEqual "splice touched" 0 $ Map.size (_spliceMap ts)

  where
    ts = addTemplate "aoeu" [] Nothing (mempty::TemplateState IO)


------------------------------------------------------------------------------
hasTemplateTest :: H.Assertion
hasTemplateTest = do
    ets <- loadT "templates"
    let tm = either (error "Error loading templates") _templateMap ets
    let ts = setTemplates tm emptyTemplateState :: TemplateState IO
    H.assertBool "hasTemplate ts" (hasTemplate "index" ts)


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
    ets <- loadT "templates"
    either (error "Error loading templates")
           (\ts -> do let tm = _templateMap ts
                      H.assertBool "loadTest size" $ Map.size tm == 23
           ) ets


------------------------------------------------------------------------------
fsLoadTest :: H.Assertion
fsLoadTest = do
    ets <- loadT "templates"
    let tm = either (error "Error loading templates") _templateMap ets
    let ts = setTemplates tm emptyTemplateState :: TemplateState IO
    let f  = g ts

    f isNothing "abc/def/xyz"
    f isJust "a"
    f isJust "bar/a"
    f isJust "/bar/a"

  where
    g ts p n = H.assertBool ("loading template " ++ n) $ p $
               lookupTemplate (B.pack n) ts

------------------------------------------------------------------------------
renderNoNameTest :: H.Assertion
renderNoNameTest = do
    ets <- loadT "templates"
    either (error "Error loading templates")
           (\ts -> do t <- renderTemplate ts ""
                      H.assertBool "renderNoName" $ isNothing t
           ) ets


------------------------------------------------------------------------------
doctypeTest :: H.Assertion
doctypeTest = do
    ets <- loadT "templates"
    let ts = either (error "Error loading templates") id ets
    Just (indexDoc, indexMIME) <- renderTemplate ts "index"
    H.assertBool "doctype test index" $ isJust $ X.docType $
        fromRight $ (X.parseHTML "index") $ toByteString $ indexDoc
    Just (iocDoc, iocMIME) <- renderTemplate ts "ioc"
    H.assertBool "doctype test ioc" $ isJust $ X.docType $
        fromRight $ (X.parseHTML "index") $ toByteString $ iocDoc
  where fromRight (Right x) = x
        fromRight (Left  s) = error s

------------------------------------------------------------------------------
attrSubstTest :: H.Assertion
attrSubstTest = do
    ets <- loadT "templates"
    let ts = either (error "Error loading templates") id ets
    check (setTs "meaning_of_everything" ts) "pre_meaning_of_everything_post"
    check ts "pre__post"

  where
    setTs val = bindSplice "foo" (return [X.TextNode val])
    check ts str = do
        Just (resDoc, resMIME) <- renderTemplate ts "attrs"
        H.assertBool ("attr subst " ++ (show str)) $ not $ B.null $
            snd $ B.breakSubstring str $ toByteString $ resDoc
        H.assertBool ("attr subst foo") $ not $ B.null $
            snd $ B.breakSubstring "${foo}" $ toByteString $ resDoc


------------------------------------------------------------------------------
bindAttrTest :: H.Assertion
bindAttrTest = do
    ets <- loadT "templates"
    let ts = either (error "Error loading templates") id ets
    check ts "<div id=\'zzzzz\'"

  where
    check ts str = do
        Just (resDoc, resMIME) <- renderTemplate ts "bind-attrs"
        H.assertBool ("attr subst " ++ (show str)) $ not $ B.null $
            snd $ B.breakSubstring str $ toByteString $ resDoc
        H.assertBool ("attr subst bar") $ B.null $
            snd $ B.breakSubstring "${bar}" $ toByteString $ resDoc


------------------------------------------------------------------------------
htmlExpected :: ByteString
htmlExpected = "<div class=\'markdown\'><p>This <em>is</em> a test.</p></div>"


------------------------------------------------------------------------------
-- | Markdown test on a file
markdownTest :: H.Assertion
markdownTest = renderTest "markdown" htmlExpected


-- | Render a template and assert that it matches an expected result
renderTest  :: ByteString   -- ^ template name
            -> ByteString   -- ^ expected result
            -> H.Assertion
renderTest templateName expectedResult = do
    ets <- loadT "templates"
    let ts = either (error "Error loading templates") id ets

    check ts expectedResult

  where
    check ts str = do
        Just (doc, _) <- renderTemplate ts templateName
        let result = B.filter (/= '\n') (toByteString doc)
        H.assertEqual ("Should match " ++ (show str)) str result


------------------------------------------------------------------------------
-- | Expansion of a bound name inside a title-tag
titleExpansion :: H.Assertion
titleExpansion = renderTest "title_expansion" "<title>foo</title>"


------------------------------------------------------------------------------
-- | Expansion of a bound name inside a textarea-tag
textareaExpansion :: H.Assertion
textareaExpansion = renderTest "textarea_expansion" "<textarea>foo</textarea>"


------------------------------------------------------------------------------
-- | Expansion of a bound name inside a div-tag
divExpansion :: H.Assertion
divExpansion = renderTest "div_expansion" "<div>foo</div>"


------------------------------------------------------------------------------
-- | Handling of <content> and bound parameters in a bonud tag.
bindParam :: H.Assertion
bindParam = renderTest "bind_param" "<li>Hi there world</li>"


------------------------------------------------------------------------------
-- | Handling of <content> and bound parameters in a bonud tag.
attrSpliceContext :: H.Assertion
attrSpliceContext = renderTest "attrsubtest2" "<a href='asdf'>link</a>"


------------------------------------------------------------------------------
-- | Markdown test on supplied text
markdownTextTest :: H.Assertion
markdownTextTest = do
    result <- evalTemplateMonad markdownSplice
                                (X.TextNode "This *is* a test.")
                                emptyTemplateState
    H.assertEqual "Markdown text" htmlExpected 
      (B.filter (/= '\n') $ toByteString $
        X.render (X.HtmlDocument X.UTF8 Nothing result))


------------------------------------------------------------------------------
applyTest :: H.Assertion
applyTest = do
    let es = emptyTemplateState :: TemplateState IO
    res <- evalTemplateMonad applyImpl
        (X.Element "apply" [("template", "nonexistant")] []) es

    H.assertEqual "apply nothing" [] res


------------------------------------------------------------------------------
ignoreTest :: H.Assertion
ignoreTest = do
    let es = emptyTemplateState :: TemplateState IO
    res <- evalTemplateMonad ignoreImpl
        (X.Element "ignore" [("tag", "ignorable")] 
          [X.TextNode "This should be ignored"]) es
    H.assertEqual "<ignore> tag" [] res


--localTSTest :: H.Assertion
--localTSTest = do
--    let es = emptyTemplateState :: TemplateState IO

lookupTemplateTest = do
    ts <- loadTS "templates"
    let k = do
            setContext ["foo"]
            getsTS $ lookupTemplate "/user/menu"
    res <- runTemplateMonad k (X.TextNode "") ts
    H.assertBool "lookup context test" $ isJust $ fst res


------------------------------------------------------------------------------
-- Utility functions

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


------------------------------------------------------------------------------
loadT :: String -> IO (Either String (TemplateState IO))
loadT s = loadTemplates s emptyTemplateState


------------------------------------------------------------------------------
loadTS :: FilePath -> IO (TemplateState IO)
loadTS baseDir = do
    etm <- loadTemplates baseDir emptyTemplateState
    return $ either error id etm


testTemplate tname = do
    ts <- loadTS "templates"
    Just (resDoc, _) <- renderTemplate ts tname
    return $ toByteString resDoc


testTemplateEval tname = do
    ts <- loadTS "templates"
    evalTemplateMonad (evalWithHooks tname) (X.TextNode "") ts


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
  where nodeSize (X.TextNode _)    = 1
        nodeSize (X.Element _ _ c) = 1 + (insSize c)


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

    goRight = doneCheck (Just . X.insertManyRight elems) X.right

    doneCheck insertFunc next l = do
      s <- get
      if s == 0
        then return $ insertFunc l
        else maybe (return Nothing) (processNode elems) $ next l


------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.  (Old convenience code.)
quickRender :: FilePath -> ByteString -> IO (Maybe ByteString)
quickRender baseDir name = do
    ts  <- loadTS baseDir
    res <- renderTemplate ts name
    return (fmap (toByteString . fst) res)


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
  shrink (X.TextNode _) = []
  shrink (X.Element _ [] []) = []
  shrink (X.Element n [] (_:cs)) = [X.Element n [] cs]
  shrink (X.Element n (_:as) []) = [X.Element n as []]
  shrink (X.Element n as cs) = [X.Element n as (tail cs), X.Element n (tail as) cs]

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
    , L.unpack $ L.concat $ map formatNode $ unsafePerformIO $
        evalTemplateMonad (runNodeList $ buildBindTemplate b)
                          (X.TextNode "") emptyTemplateState
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
calcResult :: (MonadIO m) => Apply -> m [X.Node]
calcResult apply@(Apply name _ callee _ _) =
    evalTemplateMonad (runNodeList $ buildApplyCaller apply)
        (X.TextNode "") ts

  where ts = setTemplates (Map.singleton [T.encodeUtf8 $ unName name]
                          (DocumentFile (X.HtmlDocument X.UTF8 Nothing callee)
                                        Nothing))
                          emptyTemplateState



{-
-- The beginning of some future tests for hook functions.

p :: ByteString -> Node
p t = X.Element "p" [] [X.Text t]

hookG :: Monad m => ByteString -> Template -> m Template
hookG str t = return $ (p str) : t

onLoad = hookG "Inserted on load"
preRun = hookG "Inserted on preRun"
postRun = hookG "Inserted on postRun"

ts :: IO (Either String (TemplateState IO))
ts = loadTemplates "test/templates" $
    foldr ($) (emptyTemplateState ".")
    [setOnLoadHook onLoad
    ,setPreRunHook preRun
    ,setPostRunHook postRun
    ]

r name etm = do
    let ts = either (error "Danger Will Robinson!") id etm
    ns <- runNodeList ts name
    return $ (Just . formatList') =<< ns
-}


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

