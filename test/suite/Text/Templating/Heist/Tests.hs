{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Text.Templating.Heist.Tests
  ( tests
  , quickRender
  ) where

import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad.State

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid

import           System.IO.Unsafe

import           Text.Templating.Heist
import           Text.Templating.Heist.Internal
import           Text.Templating.Heist.Splices.Apply
import           Text.XML.Expat.Cursor
import           Text.XML.Expat.Format
import qualified Text.XML.Expat.Tree as X

tests :: [Test]
tests = [ testProperty "simpleBindTest" $ monadicIO $ forAllM arbitrary prop_simpleBindTest
        , testProperty "simpleApplyTest" $ monadicIO $ forAllM arbitrary prop_simpleApplyTest
        , testCase "stateMonoidTest" monoidTest
        , testCase "templateAddTest" addTest
        , testCase "getDocTest" getDocTest
        , testCase "loadTest" loadTest
        , testCase "fsLoadTest" fsLoadTest
        , testCase "renderNoNameTest" renderNoNameTest
        , testCase "doctypeTest" doctypeTest
        , testCase "attributeSubstitution" attrSubst
        , testCase "applyTest" applyTest
        ]

applyTest :: H.Assertion
applyTest = do
    let es = emptyTemplateState :: TemplateState IO
    res <- runTemplateMonad es
        (X.Element "apply" [("template", "nonexistant")] []) applyImpl
    H.assertEqual "apply nothing" res []
    
monoidTest :: IO ()
monoidTest = do
  H.assertBool "left monoid identity" $ mempty `mappend` es == es
  H.assertBool "right monoid identity" $ es `mappend` mempty == es
  where es = emptyTemplateState :: TemplateState IO

addTest :: IO ()
addTest = do
  H.assertBool "lookup test" $ Just [] == (fmap (_itNodes . fst) $ lookupTemplate "aoeu" ts)
  H.assertBool "splice touched" $ Map.size (_spliceMap ts) == 0
  where ts = addTemplate "aoeu" (InternalTemplate Nothing []) (mempty::TemplateState IO)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


loadT :: String -> IO (Either String (TemplateState IO))
loadT s = loadTemplates s emptyTemplateState

loadTest :: H.Assertion
loadTest = do
  ets <- loadT "templates"
  either (error "Error loading templates")
         (\ts -> do let tm = _templateMap ts
                    H.assertBool "loadTest size" $ Map.size tm == 15
         ) ets

renderNoNameTest :: H.Assertion
renderNoNameTest = do
  ets <- loadT "templates"
  either (error "Error loading templates")
         (\ts -> do t <- renderTemplate ts ""
                    H.assertBool "renderNoName" $ t == Nothing
         ) ets

getDocTest :: H.Assertion
getDocTest = do
  d <- getDoc "bkteoar"
  H.assertBool "non-existent doc" $ isLeft d
  f <- getDoc "templates/index.tpl"
  H.assertBool "index doc" $ not $ isLeft f

fsLoadTest :: H.Assertion
fsLoadTest = do
  ets <- loadT "templates"
  let tm = either (error "Error loading templates") _templateMap ets
  let ts = setTemplates tm emptyTemplateState :: TemplateState IO
      f p n = H.assertBool ("loading template "++n) $ p $ lookupTemplate (B.pack n) ts
  f isNothing "abc/def/xyz"
  f isJust "a"
  f isJust "bar/a"
  f isJust "/bar/a"

doctypeTest :: H.Assertion
doctypeTest = do
    ets <- loadT "templates"
    let ts = either (error "Error loading templates") id ets
    index <- renderTemplate ts "index"
    H.assertBool "doctype test index" $ hasDoctype $ fromJust index
    ioc <- renderTemplate ts "ioc"
    H.assertBool "doctype test ioc" $ hasDoctype $ fromJust ioc

attrSubst :: H.Assertion
attrSubst = do
    ets <- loadT "templates"
    let ts = either (error "Error loading templates") id ets
    check (setTs "meaning_of_everything" ts) "pre_meaning_of_everything_post"
    check ts "pre__post"
  where
    setTs val = bindSplice "foo" (return [X.Text val])
    check ts str = do
        res <- renderTemplate ts "attrs"
        H.assertBool ("attr subst "++(show str)) $
            not $ B.null $ snd $ B.breakSubstring str $ fromJust res
        H.assertBool ("attr subst foo") $
            not $ B.null $ snd $ B.breakSubstring "$(foo)" $ fromJust res

-- dotdotTest :: H.Assertion
-- dotdotTest = do
--   ets <- loadT "templates"
--   let tm = either (error "Error loading templates") _templateMap ets
--   let ts = setTemplates tm emptyTemplateState :: TemplateState IO
--       f p n = H.assertBool ("loading template "++n) $ p $ lookupTemplate (B.pack n) ts

identStartChar :: [Char]
identStartChar = ['a'..'z']
identChar :: [Char]
identChar = '_' : identStartChar

newtype Name = Name { unName :: B.ByteString } deriving (Show)

instance Arbitrary Name where
  arbitrary = do
    first <- elements identStartChar
    n <- choose (4,10)
    rest <- vectorOf n $ elements identChar
    return $ Name $ B.pack $ first : rest

instance Arbitrary Node where
  arbitrary = limitedDepth 3
  shrink (X.Text _) = []
  shrink (X.Element _ [] []) = []
  shrink (X.Element n [] (_:cs)) = [X.Element n [] cs]
  shrink (X.Element n (_:as) []) = [X.Element n as []]
  shrink (X.Element n as cs) = [X.Element n as (tail cs), X.Element n (tail as) cs]

textGen :: Gen [Char]
textGen = listOf $ elements ((replicate 5 ' ') ++ identStartChar)

limitedDepth :: Int -> Gen Node
limitedDepth 0 = liftM (X.Text . B.pack) textGen
limitedDepth n = oneof [ liftM (X.Text . B.pack) textGen
                       , liftM3 X.Element arbitrary
                                          (liftM (take 2) arbitrary)
                                          (liftM (take 3) $ listOf $ limitedDepth (n-1))
                       ]

instance Arbitrary B.ByteString where
  arbitrary = liftM unName arbitrary

{-
 - Code for inserting nodes into any point of a tree
 -}

type Loc = Cursor B.ByteString B.ByteString
type Insert a = State Int a

{-
 - Returns the number of unique insertion points in the tree.
 - If h = insertAt f n g", the following property holds:
 - insSize h == (insSize f) + (insSize g) - 1
 -}
insSize :: [X.Node tag text] -> Int
insSize ns = 1 + (sum $ map nodeSize ns)
  where nodeSize (X.Text _) = 1
        nodeSize (X.Element _ _ c) = 1 + (insSize c)

insertAt :: [Node] -> Int -> [Node] -> [Node]
insertAt elems 0 ns = elems ++ ns
insertAt elems _ [] = elems
insertAt elems n list = maybe [] (toForest . root) $
  evalState (processNode elems $ fromJust $ fromForest list) n

move :: Insert ()
move = modify (\x -> x-1)

processNode :: [Node] -> Loc -> Insert (Maybe Loc)
processNode elems loc = liftM2 mplus (move >> goDown loc) (move >> goRight loc)
  where goDown l = case current l of
          X.Text _        -> modify (+1) >> return Nothing
          X.Element _ _ _ -> doneCheck (insertManyFirstChild elems) firstChild l
        goRight = doneCheck (Just . insertManyRight elems) right
        doneCheck insertFunc next l = do
          s <- get
          if s == 0
            then return $ insertFunc l
            else maybe (return Nothing) (processNode elems) $ next l

{-
 - <bind> tests
 -}

-- Data type encapsulating the parameters for a bind operation
data Bind = Bind {
  _bindElemName :: Name,
  _bindChildren :: [Node],
  _bindDoc :: [Node],
  _bindPos :: Int,
  _bindRefPos :: Int
} -- deriving (Show)

instance Show Bind where
  show b@(Bind e c d p r) = unlines
    ["\n"
    ,"Bind element name: "++(show e)
    ,"Bind pos: "++(show p)
    ,"Bind ref pos: "++(show r)
    ,"Bind document:"
    ,L.unpack $ L.concat $ map formatNode d
    ,"Bind children:"
    ,L.unpack $ L.concat $ map formatNode c
    ,"Result:"
    ,L.unpack $ L.concat $ map formatNode $ buildResult b
    ,"Splice result:"
    ,L.unpack $ L.concat $ map formatNode $ unsafePerformIO $
        runTemplateMonad emptyTemplateState (X.Text "")$
        runNodeList $ buildBindTemplate b
    ,"Template:"
    ,L.unpack $ L.concat $ map formatNode $ buildBindTemplate b
    ]

buildNode :: B.ByteString -> B.ByteString -> Bind -> Node
buildNode tag attr (Bind s c _ _ _) = X.Element tag [(attr, unName s)] c

buildBind :: Bind -> Node
buildBind = buildNode "bind" "tag"

instance Arbitrary Bind where
  arbitrary = do
    name <- arbitrary
    kids <- liftM (take 3) arbitrary
    doc <- liftM (take 5) arbitrary
    let s = insSize doc
    loc <- choose (0, s-1)
    loc2 <- choose (0, s-loc-1)
    return $ Bind name kids doc loc loc2
  shrink (Bind e [c] (_:ds) p r) = [Bind e [c] ds p r]
  shrink (Bind e (_:cs) d p r) = [Bind e cs d p r]
  shrink _ = []

empty :: tag -> X.Node tag text
empty n = X.Element n [] []

buildBindTemplate :: Bind -> [Node]
buildBindTemplate s@(Bind n _ d b r) =
  insertAt [empty $ unName $ n] pos $ withBind
  where bind = [buildBind s]
        bindSize = insSize bind
        withBind = insertAt bind b d
        pos = b + bindSize - 1 + r

buildResult :: Bind -> [Node]
buildResult (Bind _ c d b r) = insertAt c (b+r) d

prop_simpleBindTest :: Bind -> PropertyM IO ()
prop_simpleBindTest bind = do
  let template = buildBindTemplate bind
      result = buildResult bind
  spliceResult <- run $ runTemplateMonad emptyTemplateState (X.Text "") $
                  runNodeList template
  assert $ result == spliceResult

{-
 - <apply> tests
 -}

data Apply = Apply {
  _applyName :: Name,
  _applyCaller :: [Node],
  _applyCallee :: Template,
  _applyChildren :: [Node],
  _applyPos :: Int
} deriving (Show)

instance Arbitrary Apply where
  arbitrary = do
    name <- arbitrary
    kids <- liftM (take 3) $ listOf $ limitedDepth 2
    caller <- liftM (take 5) arbitrary
    callee <- liftM (take 1) $ listOf $ limitedDepth 3
    let s = insSize caller
    loc <- choose (0, s-1)
    return $ Apply name caller callee kids loc

buildApplyCaller :: Apply -> [Node]
buildApplyCaller (Apply name caller _ kids pos) =
  insertAt [X.Element "apply" [("template", unName name)] kids] pos caller

calcCorrect :: Apply -> [Node]
calcCorrect (Apply _ caller callee _ pos) = insertAt callee pos caller

calcResult :: (MonadIO m) => Apply -> m [Node]
calcResult apply@(Apply name _ callee _ _) =
  runTemplateMonad ts (X.Text "") $
  runNodeList $ buildApplyCaller apply
  where ts = setTemplates (Map.singleton [unName name]
                           (InternalTemplate Nothing callee))
             emptyTemplateState

prop_simpleApplyTest :: Apply -> PropertyM IO ()
prop_simpleApplyTest apply = do
  let correct = calcCorrect apply
  result <- run $ calcResult apply
  assert $ correct == result


getTS :: FilePath -> IO (TemplateState IO)
getTS baseDir = do
    etm <- loadTemplates baseDir emptyTemplateState
    return $ either error id etm

------------------------------------------------------------------------------
-- | Reloads the templates from disk and renders the specified
-- template.  (Old convenience code.)
quickRender :: FilePath -> ByteString -> IO (Maybe ByteString)
quickRender baseDir name = do
    ts <- getTS baseDir
    renderTemplate ts name


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
    foldr ($) emptyTemplateState
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

