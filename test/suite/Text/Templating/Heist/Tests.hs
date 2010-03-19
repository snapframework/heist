{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Text.Templating.Heist.Tests
  ( tests

-- Temporary exports to eliminate warnings
  , html
  , hhead
  , title
  , body
  , para
  , para2
  , para3
  , foo
  , tdoc
  , _bindPos
  , _bindRefPos
  , _bindElemName
  , _bindChildren
  , _bindDoc
  , bindElem
  , addBind
  , prn
  , runTests

  ) where

import           Test.Framework (Test, defaultMain)
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic

import           Control.Monad.State

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe

import           System.IO.Unsafe

import           Text.Templating.Heist
import           Text.XML.Expat.Cursor
import           Text.XML.Expat.Format
import qualified Text.XML.Expat.Tree as X

tests :: [Test]
tests = [ testProperty "simpleBindTest" $ monadicIO $ forAllM arbitrary prop_simpleBindTest
--        , testProperty "simpleApplyTest" $ monadicIO $ forAllM arbitrary prop_simpleApplyTest
        ]

plainElementNames :: [B.ByteString]
plainElementNames = [ "a"
                    , "p"
                    , "b"
                    , "div"
                    , "span"
                    ]

identStartChar :: [Char]
identStartChar = ['a'..'z']
identChar :: [Char]
identChar = '_' : identStartChar

newtype HtmlTag = HtmlTag B.ByteString
instance Show HtmlTag where
  show (HtmlTag s) = B.unpack s

instance Arbitrary HtmlTag where
  arbitrary = oneof $ map (return . HtmlTag) plainElementNames

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
  shrink (X.Element n [] []) = []
  shrink (X.Element n [] (c:cs)) = [X.Element n [] cs]
  shrink (X.Element n (a:as) []) = [X.Element n as []]
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
    ,L.unpack $ L.concat $ map formatNode $ unsafePerformIO $ runBareTemplate $ buildBindTemplate b
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
  shrink (Bind e [c] [d] p r) = []
  shrink (Bind e [c] (d:ds) p r) = [Bind e [c] ds p r]
  shrink (Bind e (c:cs) d p r) = [Bind e cs d p r]

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
  spliceResult <- run $ runBareTemplate template
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

calcCorrect (Apply _ caller callee _ pos) = insertAt callee pos caller
calcResult apply@(Apply name _ callee _ _) =
  runTemplate ts $ buildApplyCaller apply
  where ts = emptyTemplateState { _templateMap = Map.singleton [unName name] callee }

prop_simpleApplyTest :: Apply -> PropertyM IO ()
prop_simpleApplyTest apply@(Apply name caller callee _ _) = do
  let correct = calcCorrect apply
  result <- run $ calcResult apply
  assert $ correct == result


{-
 - Convenience code for manual ghci experimentation
 -}

html :: [Node] -> Node
html c = X.Element "html" [] [hhead, body c]
hhead :: Node
hhead = X.Element "head" [] [title, X.Element "script" [] []]
title :: Node
title = X.Element "title" [] [X.Text "Test Page"]
body :: [Node] -> Node
body = X.Element "body" []

para :: Int -> Node
para n = X.Element "p" [] [X.Text $ B.pack $ "This is paragraph " ++ show n]
para2 :: B.ByteString -> Node
para2 c = X.Element "p" [] [X.Text c]
para3 :: Node
para3 = X.Element "p" [] [X.Text "AHA!"]

foo :: Int -> [Node]
foo n = insertAt [X.Element "NEW" [] []] n [html [para 1, para 2]]

tdoc :: [Node]
tdoc = [para 1, para 2, para 3, para 4]

bindElem :: [Node] -> Int -> Int -> Bind
bindElem = Bind (Name "mytag") [para2 "bound paragraph"]

addBind :: Bind -> [Node] -> [Node]
addBind b = insertAt [buildBind b] 0 . insertAt [empty $ unName $ _bindElemName b] 2

prn :: Node -> IO ()
prn = L.putStrLn . formatNode
runTests :: IO ()
runTests = defaultMain tests

