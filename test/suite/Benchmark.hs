{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Criterion
import           Criterion.Main
import           Criterion.Measurement
import           Control.Concurrent
import           Control.Exception (evaluate)
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Maybe
import           System.Environment

import qualified Heist.Compiled as C
import Heist.Tests

------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    let page = head args
    hs <- loadTS "test/snap-website"
    let action = fromJust $ C.renderCompiledTemplate
            (encodeUtf8 $ T.pack page) hs
    out <- action
    B.writeFile (page++".out.cur") $ toByteString out
--    reference <- B.readFile "faq.out"
--    if False
--      then do
--        putStrLn "Template didn't render properly"
--        error "Aborting"
--      else
--        putStrLn "Template rendered correctly"

    defaultMain [
         bench (page++"-speed") action
       ]

