{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Criterion
import           Criterion.Main
import           Criterion.Measurement
import           Control.Concurrent
import           Control.Error
import           Control.Exception (evaluate)
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Maybe
import           System.Environment

import Heist
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I
import Heist.TestCommon

loadWithCache baseDir = do
    etm <- runEitherT $ do
        templates <- loadTemplates baseDir
        let hc = HeistConfig [] defaultLoadTimeSplices [] [] templates
        initHeistWithCacheTag hc
    either (error . unlines) (return . fst) etm

------------------------------------------------------------------------------
--applyComparison :: IO ()
applyComparison dir = do
    let page = "faq"
        pageStr = T.unpack $ decodeUtf8 page
    hs <- loadWithCache dir
    let compiledAction = fst $ fromJust $ C.renderTemplate hs page
    out <- compiledAction
    B.writeFile (pageStr++".out.compiled."++dir) $ toByteString out

    let interpretedAction = I.renderTemplate hs page
    out2 <- interpretedAction
    B.writeFile (pageStr++".out.interpreted."++dir) $ toByteString out

    defaultMain
       [ bench (pageStr++"-compiled") compiledAction
       , bench (pageStr++"-interpreted") interpretedAction
       ]

cmdLineTemplate :: IO ()
cmdLineTemplate = do
    args <- getArgs
    let page = head args
    hs <- loadHS "test/snap-website"
    let action = fst $ fromJust $ C.renderTemplate hs
            (encodeUtf8 $ T.pack page)
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

