{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
import           Criterion.Measurement
------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception (evaluate)
import           Control.Monad
import           Blaze.ByteString.Builder
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory

------------------------------------------------------------------------------
main :: IO ()
main = do
    tdir <- makeTemplateDirectory
    ts   <- getDirectoryTS tdir

    defaultMain [
         bench "faq-speed" (renderTemplate ts "snap-website/faq")
       , bench contentionBenchmarkName $
               cacheContentionBenchmark contentionTrials
                                        contentionThreads
                                        ts
       ]

  where
    contentionThreads = 10
    contentionTrials  = 3

    contentionBenchmarkName = concat [ "cacheContention/"
                                     , show contentionThreads
                                     , "/"
                                     , show contentionTrials ]


------------------------------------------------------------------------------
_estimateCostPerRun :: Int -> Int -> Double -> String
_estimateCostPerRun tasks trials totalTime =
    concat [ "per render under contention: average cost of "
           , secs $ _costPerRun tasks trials totalTime ]

_costPerRun :: Int -> Int -> Double -> Double
_costPerRun tasks trials totalTime =
    totalTime / fromIntegral (tasks * trials)


------------------------------------------------------------------------------
makeTemplateDirectory :: IO (TemplateDirectory IO)
makeTemplateDirectory = newTemplateDirectory' "templates" defaultHeistState


------------------------------------------------------------------------------
cacheContentionBenchmark :: Int    -- ^ number of trials per thread
                         -> Int    -- ^ number of pool threads
                         -> HeistState IO
                         -> IO ()
cacheContentionBenchmark nTrialsPerThread nThreads heistState = do
    mvars <- mkMVars
    _     <- mkThreads mvars

    -- wait for all threads to finish
    mapM_ takeMVar mvars

  where
    mkMVars = replicateM nThreads newEmptyMVar

    mkThreads = mapM_ (forkIO . oneThread)

    oneThread mvar = do
        replicateM_ nTrialsPerThread (renderOne >>= evaluate)
        putMVar mvar ()

    renderOne =
        renderTemplate heistState "cached_haddock" >>=
        return . maybe (error "render went wrong??") (toByteString . fst)
