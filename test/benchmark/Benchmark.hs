{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
import           Criterion.Measurement
------------------------------------------------------------------------------
import           Control.Concurrent.ParallelIO.Local
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
               cacheContentionBenchmark contentionThreads
                                        contentionTasks
                                        contentionTrials
                                        ts
       ]

  where
    contentionThreads = 10
    contentionTasks   = 20
    contentionTrials  = 1

    contentionBenchmarkName = concat [ "cacheContention/"
                                     , show contentionThreads
                                     , "/"
                                     , show contentionTasks
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
cacheContentionBenchmark :: Int    -- ^ number of tasks
                         -> Int    -- ^ number of trials per task
                         -> Int    -- ^ number of pool threads
                         -> HeistState IO
                         -> IO ()
cacheContentionBenchmark nTasks nTrialsPerTask nThreads heistState =
    withPool nThreads $ \pool -> do
        parallel_ pool $ replicate nTasks
                       $ replicateM_ nTrialsPerTask (renderOne >>= evaluate)
        return $! ()

  where
    renderOne =
        renderTemplate heistState "cached_haddock" >>=
        return . maybe (error "render went wrong??") (toByteString . fst)
