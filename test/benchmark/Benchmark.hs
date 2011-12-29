{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Criterion
import           Criterion.Main
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.XmlHtml
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory

------------------------------------------------------------------------------
main :: IO ()
main = do
    tdir <- makeTemplateDirectory
    ts   <- getDirectoryTS tdir

    defaultMain [
         bench "cacheContention" $ cacheContentionBenchmark ts
       ]


------------------------------------------------------------------------------
makeTemplateDirectory = newTemplateDirectory' "templates" defaultHeistState


------------------------------------------------------------------------------
cacheContentionBenchmark :: HeistState IO -> IO ()
cacheContentionBenchmark heistState =
    renderTemplate heistState "cached_haddock" >>=
    maybe (error "render went wrong??")
          (undefined)
