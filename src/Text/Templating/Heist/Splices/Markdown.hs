{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Text.Templating.Heist.Splices.Markdown where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe
import           Control.Concurrent
import           Control.Exception (throwIO)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.Typeable
import           Prelude hiding (catch)
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           Text.XML.Expat.Tree hiding (Node)

------------------------------------------------------------------------------
import           Text.Templating.Heist.Constants
import           Text.Templating.Heist.Types

data PandocMissingException = PandocMissingException
   deriving (Typeable)

instance Show PandocMissingException where
    show PandocMissingException =
        "Cannot find the \"pandoc\" executable; is it on your $PATH?"

instance Exception PandocMissingException


data MarkdownException = MarkdownException ByteString
   deriving (Typeable)

instance Show MarkdownException where
    show (MarkdownException e) =
        "Markdown error: pandoc replied:\n\n" ++ BC.unpack e

instance Exception MarkdownException


------------------------------------------------------------------------------
-- | Default name for the markdown splice.
markdownTag :: ByteString
markdownTag = "markdown"

------------------------------------------------------------------------------
-- | Implementation of the markdown splice.
markdownSplice :: MonadIO m => Splice m
markdownSplice = do
    pdMD <- liftIO $ findExecutable "pandoc"

    when (isNothing pdMD) $ liftIO $ throwIO PandocMissingException

    tree <- getParamNode
    markup <- liftIO $
        case getAttribute tree "file" of
            Just f  -> pandoc (fromJust pdMD) $ BC.unpack f
            Nothing -> pandocBS (fromJust pdMD) $ textContent tree

    let ee = parse' heistExpatOptions markup
    case ee of
      (Left e) -> throw $ MarkdownException
                        $ BC.pack ("Error parsing markdown output: " ++ show e)
      (Right n) -> return [n]


pandoc :: FilePath -> FilePath -> IO ByteString
pandoc pandocPath inputFile = do
    (ex, sout, serr) <- readProcessWithExitCode' pandocPath args ""

    when (isFail ex) $ throw $ MarkdownException serr
    return $ BC.concat [ "<div class=\"markdown\">\n"
                       , sout
                       , "\n</div>" ]

  where
    isFail ExitSuccess = False
    isFail _           = True

    -- FIXME: hardcoded path
    args = [ "-S", "--no-wrap", "templates/"++inputFile ]


pandocBS :: FilePath -> ByteString -> IO ByteString
pandocBS pandocPath s = do
    -- using the crummy string functions for convenience here
    (ex, sout, serr) <- readProcessWithExitCode' pandocPath args s

    when (isFail ex) $ throw $ MarkdownException serr
    return $ BC.concat [ "<div class=\"markdown\">\n"
                       , sout
                       , "\n</div>" ]

  where
    isFail ExitSuccess = False
    isFail _           = True
    args = [ "-S", "--no-wrap" ]


-- a version of readProcessWithExitCode that does I/O properly
readProcessWithExitCode'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> ByteString               -- ^ standard input
    -> IO (ExitCode,ByteString,ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar

    outM <- newEmptyMVar
    errM <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    forkIO $ do
        out <- B.hGetContents outh
        putMVar outM out
        putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    forkIO $ do
        err  <- B.hGetContents errh
        putMVar errM err
        putMVar outMVar ()

    -- now write and flush any input
    when (not (B.null input)) $ do B.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    out <- readMVar outM
    err <- readMVar errM

    return (ex, out, err)




