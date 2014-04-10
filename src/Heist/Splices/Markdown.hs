{-# LANGUAGE DeriveDataTypeable #-}
{-| The \"markdown\" splice formats markdown content as HTML and inserts
it into the document.

If the file attribute is present the contents of the tag is ignored and
the file specified is converted to HTML.

Otherwise the non-markup children of the tag are processed as markdown
and converted to HTML.

This splice requires that the \"pandoc\" executable is in your path.
-}
module Heist.Splices.Markdown where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Control.Concurrent
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Trans
import           Data.Typeable
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process
import           Text.XmlHtml

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Interpreted.Internal
import           Heist.Types

data PandocMissingException = PandocMissingException
   deriving (Typeable)

instance Show PandocMissingException where
    show PandocMissingException =
        "Cannot find the \"pandoc\" executable.  If you have Haskell, then install it with \"cabal install\".  Otherwise you can download it from http://johnmacfarlane.net/pandoc/installing.html.  Then make sure it is in your $PATH."

instance Exception PandocMissingException


data MarkdownException = MarkdownException ByteString
   deriving (Typeable)

instance Show MarkdownException where
    show (MarkdownException e) =
        "Markdown error: pandoc replied:\n\n" ++ BC.unpack e

instance Exception MarkdownException


data NoMarkdownFileException = NoMarkdownFileException
    deriving (Typeable)

instance Show NoMarkdownFileException where
    show NoMarkdownFileException =
        "Markdown error: no file or template in context" ++
        " during processing of markdown tag"

instance Exception NoMarkdownFileException where

------------------------------------------------------------------------------
-- | Default name for the markdown splice.
markdownTag :: Text
markdownTag = "markdown"

------------------------------------------------------------------------------
-- | Implementation of the markdown splice.
markdownSplice :: MonadIO m => Splice m
markdownSplice = do
    templateDir <- liftM (fmap takeDirectory) getTemplateFilePath
    pdMD <- liftIO $ findExecutable "pandoc"

    when (isNothing pdMD) $ liftIO $ throwIO PandocMissingException

    tree <- getParamNode
    (source,markup) <- liftIO $
        case getAttribute "file" tree of
            Just f  -> do
                m <- maybe (liftIO $ throwIO NoMarkdownFileException )
                           (\tp -> pandoc (fromJust pdMD) tp $ T.unpack f)
                           templateDir
                return (T.unpack f,m)
            Nothing -> do
                m <- pandocBS (fromJust pdMD) $ T.encodeUtf8 $ nodeText tree
                return ("inline_splice",m)

    let ee = parseHTML source markup
    case ee of
      Left e  -> throw $ MarkdownException
                       $ BC.pack ("Error parsing markdown output: " ++ e)
      Right d -> return (docContent d)


pandoc :: FilePath -> FilePath -> FilePath -> IO ByteString
pandoc pandocPath templateDir inputFile = do
    (ex, sout, serr) <- readProcessWithExitCode' pandocPath args ""

    when (isFail ex) $ throw $ MarkdownException serr
    return $ BC.concat [ "<div class=\"markdown\">\n"
                         , sout
                         , "\n</div>" ]

  where
    isFail ExitSuccess = False
    isFail _           = True

    args = [ "-S", "--no-wrap", templateDir </> inputFile ]


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
    _ <- forkIO $ do
        out <- B.hGetContents outh
        putMVar outM out
        putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    _ <- forkIO $ do
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




