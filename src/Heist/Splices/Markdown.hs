{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-| The \"markdown\" splice formats markdown content as HTML and inserts
it into the document.

If the file attribute is present the contents of the tag is ignored and
the file specified is converted to HTML.

Otherwise the non-markup children of the tag are processed as markdown
and converted to HTML.

This splice requires that the \"pandoc\" executable is in your path.

You can add custom pandoc splice with 'pandocSplice'. It is not limited to
markdown input, and can process anything pandoc can.

For example you can create a page with generated table of contents, using
heist template as pandoc template.

>  <!-- _wrap.tpl -->
>  <html>
>    <head> <title> <pageTitle/> </title> </head>
>
>    <div class="nav"> <pageToc/> </div>
>    <apply-content/>
>  </html>

And pandoc template, which would bind @pageTitle@ and @pageToc@ splices and
applies "_wrap" template.

>  <!-- _pandoc.tpl -->
>  <apply template="_wrap.tpl">
>    <bind tag="pageTitle"> $title$</bind>
>    <bind tag="pageToc"> $toc$</bind>
>    $body$
>  </apply>

Bind splice pandoc splice. Set it to not wrap in div, or it will break html
from _wrap.tpl

>  splices = "docmarkdown" ## pandocSplice opts
>    where
>      opts = setPandocArgs  ["-S", "--no-wrap", "--toc"
>                            , "--standalone"
>                            , "--template", "_pandoc.tpl"
>                            , "--html5"]
>             $ setPandocWrapDiv Nothing
>             $ defaultPandocOptions
>

And then use it to render your markdown file


>  <!-- apidocs.tpl -->
>  <DOCTYPE html>
>  <html lang="en">
>  <head>
>    <link href="/static/css/site.css rel="stylesheet">
>  </head>
>  <body>
>    <apply template="_navbar.tpl" />
>    <docmarkdown file="apidocs.md"/>
>  </body>

-}
module Heist.Splices.Markdown
  (
  -- * Exceptions
    PandocMissingException
  , MarkdownException
  , NoMarkdownFileException
  -- * Markdown Splice
  , markdownTag
  , markdownSplice
  -- * Generic pandoc splice
  , pandocSplice
  -- ** Pandoc Options
  , PandocOptions
  , defaultPandocOptions
  , setPandocExecutable
  , setPandocArgs
  , setPandocBaseDir
  , setPandocWrapDiv
  -- ** Lens for 'PandocOptions'
  , pandocExecutable
  , pandocArgs
  , pandocBaseDir
  , pandocWrapDiv
  -- * Internal helper functions
  , pandoc
  , pandocBS
  , readProcessWithExitCode'
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Typeable
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process
import           Text.XmlHtml

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative             ((<$>))
#endif

------------------------------------------------------------------------------
import           Heist.Common
import           Heist.Internal.Types.HeistState
import           Heist.Interpreted.Internal

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

--------------------------------------------------------------------------------

data PandocOptions = PandocOptions
     { _pandocExecutable :: FilePath
     , _pandocArgs       :: [String]         -- ^ Arguments to pandoc
     , _pandocBaseDir    :: Maybe FilePath   -- ^ Base directory for input files
                                             --   defaults to template path
     , _pandocWrapDiv    :: Maybe Text       -- ^ Wrap content in div with class
     } deriving (Eq, Ord, Show)

-- | Default options
defaultPandocOptions :: PandocOptions
defaultPandocOptions = PandocOptions "pandoc"
                                     ["--wrap=none"]
                                     Nothing
                                     (Just "markdown+smart")

-- | Name of pandoc executable
setPandocExecutable :: FilePath -> PandocOptions -> PandocOptions
setPandocExecutable e opt = opt { _pandocExecutable = e }

-- | Arguments passed to pandoc
setPandocArgs :: [String] -> PandocOptions -> PandocOptions
setPandocArgs args opt = opt { _pandocArgs = args }

-- | Base directory for input files, defaults to current template dir
setPandocBaseDir :: Maybe FilePath -> PandocOptions -> PandocOptions
setPandocBaseDir bd opt = opt { _pandocBaseDir = bd }

-- | Wrap pandoc output in div with class. Appends node attributes to
--   div and appends class to ones specified on node.
setPandocWrapDiv :: Maybe Text -> PandocOptions -> PandocOptions
setPandocWrapDiv wd opt = opt { _pandocWrapDiv = wd }

pandocExecutable :: Functor f =>
     (FilePath -> f FilePath) -> PandocOptions -> f PandocOptions
pandocExecutable f po = (\e -> po { _pandocExecutable = e})
                       <$> f (_pandocExecutable po)

pandocArgs :: Functor f =>
     ([String] -> f [String]) -> PandocOptions -> f PandocOptions
pandocArgs f po = (\a -> po { _pandocArgs = a}) <$> f (_pandocArgs po)

pandocBaseDir :: Functor f =>
     (Maybe FilePath -> f (Maybe FilePath)) -> PandocOptions -> f PandocOptions
pandocBaseDir f po = (\b -> po {_pandocBaseDir = b }) <$> f (_pandocBaseDir po)

pandocWrapDiv :: Functor f =>
     (Maybe Text -> f (Maybe Text)) -> PandocOptions -> f PandocOptions
pandocWrapDiv f po = (\w -> po {_pandocWrapDiv = w}) <$> f (_pandocWrapDiv po)

------------------------------------------------------------------------------
-- | Default name for the markdown splice.
markdownTag :: Text
markdownTag = "markdown"

------------------------------------------------------------------------------
-- | Default markdown splice with executable "pandoc" and options "-S --no-wrap"
markdownSplice :: MonadIO m => Splice m
markdownSplice= pandocSplice defaultPandocOptions

-- | Implementation of the markdown splice.
pandocSplice :: MonadIO m => PandocOptions -> Splice m
pandocSplice PandocOptions{..} = do
    templateDir <- liftM (fmap takeDirectory) getTemplateFilePath
    pdMD <- liftIO $ findExecutable _pandocExecutable

    pandocExe <- case pdMD of
       Nothing -> liftIO $ throwIO PandocMissingException
       Just pd -> return pd
    let withDir tp = fromMaybe tp _pandocBaseDir
        pandocFile f tp = pandocWith pandocExe _pandocArgs (withDir tp) f
    tree <- getParamNode
    (source,markup) <- liftIO $
        case getAttribute "file" tree of
            Just f  -> do
                m <- maybe (liftIO $ throwIO NoMarkdownFileException )
                           (pandocFile (T.unpack f))
                           templateDir
                return (T.unpack f,m)
            Nothing -> do
                m <- pandocWithBS pandocExe _pandocArgs $ T.encodeUtf8 $ nodeText tree
                return ("inline_splice",m)

    let ee = parseHTML source markup
        nodeAttrs = case tree of
          Element _ a _ -> a
          _ -> []
        nodeClass = lookup "class" nodeAttrs
        attrs = filter (\(name, _) -> name /= "class" && name /= "file") nodeAttrs
    case ee of
      Left e  -> throw $ MarkdownException
                       $ BC.pack ("Error parsing markdown output: " ++ e)
      Right d -> return $ wrapResult nodeClass attrs (docContent d)

  where
    wrapResult nodeClass attrs body = case _pandocWrapDiv of
        Nothing -> body
        Just cls -> let finalAttrs = ("class", appendClass nodeClass cls):attrs
                    in [Element "div" finalAttrs  body]
    appendClass Nothing cls = cls
    appendClass (Just orig) cls = T.concat [orig, " ", cls]


pandoc :: FilePath -> FilePath -> FilePath -> IO ByteString
pandoc pandocPath templateDir inputFile = do
    sout <- pandocWith pandocPath args templateDir inputFile
    return $ BC.concat [ "<div class=\"markdown\">\n"
                         , sout
                         , "\n</div>" ]

  where
    args = [ "-S", "--no-wrap"]


pandocBS :: FilePath -> ByteString -> IO ByteString
pandocBS pandocPath s = do
    sout <- pandocWithBS pandocPath args s
    return $ BC.concat [ "<div class=\"markdown\">\n"
                       , sout
                       , "\n</div>" ]

  where
    args = [ "-S", "--no-wrap" ]


pandocWith :: FilePath -> [String] -> FilePath -> FilePath -> IO ByteString
pandocWith path args templateDir inputFile = do
    (ex, sout, serr) <- readProcessWithExitCode' path args' ""

    when (isFail ex) $ throw $ MarkdownException serr
    return sout

  where
    isFail ExitSuccess = False
    isFail _           = True

    args' = args ++ [templateDir </> inputFile ]

pandocWithBS :: FilePath -> [String] -> ByteString -> IO ByteString
pandocWithBS pandocPath args s = do
    -- using the crummy string functions for convenience here
    (ex, sout, serr) <- readProcessWithExitCode' pandocPath args s

    when (isFail ex) $ throw $ MarkdownException serr
    return sout

  where
    isFail ExitSuccess = False
    isFail _           = True


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
