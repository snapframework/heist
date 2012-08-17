module Heist where

import           Control.Monad
import           Control.Monad.Trans
import           Data.DList                      (DList)
import           Data.Either
import qualified Data.Foldable as F
import qualified Data.HeterogeneousEnvironment   as HE
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Text                       (Text)
import           System.Directory.Tree

import           Heist.Common
import           Heist.Compiled.Internal
import qualified Heist.Compiled.Splices as CS
import           Heist.Interpreted.Splices
import           Heist.Types


------------------------------------------------------------------------------
-- | The default set of built-in splices.
defaultInterpretedSplices :: MonadIO n => HashMap Text (HeistT n n Template)
defaultInterpretedSplices = Map.fromList
    [(applyTag, applyImpl)
    ,(bindTag, bindImpl)
    ,(ignoreTag, ignoreImpl)
    ,(markdownTag, markdownSplice)
    ]


defaultCompiledSplices :: MonadIO n => HashMap Text (CompiledSplice n)
defaultCompiledSplices = Map.fromList
    [(CS.applyTag, CS.applyImpl)
    ,(CS.bindTag, CS.bindImpl)
    ,(CS.ignoreTag, CS.ignoreImpl)
    ,(CS.markdownTag, CS.markdownSplice)
    ]


------------------------------------------------------------------------------
-- | An empty heist state, with Heist's default splices (@\<apply\>@,
-- @\<bind\>@, @\<ignore\>@, and @\<markdown\>@) mapped.  The cache tag is
-- not mapped here because it must be mapped manually in your application.
defaultHeistState :: MonadIO n => IO (HeistState n m)
defaultHeistState =
    liftM (HeistState defaultInterpretedSplices Map.empty
                      --defaultCompiledSplices Map.empty
                      Map.empty Map.empty
                      True [] 0 return [] Nothing)
          HE.newKeyGen


------------------------------------------------------------------------------
-- | This is the main heist initialization function.  First it traverses the
-- specified directory structure and builds a HeistState by loading all the
-- files with a ".tpl" or ".xtpl" extension.  Next all compiled splices are
-- processed.  We don't provide functions to bind compiled splices because it
-- doesn't make any sense unless you re-compile all templates with the new
-- splices.  If you add any heist splices after loadTemplates, they will still
-- work fine.  If you add any templates later, then those templates will be
-- available for interpreted rendering, but not for compiled rendering.
loadTemplates :: Monad n
              => FilePath
              -> [(Text, HeistT n IO (DList (Chunk n)))]
              -- ^ Loadtime splices
              -> HeistState n IO
              -> IO (Either String (HeistState n IO))
loadTemplates dir splices hs = do
    d <- readDirectoryWith (loadTemplate dir) dir
    let tlist = F.fold (free d)
        errs = lefts tlist
        hs' = bindSplices splices hs
    case errs of
        [] -> do ts' <- compileTemplates =<< foldM loadHook hs' (rights tlist)
                 return $ Right ts'
        _  -> return $ Left $ unlines errs


