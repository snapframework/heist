{-# LANGUAGE BangPatterns               #-}

module Text.Templating.Heist.Common where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.Text            as AP
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                       as T
import           System.FilePath
import           Text.Templating.Heist.Types

------------------------------------------------------------------------------
-- | Sets the current template file.
setCurTemplateFile :: Monad m
                   => Maybe FilePath -> HeistState n m -> HeistState n m
setCurTemplateFile fp ts = ts { _curTemplateFile = fp }


------------------------------------------------------------------------------
setCurContext :: Monad m => TPath -> HeistState n m -> HeistState n m
setCurContext tp ts = ts { _curContext = tp }


------------------------------------------------------------------------------
-- | Parser for attribute variable substitution.
attParser :: AP.Parser [AttAST]
attParser = liftM ($! []) (loop id)
  where
    append !dl !x = dl . (x:)

    loop !dl = go id
      where
        finish subDL = let !txt = T.concat $! subDL []
                           lit  = Literal $! T.concat $! subDL []
                       in return $! if T.null txt
                                      then dl
                                      else append dl lit

        go !subDL = (gobbleText >>= go . append subDL)
                    <|> (AP.endOfInput *> finish subDL)
                    <|> (escChar >>= go . append subDL)
                    <|> (do
                            idp <- identParser
                            dl' <- finish subDL
                            loop $! append dl' idp)

    gobbleText = AP.takeWhile1 (AP.notInClass "\\$")

    escChar = AP.char '\\' *> (T.singleton <$> AP.anyChar)

    identParser = AP.char '$' *> (ident <|> return (Literal "$"))
    ident = (AP.char '{' *> (Ident <$> AP.takeWhile (/='}')) <* AP.string "}")


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order.  If the
-- path is absolute, we need to remove the leading slash so the split doesn't
-- leave @\"\"@ as the last element of the TPath.
--
-- FIXME @\"..\"@ currently doesn't work in paths, the solution is non-trivial
splitPathWith :: Char -> ByteString -> TPath
splitPathWith s p = if BC.null p then [] else (reverse $ BC.split s path)
  where
    path = if BC.head p == s then BC.tail p else p


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order using the
-- path separator of the local operating system. See 'splitPathWith' for more
-- details.
splitLocalPath :: ByteString -> TPath
splitLocalPath = splitPathWith pathSeparator


------------------------------------------------------------------------------
-- | Converts a path into an array of the elements in reverse order using a
-- forward slash (/) as the path separator. See 'splitPathWith' for more
-- details.
splitTemplatePath :: ByteString -> TPath
splitTemplatePath = splitPathWith '/'


------------------------------------------------------------------------------
-- | Convenience function for looking up a template.
lookupTemplate :: ByteString
               -> HeistState n m
               -> (HeistState n m -> HashMap TPath t)
               -> Maybe (t, [ByteString])
lookupTemplate nameStr ts tm = f (tm ts) path name
  where
    (name:p) = case splitTemplatePath nameStr of
                   [] -> [""]
                   ps -> ps
    ctx = if B.isPrefixOf "/" nameStr then [] else _curContext ts
    path = p ++ ctx
    f = if '/' `BC.elem` nameStr
            then singleLookup
            else traversePath


------------------------------------------------------------------------------
-- | Returns 'True' if the given template can be found in the heist state.
hasTemplate :: ByteString -> HeistState n m -> Bool
hasTemplate nameStr ts =
    isJust $ lookupTemplate nameStr ts _templateMap


------------------------------------------------------------------------------
-- | Does a single template lookup without cascading up.
singleLookup :: (Eq a, Hashable a)
             => HashMap [a] t -> [a] -> a -> Maybe (t, [a])
singleLookup tm path name = fmap (\a -> (a,path)) $ Map.lookup (name:path) tm


------------------------------------------------------------------------------
-- | Searches for a template by looking in the full path then backing up into
-- each of the parent directories until the template is found.
traversePath :: (Eq a, Hashable a)
             => HashMap [a] t -> [a] -> a -> Maybe (t, [a])
traversePath tm [] name = fmap (\a -> (a,[])) (Map.lookup [name] tm)
traversePath tm path name =
    singleLookup tm path name `mplus`
    traversePath tm (tail path) name


------------------------------------------------------------------------------
-- | Maps a splice generating function over a list and concatenates the
-- results.  This function now has a more general type signature so it works
-- with both Heist and Caper splices.  The old type signature was this:
--
-- > mapSplices :: (Monad n)
-- >         => (a -> Splice n n)
-- >         -> [a]
-- >         -> Splice n n
mapSplices :: (Monad m, Monoid r)
           => (a -> m r)
           -- ^ Splice generating function
           -> [a]
           -- ^ List of items to generate splices for
           -> m r
           -- ^ The result of all splices concatenated together.
mapSplices f vs = liftM mconcat $ mapM f vs
{-# INLINE mapSplices #-}


