module Text.Templating.Heist.Splices.Ignore where

------------------------------------------------------------------------------
import           Data.Text (Text)

------------------------------------------------------------------------------
import           Text.Templating.Heist.Types


------------------------------------------------------------------------------
-- | Default name for the ignore splice.
ignoreTag :: Text
ignoreTag = "ignore"


------------------------------------------------------------------------------
-- | The ignore tag and everything it surrounds disappears in the
-- rendered output.
ignoreImpl :: Monad m => Splice m
ignoreImpl = return []


