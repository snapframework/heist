{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-|

Internal types and accessors.  There are no guarantees that heist will
preserve backwards compatibility for symbols in this module.  If you use them,
no complaining when your code breaks.

-}

module Heist.Internal.Types
  ( module Heist.Internal.Types.HeistState
  , module Heist.Internal.Types
  ) where

------------------------------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
import           Data.Monoid
#endif

------------------------------------------------------------------------------
import qualified Heist.Compiled.Internal       as C
import qualified Heist.Interpreted.Internal    as I
import           Heist.Internal.Types.HeistState
------------------------------------------------------------------------------


------------------------------------------------------------------------------
type TemplateRepo = HashMap TPath DocumentFile


------------------------------------------------------------------------------
-- | An IO action for getting a template repo from this location.  By not just
-- using a directory path here, we support templates loaded from a database,
-- retrieved from the network, or anything else you can think of.
type TemplateLocation = IO (Either [String] TemplateRepo)


------------------------------------------------------------------------------
-- | My lens creation function to avoid a dependency on lens.
lens :: Functor f => (t1 -> t) -> (t1 -> a -> b) -> (t -> f a) -> t1 -> f b
lens sa sbt afb s = sbt s <$> afb (sa s)


------------------------------------------------------------------------------
-- | The splices and templates Heist will use.  To bind a splice simply
-- include it in the appropriate place here.
data SpliceConfig m = SpliceConfig
    { _scInterpretedSplices     :: Splices (I.Splice m)
        -- ^ Interpreted splices are the splices that Heist has always had.
        -- They return a list of nodes and are processed at runtime.
    , _scLoadTimeSplices        :: Splices (I.Splice IO)
        -- ^ Load time splices are like interpreted splices because they
        -- return a list of nodes.  But they are like compiled splices because
        -- they are processed once at load time.  All of Heist's built-in
        -- splices should be used as load time splices.
    , _scCompiledSplices        :: Splices (C.Splice m)
        -- ^ Compiled splices return a DList of Chunks and are processed at
        -- load time to generate a runtime monad action that will be used to
        -- render the template.
    , _scAttributeSplices       :: Splices (AttrSplice m)
        -- ^ Attribute splices are bound to attribute names and return a list
        -- of attributes.
    , _scTemplateLocations      :: [TemplateLocation]
        -- ^ A list of all the locations that Heist should get its templates
        -- from.
    , _scCompiledTemplateFilter :: TPath -> Bool
        -- ^ Predicate function to control which templates to compile.  Using
        -- templates filtered out with this is still possible via
        -- callTemplate.
    }


------------------------------------------------------------------------------
-- | Lens for interpreted splices
-- :: Simple Lens (SpliceConfig m) (Splices (I.Splice m))
scInterpretedSplices
    :: Functor f
    => (Splices (I.Splice m) -> f (Splices (I.Splice m)))
    -> SpliceConfig m -> f (SpliceConfig m)
scInterpretedSplices = lens _scInterpretedSplices setter
  where
    setter sc v = sc { _scInterpretedSplices = v }


------------------------------------------------------------------------------
-- | Lens for load time splices
-- :: Simple Lens (SpliceConfig m) (Splices (I.Splice IO))
scLoadTimeSplices
    :: Functor f
    => (Splices (I.Splice IO) -> f (Splices (I.Splice IO)))
    -> SpliceConfig m -> f (SpliceConfig m)
scLoadTimeSplices = lens _scLoadTimeSplices setter
  where
    setter sc v = sc { _scLoadTimeSplices = v }


------------------------------------------------------------------------------
-- | Lens for complied splices
-- :: Simple Lens (SpliceConfig m) (Splices (C.Splice m))
scCompiledSplices
    :: Functor f
    => (Splices (C.Splice m) -> f (Splices (C.Splice m)))
    -> SpliceConfig m -> f (SpliceConfig m)
scCompiledSplices = lens _scCompiledSplices setter
  where
    setter sc v = sc { _scCompiledSplices = v }


------------------------------------------------------------------------------
-- | Lens for attribute splices
-- :: Simple Lens (SpliceConfig m) (Splices (AttrSplice m))
scAttributeSplices
    :: Functor f
    => (Splices (AttrSplice m) -> f (Splices (AttrSplice m)))
    -> SpliceConfig m -> f (SpliceConfig m)
scAttributeSplices = lens _scAttributeSplices setter
  where
    setter sc v = sc { _scAttributeSplices = v }


------------------------------------------------------------------------------
-- | Lens for template locations
-- :: Simple Lens (SpliceConfig m) [TemplateLocation]
scTemplateLocations
    :: Functor f
    => ([TemplateLocation] -> f [TemplateLocation])
    -> SpliceConfig m -> f (SpliceConfig m)
scTemplateLocations = lens _scTemplateLocations setter
  where
    setter sc v = sc { _scTemplateLocations = v }


------------------------------------------------------------------------------
-- | Lens for compiled template filter
-- :: Simple Lens (SpliceConfig m) (TBool -> Bool)
scCompiledTemplateFilter
    :: Functor f
    => ((TPath -> Bool) -> f (TPath -> Bool))
    -> SpliceConfig m -> f (SpliceConfig m)
scCompiledTemplateFilter = lens _scCompiledTemplateFilter setter
  where
    setter sc v = sc { _scCompiledTemplateFilter = v }


instance Monoid (SpliceConfig m) where
    mempty = SpliceConfig mempty mempty mempty mempty mempty (const True)
    mappend (SpliceConfig a1 b1 c1 d1 e1 f1)
            (SpliceConfig a2 b2 c2 d2 e2 f2) =
      SpliceConfig (mappend a1 a2) (mappend b1 b2) (mappend c1 c2)
                   (mappend d1 d2) (mappend e1 e2) (\x -> f1 x && f2 x)


data HeistConfig m = HeistConfig
    { _hcSpliceConfig  :: SpliceConfig m
        -- ^ Splices and templates
    , _hcNamespace     :: Text
        -- ^ A namespace to use for all tags that are bound to splices.  Use
        -- empty string for no namespace.
    , _hcErrorNotBound :: Bool
        -- ^ Whether to throw an error when a tag wih the heist namespace does
        -- not correspond to a bound splice.  When not using a namespace, this
        -- flag is ignored.
    }


------------------------------------------------------------------------------
-- | Lens for the SpliceConfig
-- :: Simple Lens (HeistConfig m) (SpliceConfig m)
hcSpliceConfig
    :: Functor f
    => ((SpliceConfig m) -> f (SpliceConfig m))
    -> HeistConfig m -> f (HeistConfig m)
hcSpliceConfig = lens _hcSpliceConfig setter
  where
    setter hc v = hc { _hcSpliceConfig = v }


------------------------------------------------------------------------------
-- | Lens for the namespace
-- :: Simple Lens (HeistConfig m) Text
hcNamespace
    :: Functor f
    => (Text -> f Text)
    -> HeistConfig m -> f (HeistConfig m)
hcNamespace = lens _hcNamespace setter
  where
    setter hc v = hc { _hcNamespace = v }


------------------------------------------------------------------------------
-- | Lens for the namespace error flag
-- :: Simple Lens (HeistConfig m) Bool
hcErrorNotBound
    :: Functor f
    => (Bool -> f Bool)
    -> HeistConfig m -> f (HeistConfig m)
hcErrorNotBound = lens _hcErrorNotBound setter
  where
    setter hc v = hc { _hcErrorNotBound = v }


------------------------------------------------------------------------------
-- | Lens for interpreted splices
-- :: Simple Lens (HeistConfig m) (Splices (I.Splice m))
hcInterpretedSplices
    :: Functor f
    => (Splices (I.Splice m) -> f (Splices (I.Splice m)))
    -> HeistConfig m -> f (HeistConfig m)
hcInterpretedSplices = hcSpliceConfig . scInterpretedSplices


------------------------------------------------------------------------------
-- | Lens for load time splices
-- :: Simple Lens (HeistConfig m) (Splices (I.Splice IO))
hcLoadTimeSplices
    :: Functor f
    => (Splices (I.Splice IO) -> f (Splices (I.Splice IO)))
    -> HeistConfig m -> f (HeistConfig m)
hcLoadTimeSplices = hcSpliceConfig . scLoadTimeSplices 


------------------------------------------------------------------------------
-- | Lens for compiled splices
-- :: Simple Lens (HeistConfig m) (Splices (C.Splice m))
hcCompiledSplices
    :: Functor f
    => (Splices (C.Splice m) -> f (Splices (C.Splice m)))
    -> HeistConfig m -> f (HeistConfig m)
hcCompiledSplices = hcSpliceConfig . scCompiledSplices 


------------------------------------------------------------------------------
-- | Lens for attribute splices
-- :: Simple Lens (HeistConfig m) (Splices (AttrSplice m))
hcAttributeSplices
    :: Functor f
    => (Splices (AttrSplice m) -> f (Splices (AttrSplice m)))
    -> HeistConfig m -> f (HeistConfig m)
hcAttributeSplices = hcSpliceConfig . scAttributeSplices 


------------------------------------------------------------------------------
-- | Lens for template locations
-- :: Simple Lens (HeistConfig m) [TemplateLocation]
hcTemplateLocations
    :: Functor f
    => ([TemplateLocation] -> f [TemplateLocation])
    -> HeistConfig m -> f (HeistConfig m)
hcTemplateLocations = hcSpliceConfig . scTemplateLocations 


------------------------------------------------------------------------------
-- | Lens for compiled template filter
-- :: Simple Lens (SpliceConfig m) (TBool -> Bool)
hcCompiledTemplateFilter
    :: Functor f
    => ((TPath -> Bool) -> f (TPath -> Bool))
    -> HeistConfig m -> f (HeistConfig m)
hcCompiledTemplateFilter = hcSpliceConfig . scCompiledTemplateFilter


