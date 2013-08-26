{-|

Compiled splices are similar to the original Heist (interpreted) splices, but
without the high performance costs of traversing a DOM at runtime.  Compiled
splices do all of their DOM processing at load time.  They are compiled to
produce a runtime computation that generates a ByteString Builder.  This
preserves the ability to write splices that access runtime information from
the HTTP request, database, etc.

If you import both this module and "Heist.Interpreted" in the same file, then
you will need to import them qualified.

-}

module Heist.Compiled
  (
  -- * High level compiled splice API
    Splice
  , renderTemplate
  , codeGen
  , runChildren

  -- * Functions for manipulating lists of compiled splices
  , textSplice
  , nodeSplice
  , pureSplice

  , deferMany
  , deferMap
  , mayDeferMap
  , bindLater
  , withSplices
  , manyWithSplices
  , withLocalSplices

  -- * Constructing Chunks
  -- $yieldOverview
  , yieldPure
  , yieldRuntime
  , yieldRuntimeEffect
  , yieldPureText
  , yieldRuntimeText

  -- * Running nodes and splices
  , runNodeList
  , runNode
  , runAttributes
  , runAttributesRaw
  , callTemplate

  ) where

import Heist.Compiled.Internal

-- $yieldOverview
-- The internals of the Chunk data type are deliberately not exported because
-- we want to hide the underlying implementation as much as possible.  The
-- @yield...@ functions give you lower level construction of DLists of Chunks.
--
-- Most of the time you will use these functions composed with return to
-- generate a Splice.  But we decided not to include the return in these
-- functions to allow you to work with the DLists purely.
