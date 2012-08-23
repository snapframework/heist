{-|

Compiled splices are similar to the original Heist (interpreted) splices, but
without the high performance costs of traversing a DOM at runtime.  Compiled
splices do all of their DOM processing at load time.  They are compiled to
produce a runtime computation that generates a ByteString Builder.  This
preserves the ability to write splices that access runtime information from
the HTTP request, database, etc.

-}

module Heist.Compiled
  (
  -- * High level compiled splice API
    Splice
  , renderCompiledTemplate
  , mapPromises
  , promiseChildren
  , promiseChildrenWith
  , promiseChildrenWithTrans
  , promiseChildrenWithText
  , promiseChildrenWithNodes

  -- * Other kinds of Splice construction
  -- $yieldOverview
  , yieldPure
  , yieldPureText
  , yieldRuntime
  , yieldRuntimeText
  , yieldRuntimeEffect
  , yieldLater
  , yieldPromise
  , addSplices

  -- * Lower level promise functions
  , Promise
  , promise
  , newEmptyPromise
  , newEmptyPromiseWithError
  , runtimeSplicePromise
  , withPromise

  -- * RuntimeSplice functions
  , getPromise
  , putPromise
  , adjustPromise
  , codeGen

  -- * Running nodes and splices
  , runNodeList
  , runNode
  , compileNode
  , runSplice

  ) where

import Heist.Compiled.Internal

-- $yieldOverview
-- The internals of the Chunk data type are deliberately not exported because
-- we want to hide the underlying implementation as much as possible.  The
-- @yield...@ functions give you the ability to construct Splices from
-- different types of input.
