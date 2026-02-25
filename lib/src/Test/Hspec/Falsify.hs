-- | Support for @falsify@ in the @hspec@ framework
module Test.Hspec.Falsify (
    -- * Running falsify properties in hspec
    prop
  , propWith
    -- * Configuration
  , module Test.Falsify.Standalone
    -- * Re-exports
  , module Test.Falsify.Property
    -- ** Generators
  , Gen
    -- ** Functions
  , pattern Gen.Fn
  , pattern Gen.Fn2
  , pattern Gen.Fn3
  ) where

import Data.Default
import Test.Hspec (Spec, expectationFailure, it)

import Test.Falsify.Generator (Gen)
import Test.Falsify.Property
import Test.Falsify.Standalone

import qualified Test.Falsify.Reexported.Generator.Function as Gen

-- | Create an hspec test case from a falsify property
prop :: String -> Property' String () -> Spec
prop = propWith def

-- | Create an hspec test case from a falsify property with custom options
propWith :: Options -> String -> Property' String () -> Spec
propWith opts name property = it name $ do
    result <- runWith opts property
    case result of
      TestPassed -> return ()
      TestFailed failure ->
        expectationFailure $ failureError failure
