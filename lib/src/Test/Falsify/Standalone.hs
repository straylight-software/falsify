-- | Standalone property runner
module Test.Falsify.Standalone (
    -- * Running properties
    check
  , checkWith
  , runWith
    -- * Running IO properties
  , checkIO
  , checkIOWith
  , runIOWith
    -- * Options
  , Options(..)
    -- * Results
  , TestResult(..)
  , TestFailure(..)
  ) where

import Data.Bifunctor
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import GHC.Exception (prettyCallStack)

import qualified Data.List.NonEmpty as NE

import Test.Falsify.Internal.Driver (Options(..))
import Test.Falsify.Internal.Driver.ReplaySeed (ReplaySeed)
import Test.Falsify.Internal.Generator.Shrinking (shrinkHistory, shrinkOutcome)
import Test.Falsify.Internal.Property (Log(..), LogEntry(..), Property', TestRun(..))

import qualified Test.Falsify.Internal.Driver as Driver

-- | Result of running a property
data TestResult =
    -- | All tests passed
    TestPassed

    -- | A counter-example was found
  | TestFailed TestFailure
  deriving (Show)

-- | Information about a test failure
data TestFailure = TestFailure {
      -- | The (shrunk) counter-example error message
      failureError :: String

      -- | The log from the failing test run (includes Prop.info messages)
    , failureLog :: String

      -- | The seed that can be used to replay this failure
    , failureSeed :: ReplaySeed

      -- | The full shrink history (from original to most-shrunk)
    , failureShrinkHistory :: NonEmpty String
    }
  deriving (Show)

-- | Run a property with default options (100 tests, unlimited shrinks)
check :: Property' String () -> IO Bool
check = checkWith def

-- | Run a property with custom options
checkWith :: Options -> Property' String () -> IO Bool
checkWith opts = fmap isPassed . runWith opts
  where
    isPassed TestPassed = True
    isPassed _          = False

-- | Run a property and return the full result
runWith :: Options -> Property' String () -> IO TestResult
runWith opts prop = do
    (_seed, _successes, _discarded, mFailure) <- Driver.falsify opts prop
    return $ case mFailure of
      Nothing -> TestPassed
      Just f ->
        let explanation = Driver.failureRun f
            history = shrinkHistory $ first fst $ explanation
            ((_err, finalRun), _rejected) = shrinkOutcome explanation
         in TestFailed
              TestFailure {
                  failureError        = NE.last history
                , failureLog          = renderLog (runLog finalRun)
                , failureSeed         = Driver.failureSeed f
                , failureShrinkHistory = history
                }

-- | Render a log to a string
renderLog :: Log -> String
renderLog (Log entries) = unlines $ map renderLogEntry (reverse entries)
  where
    renderLogEntry (Generated stack x) =
      "generated " ++ x ++ " at " ++ prettyCallStack stack
    renderLogEntry (Info x) = x

{-------------------------------------------------------------------------------
  IO Properties
  
  These functions run properties that generate IO actions. The property
  generates an IO action which is then executed. If the IO action throws
  an exception, the test fails. This allows testing IO code while maintaining
  falsify's shrinking capabilities.
-------------------------------------------------------------------------------}

-- | Run an IO property with default options
--
-- The property generates an IO action. The test passes if the IO action
-- completes without throwing an exception.
--
-- === Example
--
-- @
-- prop :: Property' String (IO ())
-- prop = do
--     x <- gen $ Gen.int (Range.between (0, 100))
--     return $ do
--         result <- runMyIOCode x
--         when (result < 0) $ throwIO (userError "negative result")
-- @
checkIO :: Property' String (IO ()) -> IO Bool
checkIO = checkIOWith def

-- | Run an IO property with custom options
checkIOWith :: Options -> Property' String (IO ()) -> IO Bool
checkIOWith opts = fmap isPassed . runIOWith opts
  where
    isPassed TestPassed = True
    isPassed _          = False

-- | Run an IO property and return the full result
--
-- This runs the property multiple times. For each run:
-- 1. Generate an IO action using the property
-- 2. Execute the IO action
-- 3. If it throws an exception, the test fails (and shrinking begins)
-- 4. If it completes successfully, move to the next test
runIOWith :: Options -> Property' String (IO ()) -> IO TestResult
runIOWith opts prop = do
    (_seed, _successes, _discarded, mFailure) <- Driver.falsifyIO opts prop
    return $ case mFailure of
      Nothing -> TestPassed
      Just f ->
        let explanation = Driver.failureRun f
            history = shrinkHistory $ first fst $ explanation
            ((_err, finalRun), _rejected) = shrinkOutcome explanation
         in TestFailed
              TestFailure {
                  failureError        = NE.last history
                , failureLog          = renderLog (runLog finalRun)
                , failureSeed         = Driver.failureSeed f
                , failureShrinkHistory = history
                }
