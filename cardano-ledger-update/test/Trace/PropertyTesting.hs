{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Trace.PropertyTesting
  ( forAllTracesShow
  , Check (Check)
  )
where

import           Test.QuickCheck (Arbitrary, Property, Testable,
                     forAllShrinkShow, property)
import           Test.QuickCheck.Property (failed, reason, succeeded)

import           Cardano.Ledger.Assert (Assertion, runAssertion, showErrors)

import           Trace
import           Trace.Generation
import           Trace.Scenario


-- TODO: we need to find a way of testing that the SUT action generator we're
-- using can generate actions that can violate the trace property. Otherwise we
-- might have a generator that generates only traces that satisfy the given
-- property, thus failing to uncover a potential error in the SUT.

forAllTracesShow
  :: forall s t prop
   . ( Arbitrary (Scenario t)
     , HasScenario s t
     , Testable prop
     )
  => (Trace s t -> prop) -> (Trace s t -> String) -> Property
forAllTracesShow prop customShow =
  forAllShrinkShow arbitraryTrace shrinkTrace customShow prop

-- | Type wrapper around assertions to avoid defining an orphan 'Testable'
-- instance.
newtype Check = Check Assertion

instance Testable Check where
  property (Check prop) =
    case runAssertion prop of
      Nothing   -> property succeeded
      Just errs -> property $ failed { reason = showErrors errs }
