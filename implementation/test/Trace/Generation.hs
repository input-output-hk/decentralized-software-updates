{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for trace generation of systems under tests.
module Trace.Generation
  ( elaborateTrace
  , arbitraryTrace
  , shrinkTrace
  )
where

import           Test.QuickCheck (Arbitrary, Gen, arbitrary, shrink)

import           Trace
import           Trace.Scenario

-- | Elaborate a trace of a system under test given a scenario.
--
-- If the system under test has a 'HasScenario' instance, we can map a test setup
-- to a trace. In other words, the scenario uniquely determines the trace.
elaborateTrace :: forall s t . HasScenario s t => Scenario t -> Trace s t
elaborateTrace testSetup = go initElaboratorSt (bootstrap testSetup initSUTSt)
  where
    (initElaboratorSt, initSUTSt) = mkInitStates testSetup

    go :: ElaboratorSt t -> Trace s t -> Trace s t
    go genSt trace
      | canTerminate @s genSt = trace
      | otherwise             =
        let (nAct, nextElaboratorSt) = nextAct testSetup genSt in
          go nextElaboratorSt (extend trace nAct)

arbitraryTrace :: (Arbitrary (Scenario t), HasScenario s t) => Gen (Trace s t)
arbitraryTrace = fmap elaborateTrace arbitrary

shrinkTrace :: (Arbitrary (Scenario t), HasScenario s t) => Trace s t -> [Trace s t]
shrinkTrace = fmap elaborateTrace . shrink . scenario
