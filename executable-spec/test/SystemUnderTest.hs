{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SystemUnderTest
  ( SUT
  , SUTAct
  , SUTSt
  , apply
  )
where

-- | System under test.
--
-- This class defines a very simple interface with a system under test.
--
-- The type of the 'apply' function limits the system under test to a collection
-- of pure functions.
class SUT s where

  -- | State of the SUT.
  data SUTSt s

  -- | Actions of the SUT.
  data SUTAct s

  -- | Apply the action to the SUT, and return the ensuing state if the action
  -- could be successfully applied, 'Nothing' otherwise.
  apply :: SUTAct s -> SUTSt s -> Maybe (SUTSt s)
