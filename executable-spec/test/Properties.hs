{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Draft design of a new approach for property-testing the update mechanism.
module Properties where

import           Data.List (foldl')
import           Data.Sequence (Seq ((:|>), Empty), (|>))
import           GHC.Exts (toList)
import           GHC.Generics (Generic)

import           Data.Tree
import           Test.QuickCheck

import           SUTTest


genTraceTree
  :: (TestGen s t)
  => TestSetup t -> Gen (Tree (Trace s t))
genTraceTree testSetup = do
  trace <- genTrace testSetup
  pure $ unfoldTree (\trace' -> (trace', shrinkTrace trace')) trace

-- |
--
-- TODO: Constraint @Arbitrary (SUTAct s)@ is needed for shrinking the trace. If
-- we would only shrink the list of actions and not the actions we wouldn't need
-- it. Shrinking the action might not make much sense, since we're shrinking the
-- test setup, which should constrain which actions are possible.
instance ( Arbitrary (TestSetup t)
         , TestGen s t
         ) => Arbitrary (Tree (Trace s t)) where
  arbitrary = do
    testSetupTree <- arbitraryTree
    applyToTree genTraceTree testSetupTree

  shrink (Node _ xs) = xs

-- | TODO: we need to find a way of testing that the SUT action generator we're
-- using can generate actions that can violate the trace property. Otherwise we
-- might have a generator that generates only traces that satisfy the given
-- property, thus failing to uncover a potential error in the SUT.
traceProp
  :: forall s t
   . ( Arbitrary (TestSetup t)
     , Show (SUTSt s)
     , Show (SUTAct s)
     , TestGen s t
     )
  => (Trace s t -> Bool) -> Property
traceProp prop =
  property $ \(Node trace _) -> prop trace

-- NOTE: this will require the use @TypeApplications@ to disambiguate @t@.
forAllSequencesOfActions
  :: forall s t
   . ( Arbitrary (TestSetup t)
     , Show (SUTSt s)
     , Show (SUTAct s)
     , TestGen s t
     )
  => ([SUTAct s] -> Bool) -> Property
forAllSequencesOfActions prop
  -- All the valid traces should satisfy the property.
  =   traceProp @s @t (prop . validActions)
  -- There is at least one invalid trace that does not satisfy the property.
  --
  -- The generators should generate traces that violate the given property,
  -- otherwise they could be masking off violations of the given property
  -- (@prop@) by the SUT.
  --
  -- TODO: we need to analyze whether this is a sensible constraint. It might
  -- impose heavy restrictions on the action generators, but on the other hand
  -- we want to prevent generators from masking off errors!
 .&&. expectFailure (property $ traceProp @s @t (prop . allActions))


--------------------------------------------------------------------------------
-- Tree generators
--------------------------------------------------------------------------------

arbitraryTree :: Arbitrary a => Gen (Tree a)
arbitraryTree = do
  a <- arbitrary
  pure $ unfoldTree (\a' -> (a', shrink a')) a

applyToTree :: forall a b . (a -> Gen (Tree b)) -> Tree a -> Gen (Tree b)
applyToTree f (Node a as) = do
  Node b bs <- f a
  bs' <- traverse (applyToTree f) as
  -- We try to keep the list sorted on the size of the shrinks: @bs@ contains
  -- the shrinks based on node value @a@, this means that @bs'@ will contain
  -- values generated from @f@ using smaller values.
  --
  -- Note that this does not necessarily guarantee that the shrink list is
  -- increasing in the size of the shrunk values. For instance, function @f@
  -- can return larger values for smaller values of its argument.
  pure $ Node b (bs' ++ bs)

--------------------------------------------------------------------------------
-- Trace generation
--------------------------------------------------------------------------------

genTrace
  :: forall s t
   . TestGen s t
  => TestSetup t -> Gen (Trace s t)
genTrace testSetup = do
  let (initGenSt, initSUTSt) = mkInitStates testSetup
  go initGenSt (bootstrap initSUTSt)
  where
    go :: GenSt t -> Trace s t -> Gen (Trace s t)
    go genSt trace
      | canTerminate @s genSt = pure trace
      | otherwise             = do
          mNextAct <- genAct genSt
          case mNextAct of
            Nothing      ->
              -- TODO: here we have to determine whether we should retry the
              -- action generator. I think the simplest design is to assume that
              -- if the action generator could not generate a valid action in
              -- the given SUT state, then no valid actions can be generated in
              -- that state.
              pure trace
            Just nextAct ->
              let trace'    = extend trace nextAct
                  nextGenSt = update genSt (latestState trace')
              in  go nextGenSt trace'

--------------------------------------------------------------------------------
-- Trace shrinking
--------------------------------------------------------------------------------

-- |
--
-- NOTE: we do not shrink the actions inside the trace.
shrinkTrace
  :: SUT s => Trace s t -> [Trace s t]
shrinkTrace trace =
  fmap (reconstruct (initialState trace)) $ shrinkList shrinkNothing (allActions trace)

--------------------------------------------------------------------------------
-- Trace
--------------------------------------------------------------------------------

-- | Trace for SUT @s@ generated using test generator @t@.
data Trace s t =
  Trace
  { initialState :: !(SUTSt s)
  , transitions  :: !(Seq (TraceEvent s))
  } deriving (Generic)

deriving instance (Eq (SUTSt s), Eq (TraceEvent s))     => Eq (Trace s t)
deriving instance (Show (SUTSt s), Show (TraceEvent s)) => Show (Trace s t)

-- | Trace events.
data TraceEvent s
  = Invalid !(SUTAct s)
  | Valid
    { action       :: !(SUTAct s)
    , ensuingState :: !(SUTSt s)
    }

deriving instance (Eq (SUTAct s), Eq (SUTSt s))     => Eq (TraceEvent s)
deriving instance (Show (SUTAct s), Show (SUTSt s)) => Show (TraceEvent s)

validAction :: TraceEvent s -> Bool
validAction (Invalid {}) = False
validAction (Valid {})   = True

extractAction :: TraceEvent s -> SUTAct s
extractAction (Invalid act)    = act
extractAction (Valid { action }) = action

-- | Initialize the trace with an initial state.
bootstrap :: SUTSt s -> Trace s t
bootstrap st =
  Trace
  { initialState = st
  , transitions  = Empty
  }

latestState :: Trace s t -> SUTSt s
latestState (Trace st Empty                )   = st
latestState (Trace st (ts :|> (Invalid _ )))   = latestState (Trace st ts)
latestState (Trace _  (_  :|> (Valid _ st' ))) = st'

-- | Return all the __valid__ actions in the trace, where the oldest action is
-- returned first.
validActions :: forall s t . Trace s t -> [SUTAct s]
validActions Trace { transitions } =
  fmap extractAction $ filter validAction $ toList transitions

-- | Return __all__ the actions in the trace, both __valid__ and __invalid__.
allActions :: Trace s t -> [SUTAct s]
allActions Trace { transitions }=
  fmap extractAction $ toList transitions

-- | Try to reconstruct a trace using the initial state and provided actions.
reconstruct :: SUT s => SUTSt s -> [SUTAct s] -> Trace s t
reconstruct st0 acts = foldl' extend (bootstrap st0) acts

extend :: SUT s => Trace s t -> SUTAct s -> Trace s t
extend trace act =
  trace { transitions =
          transitions trace |> case apply act (latestState trace) of
                                 Nothing     -> Invalid act
                                 Just nextSt -> Valid   act nextSt
        }
