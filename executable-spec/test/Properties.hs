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
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq ((:|>), Empty), (|>))
import           GHC.Exts (toList)
import           GHC.Generics (Generic)

import           Data.Tree
import           Test.QuickCheck

import           Cardano.Ledger.Assert (prettyShow)

import           SUTTest


-- | Given a test setup, generate a tree of traces based on that test setup.
genTraceTree
  :: (TestGen s t)
  => TestSetup t -> Gen (Tree (Trace s t))
genTraceTree testSetup = do
  trace <- genTrace testSetup
  pure $ unfoldTree (\trace' -> (trace', shrinkTrace trace')) trace

-- | TODO: Constraint @Arbitrary (SUTAct s)@ is needed for shrinking the trace. If
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
     , Show (TestSetup t)
     , Show (SUTSt s)
     , Show (SUTAct s)
     , TestGen s t
     )
  => (Trace s t -> Bool) -> Property
traceProp prop = tracePropShow prop prettyShow

tracePropShow
  :: forall s t
   . ( Arbitrary (TestSetup t)
     , TestGen s t
     )
  => (Trace s t -> Bool) -> (Trace s t -> String) -> Property
tracePropShow prop customShow =
  forAllShrinkShow
    arbitrary
    shrink
    (\(Node trace _) -> customShow trace)
    (\(Node trace _) -> prop trace)

forAllSequencesOfActions
  :: forall s t
   . ( Arbitrary (TestSetup t)
     -- TODO: we might want to bundle these constraints in the @TestGen@ class.
     , Show (TestSetup t)
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
  go initGenSt (bootstrap testSetup initSUTSt)
  where
    go :: GenSt t -> Trace s t -> Gen (Trace s t)
    go genSt trace
      | canTerminate @s genSt = pure trace
      | otherwise             = do
          nextAct <- genAct testSetup genSt
          let trace'    = extend trace nextAct
              nextGenSt = update genSt (latestState trace')
          go nextGenSt trace'

--------------------------------------------------------------------------------
-- Trace shrinking
--------------------------------------------------------------------------------

-- | NOTE: we do not shrink the actions inside the trace.
shrinkTrace
  :: SUT s => Trace s t -> [Trace s t]
shrinkTrace trace
  = fmap (reconstruct (testSetup trace) (initialState trace))
  $ shrinkList shrinkNothing (allActions trace)

--------------------------------------------------------------------------------
-- Trace
--------------------------------------------------------------------------------

-- | Trace for SUT @s@ generated using test generator @t@.
data Trace s t =
  Trace
  { testSetup    :: !(TestSetup t)
  , initialState :: !(SUTSt s)
  , transitions  :: !(Seq (TraceEvent s))
    -- ^ Trace events. They appear in an oldest-first order. This is: the events
    -- appear in the order they occurred.
  } deriving (Generic)

deriving instance
  (Eq (SUTSt s), Eq (TraceEvent s), Eq (TestSetup t))       => Eq (Trace s t)
deriving instance
  (Show (SUTSt s), Show (TraceEvent s), Show (TestSetup t)) => Show (Trace s t)

-- | Trace events.
data TraceEvent s
  = Invalid !(SUTAct s)
  | Valid
    { action       :: !(SUTAct s)
    , ensuingState :: !(SUTSt s)
    }

deriving instance (Eq (SUTAct s), Eq (SUTSt s))     => Eq (TraceEvent s)
deriving instance (Show (SUTAct s), Show (SUTSt s)) => Show (TraceEvent s)

validEvent :: TraceEvent s -> Bool
validEvent (Invalid {}) = False
validEvent (Valid {})   = True

extractAction :: TraceEvent s -> SUTAct s
extractAction (Invalid act)    = act
extractAction (Valid { action }) = action

extractState :: TraceEvent s -> SUTSt s
extractState (Invalid {})               = error "Cannot extract an invalid state"
extractState (Valid   { ensuingState }) = ensuingState

validTransitions :: Trace s t -> [(SUTAct s, SUTSt s)]
validTransitions Trace { transitions } =
  fmap asPair $ filter validEvent $ toList transitions
  where
    asPair Invalid {}  = error "This function should be called with valid events only."
    asPair (Valid a s) = (a, s)

-- | Initialize the trace with an initial state.
bootstrap :: TestSetup t -> SUTSt s -> Trace s t
bootstrap setup st =
  Trace
  { testSetup    = setup
  , initialState = st
  , transitions  = Empty
  }

-- | Return the latest valid state of a trace.
latestState :: Trace s t -> SUTSt s
latestState Trace { initialState, transitions } =
  fromMaybe initialState (latestStateOf transitions)
  where
    latestStateOf Empty                   = Nothing
    latestStateOf (ts :|> (Invalid _ ))   = latestStateOf ts
    latestStateOf (_  :|> (Valid _ st' )) = Just st'

-- | Return all the __valid__ actions in the trace, where the oldest action is
-- returned first.
validActions :: forall s t . Trace s t -> [SUTAct s]
validActions Trace { transitions } =
  fmap extractAction $ filter validEvent $ toList transitions

-- | Return __all__ the actions in the trace, both __valid__ and __invalid__.
allActions :: Trace s t -> [SUTAct s]
allActions Trace { transitions }=
  fmap extractAction $ toList transitions

validStates :: Trace s t -> [SUTSt s]
validStates Trace { initialState, transitions } =
  initialState : (fmap extractState $ filter validEvent  $ toList transitions)

-- | Try to reconstruct a trace using the initial state and provided actions.
reconstruct
  :: SUT s
  => TestSetup t
  -- ^ Test setup that originated the trace.
  -> SUTSt s
  -- ^ Initial state.
  -> [SUTAct s]
  -- ^ Actions to apply to the initial state.
  -> Trace s t
reconstruct setup st0 acts = foldl' extend (bootstrap setup st0) acts

extend :: SUT s => Trace s t -> SUTAct s -> Trace s t
extend trace act =
  trace { transitions =
          transitions trace |> case apply act (latestState trace) of
                                 Nothing     -> Invalid act
                                 Just nextSt -> Valid   act nextSt
        }
