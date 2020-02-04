{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Chain.Body where

import           Data.Function ((&))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst),
                     traceSignals)

import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import           Data.AbstractSize (HasTypeReps)

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme)
import           Cardano.Ledger.Spec.Classes.Sizeable (HasSize, Size, Sizeable,
                     size)
import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)


data BODY p

data BBody p
 = BBody
   { transactions :: ![Signal (TRANSACTION p)]
   }
   deriving (Show, Generic)

instance ( Hashable p
         , HasSigningScheme p
         , STS (TRANSACTION p)
         ) => STS (BODY p) where

  type Environment (BODY p) = Environment (TRANSACTION p)

  type State (BODY p) = State (TRANSACTION p)

  type Signal (BODY p) = BBody p

  data PredicateFailure (BODY p)
    =
     BodyFailure (PredicateFailure (TRANSACTION p))
    deriving (Eq, Show)


  initialRules = [
    ]

  transitionRules = [
    do
      TRC ( env
          , st
          , BBody {transactions}
          ) <- judgmentContext
      case transactions of
        [] -> pure $! st
        (tx:txs') -> do
          st' <- trans @(TRANSACTION p)
              $ TRC ( env, st, tx)
          trans @(BODY p) $ TRC ( env, st', BBody txs')
    ]


instance ( STS (TRANSACTION p), STS (BODY p)
         ) => Embed (TRANSACTION p) (BODY p) where
  wrapFailed = BodyFailure

-- | Block body generator.
gen
  :: ( STS.Gen.HasTrace (TRANSACTION p) ()
     , Sizeable p
     , HasSize p (Transaction.Tx p)
     )
  => Size p
  -> Environment (BODY p)
  -> State (BODY p)
  -> QC.Gen (BBody p)
gen maximumBlockSize transactionEnv transactionSt = do
  transactions <-
    transactionsGen maximumBlockSize transactionEnv transactionSt
  pure $! BBody transactions

-- | Shrink a block body signal.
shrink
  :: forall p
   . ( STS.Gen.HasTrace (TRANSACTION p) ())
  => BBody p -> [BBody p]
shrink body =
  BBody <$> QC.shrinkList
             (STS.Gen.shrinkSignal @(TRANSACTION p) @())
             (transactions body)

-- | Generate a list of 'Tx's that fit in the given maximum size.
transactionsGen
  :: forall p
   . ( STS.Gen.HasTrace (TRANSACTION p) ()
     , Sizeable p
     , HasSize p (Transaction.Tx p)
     )
  => Size p
  -> Environment (BODY p)
  -> State (BODY p)
  -> QC.Gen [Transaction.Tx p]
transactionsGen maximumSize env st
  =   fitTransactions maximumSize . traceSignals OldestFirst
  <$> STS.Gen.traceFrom @(TRANSACTION p) () 30 () env st

-- | Return the transactions that fit in the given maximum block size.
fitTransactions
  :: forall p
   . ( Sizeable p
     , HasSize p (Transaction.Tx p)
     )
  => Size p -> [Transaction.Tx p] -> [Transaction.Tx p]
fitTransactions maximumSize txs
  = zip txs (tail sizes)
  -- We subtract to account for the block constructor and the 'Word64' value of
  -- the slot.
  --
  & takeWhile ((< maximumSize) . snd)
  & fmap fst
  where
    -- We compute the cumulative sum of the transaction sizes. We add 3 to
    -- account for the list constructor.
    --
    sizes :: [Size p]
    sizes = scanl (\acc tx -> acc + size tx) 0 txs

deriving instance ( Typeable p
                  , HasTypeReps (Signal (TRANSACTION p))
                  ) => HasTypeReps (BBody p)

instance ( Typeable p
         , Sized (Signal (TRANSACTION p))
         ) => Sized (BBody p) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION p))
