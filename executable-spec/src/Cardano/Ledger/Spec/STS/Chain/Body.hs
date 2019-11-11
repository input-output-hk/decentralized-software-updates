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
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst),
                     traceSignals)

import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import           Data.AbstractSize (HasTypeReps)

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data


data BODY hashAlgo

data BBody hashAlgo
 = BBody
   { transactions :: ![Signal (TRANSACTION hashAlgo)]
   }
   deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  ) => HasTypeReps (BBody hashAlgo)

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => Sized (BBody hashAlgo) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION hashAlgo))


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => STS (BODY hashAlgo) where

  type Environment (BODY hashAlgo) = Environment (TRANSACTION hashAlgo)

  type State (BODY hashAlgo) = State (TRANSACTION hashAlgo)

  type Signal (BODY hashAlgo) = BBody hashAlgo

  data PredicateFailure (BODY hashAlgo)
    =
     BodyFailure (PredicateFailure (TRANSACTION hashAlgo))
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
          st' <- trans @(TRANSACTION hashAlgo)
              $ TRC ( env, st, tx)
          trans @(BODY hashAlgo) $ TRC ( env, st', BBody txs')
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         ) => Embed (TRANSACTION hashAlgo) (BODY hashAlgo) where
  wrapFailed = BodyFailure

-- | Block body generator.
gen
  :: ( HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Data.Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo Data.SIPData)
     )
  => Size
  -> Environment (BODY hashAlgo)
  -> State (BODY hashAlgo)
  -> QC.Gen (BBody hashAlgo)
gen maximumBlockSize transactionEnv transactionSt = do
  transactions <-
    transactionsGen maximumBlockSize transactionEnv transactionSt
  pure $! BBody transactions

-- | Shrink a block body signal.
shrink
  :: forall hashAlgo
   . ( HashAlgorithm hashAlgo )
  => BBody hashAlgo -> [BBody hashAlgo]
shrink body =
  BBody <$> QC.shrinkList
             (STS.Gen.shrinkSignal @(TRANSACTION hashAlgo) @())
             (transactions body)

-- | Generate a list of 'Tx's that fit in the given maximum size.
transactionsGen
  :: forall hashAlgo
   . ( HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Data.Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo Data.SIPData)
     )
  => Size
  -> Environment (BODY hashAlgo)
  -> State (BODY hashAlgo)
  -> QC.Gen [Transaction.Tx hashAlgo]
transactionsGen maximumSize env st
  =   fitTransactions maximumSize . traceSignals OldestFirst
  <$> STS.Gen.traceFrom @(TRANSACTION hashAlgo) 30 () env st

-- | Return the transactions that fit in the given maximum block size.
fitTransactions
  :: ( HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Data.Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo Data.SIPData)
     )
  => Size -> [Transaction.Tx hashAlgo] -> [Transaction.Tx hashAlgo]
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
    sizes :: [Size]
    sizes = scanl (\acc tx -> acc + size tx) 0 txs
