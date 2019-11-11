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

import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
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


data BODY hashAlgo dsignAlgo

data BBody hashAlgo dsignAlgo
 = BBody
   { transactions :: ![Signal (TRANSACTION hashAlgo dsignAlgo)]
   }
   deriving (Eq, Show, Generic)

deriving instance ( Typeable dsignAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
                  ) => HasTypeReps (BBody hashAlgo dsignAlgo)

instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
         ) => Sized (BBody hashAlgo dsignAlgo) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION hashAlgo dsignAlgo))


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => STS (BODY hashAlgo dsignAlgo) where

  type Environment (BODY hashAlgo dsignAlgo) = Environment (TRANSACTION hashAlgo dsignAlgo)

  type State (BODY hashAlgo dsignAlgo) = State (TRANSACTION hashAlgo dsignAlgo)

  type Signal (BODY hashAlgo dsignAlgo) = BBody hashAlgo dsignAlgo

  data PredicateFailure (BODY hashAlgo dsignAlgo)
    =
     BodyFailure (PredicateFailure (TRANSACTION hashAlgo dsignAlgo))
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
          st' <- trans @(TRANSACTION hashAlgo dsignAlgo)
              $ TRC ( env, st, tx)
          trans @(BODY hashAlgo dsignAlgo) $ TRC ( env, st', BBody txs')
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         ) => Embed (TRANSACTION hashAlgo dsignAlgo) (BODY hashAlgo dsignAlgo) where
  wrapFailed = BodyFailure

-- | Block body generator.
gen
  :: ( Typeable dsignAlgo
     , HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Data.Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo Data.SIPData)
     , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
     )
  => Size
  -> Environment (BODY hashAlgo dsignAlgo)
  -> State (BODY hashAlgo dsignAlgo)
  -> QC.Gen (BBody hashAlgo dsignAlgo)
gen maximumBlockSize transactionEnv transactionSt = do
  transactions <-
    transactionsGen maximumBlockSize transactionEnv transactionSt
  pure $! BBody transactions

-- | Shrink a block body signal.
shrink
  :: forall hashAlgo dsignAlgo
   . ( HashAlgorithm hashAlgo )
  => BBody hashAlgo dsignAlgo -> [BBody hashAlgo dsignAlgo]
shrink body =
  BBody <$> QC.shrinkList
             (STS.Gen.shrinkSignal @(TRANSACTION hashAlgo dsignAlgo) @())
             (transactions body)

-- | Generate a list of 'Tx's that fit in the given maximum size.
transactionsGen
  :: forall hashAlgo dsignAlgo
   . ( Typeable dsignAlgo
     , HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Data.Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo Data.SIPData)
     , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
     )
  => Size
  -> Environment (BODY hashAlgo dsignAlgo)
  -> State (BODY hashAlgo dsignAlgo)
  -> QC.Gen [Transaction.Tx hashAlgo dsignAlgo]
transactionsGen maximumSize env st
  =   fitTransactions maximumSize . traceSignals OldestFirst
  <$> STS.Gen.traceFrom @(TRANSACTION hashAlgo dsignAlgo) 30 () env st

-- | Return the transactions that fit in the given maximum block size.
fitTransactions
  :: ( Typeable dsignAlgo
     , HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Data.Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo Data.SIPData)
     , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
     )
  => Size -> [Transaction.Tx hashAlgo dsignAlgo] -> [Transaction.Tx hashAlgo dsignAlgo]
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
