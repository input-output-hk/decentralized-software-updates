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
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen


import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (genTrace, sigGen)
import           Control.State.Transition.Trace (TraceOrder (OldestFirst),
                     traceSignals)
import           Data.AbstractSize (HasTypeReps)

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (Coin (Coin))
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
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

-- | Generate a list of 'Tx's that fit in the given maximum size.
--
-- TODO: this seems to belong to transactions ...
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
  -> Gen [Transaction.Tx hashAlgo]
transactionsGen maximumSize env st
  =   fitTransactions . traceSignals OldestFirst
  -- TODO: check what is a realistic distribution for empty blocks, or disallow
  -- the generation of empty blocks altogether.
  <$> genTrace @(TRANSACTION hashAlgo) 30 env st transactionGen

  where
    -- Fit the transactions that fit in the given maximum block size.
    fitTransactions :: [Transaction.Tx hashAlgo] -> [Transaction.Tx hashAlgo]
    fitTransactions txs = zip txs (tail sizes)
                          -- We subtract to account for the block constructor
                          -- and the 'Word64' value of the slot.
                        & takeWhile ((< maximumSize - 5) . snd)
                        & fmap fst
      where
        -- We compute the cumulative sum of the transaction sizes. We add 3 to
        -- account for the list constructor.
        sizes :: [Size]
        sizes = scanl (\acc tx -> acc + size tx + 3) 0 txs

    transactionGen  (Transaction.Env { Transaction.k
                                     , Transaction.currentSlot
                                     , Transaction.asips
                                     , Transaction.participants
                                     , Transaction.apprvsips
                                     }
                    )
                    (Transaction.St { Transaction.subsips = subsips
                                    , Transaction.wssips = wssips
                                    , Transaction.wrsips = wrsips
                                    , Transaction.sipdb = sipdb
                                    , Transaction.ballots = ballots
                                    , Transaction.implementationSt = implementationSt
                                    }
                    )
      -- TODO: figure out what a realistic distribution for update payload is.
      --
      -- TODO: do we need to model the __liveness__ assumption of the underlying
      -- protocol? That is, model the fact that honest party votes will be
      -- eventually comitted to the chain. Or is this implicit once we start
      -- generating votes uniformly distributed over all the parties (honest and
      -- otherwise)
      --
      -- We do not generate witnesses for now
      =   (`Transaction.Tx` [])
      .   dummyBody
      <$> Gen.frequency
            [ (9, pure $! []) -- We don't generate payload in 9/10 of the cases.
            , (1, sigGen
                    @(UPDATES hashAlgo)
                    Update.Env { Update.k = k
                               , Update.currentSlot = currentSlot
                               , Update.asips = asips
                               , Update.participants = participants
                               , Update.apprvsips = apprvsips
                               }
                    Update.St { Update.subsips = subsips
                              , Update.wssips = wssips
                              , Update.wrsips = wrsips
                              , Update.sipdb = sipdb
                              , Update.ballots = ballots
                              , Update.implementationSt = implementationSt
                              }
              )
            ]
      where
        -- For now we don't generate inputs and outputs.
        dummyBody update
          = Transaction.TxBody
            { Transaction.inputs = mempty
            , Transaction.outputs = mempty
            , Transaction.fees = Coin
            , Transaction.update = update
            }
