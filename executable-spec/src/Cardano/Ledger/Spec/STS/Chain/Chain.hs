{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount (BlockCount), Slot (Slot))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Generators (currentSlotGen, kGen,
                     participantsGen)
import           Cardano.Ledger.Spec.STS.Chain.Body (BODY)
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import           Cardano.Ledger.Spec.STS.Chain.Header (HEADER)
import qualified Cardano.Ledger.Spec.STS.Chain.Header as Header
import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation


data CHAIN hashAlgo


data Env hashAlgo
  = Env
    { k :: !BlockCount
    , maximumBlockSize :: !Size
      -- ^ Maximum block size. The interpretation of this value depends on the
      -- instance of 'Sized'.
      --
      -- TODO: use abstract size instead.
    , initialSlot :: !Slot
    , participants :: !(Bimap Core.VKey Core.SKey)
    }
    deriving (Eq, Show)


data St hashAlgo
  -- TODO: DISCUSS: here I think it doesn't make sense to have a header state
  -- and body state. We have that both states contain variables which we will
  -- have to keep in sync in the CHAIN rules if we duplicate them. So it's
  -- better just to expand the inner components, and reduce duplication.
  = St
    { currentSlot :: !Slot
    , subsips :: !(Map (Data.Commit hashAlgo) (Data.SIP hashAlgo))
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , wssips :: !(Map (Data.Commit hashAlgo) Slot)
    , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
    , voteResultSIPs :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
    , implementationSt :: !(State IMPLEMENTATION)
    , utxoSt :: !(State UTXO)
    }
    deriving (Eq, Show)


data Block hashAlgo
  = Block
    { header :: Signal (HEADER hashAlgo)
    , body :: Signal (BODY hashAlgo)
    }
    deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  ) => HasTypeReps (Block hashAlgo)


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => Sized (Block hashAlgo) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION hashAlgo))


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => STS (CHAIN hashAlgo) where

  type Environment (CHAIN hashAlgo) = Env hashAlgo

  type State (CHAIN hashAlgo) = St hashAlgo

  type Signal (CHAIN hashAlgo) = Block hashAlgo

  data PredicateFailure (CHAIN hashAlgo)
    = MaximumBlockSizeExceeded Size (Threshold Size)
    | ChainFailureBody (PredicateFailure (BODY hashAlgo))
    | ChainFailureHeader (PredicateFailure (HEADER hashAlgo))
    deriving (Eq, Show)


  initialRules = [ do
    IRC Env { initialSlot } <- judgmentContext
    pure St { currentSlot = initialSlot
            , subsips = Map.empty
            , asips = Map.empty
            , wssips = Map.empty
            , wrsips = Map.empty
            , ballots = Map.empty
            , voteResultSIPs = Map.empty
            , implementationSt = Implementation.St ()
            , utxoSt = UTxO.St ()
            }
    ]

  transitionRules = [
    do
      TRC ( Env { k
                , maximumBlockSize
                , participants
                }
          , St  { currentSlot
                , subsips
                , asips
                , wssips
                , wrsips
                , ballots
                , voteResultSIPs
                , implementationSt
                , utxoSt
                }
          , block@Block{ header, body }
          ) <- judgmentContext
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)

      -- First a HEAD transition in order to update the state
      headerSt'@Header.St { Header.currentSlot = currentSlot'
                          , Header.wrsips = wrsips'
                          , Header.asips = asips'
                          }  <-
        trans @(HEADER hashAlgo)
          $ TRC ( Header.Env { Header.k = k }
                , Header.St { Header.currentSlot = currentSlot
                            , Header.wrsips = wrsips
                            , Header.asips = asips
                            }
                , header
                )

      -- Second a BODY transition with the updated state from header
      Transaction.St
        { Transaction.subsips = subsips'
        , Transaction.wssips = wssips'
        , Transaction.wrsips = wrsips'
        , Transaction.ballots = ballots'
        , Transaction.voteResultSIPs = voteResultSIPs'
        , Transaction.implementationSt = implementationSt'
        , Transaction.utxoSt = utxoSt'
        } <- trans @(BODY hashAlgo)
               $ TRC ( Transaction.Env
                          { Transaction.k = k
                          , Transaction.currentSlot = currentSlot'
                          , Transaction.asips = asips'
                          , Transaction.participants = participants
                          , Transaction.utxoEnv = UTxO.Env
                          }
                      , Transaction.St
                          { Transaction.subsips = subsips
                          , Transaction.wssips = wssips
                          , Transaction.wrsips = wrsips
                          , Transaction.ballots = ballots
                          , Transaction.voteResultSIPs = voteResultSIPs
                          , Transaction.implementationSt = implementationSt
                          , Transaction.utxoSt = utxoSt
                          }
                      , body
                     )
      pure $! St { currentSlot = currentSlot'
                 , subsips = subsips'
                 , asips = asips'
                 , wssips = wssips'
                 , wrsips = wrsips'
                 , ballots = ballots'
                 , voteResultSIPs = voteResultSIPs'
                 , implementationSt = implementationSt'
                 , utxoSt = utxoSt'
                 }
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => Embed (BODY hashAlgo) (CHAIN hashAlgo) where
  wrapFailed = ChainFailureBody

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => Embed (HEADER hashAlgo) (CHAIN hashAlgo) where
  wrapFailed = ChainFailureHeader


instance ( HasTypeReps hashAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps (Data.Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         ) => HasTrace (CHAIN hashAlgo) where

  envGen _traceLength
    = Env
    <$> kGen
    <*> maxBlockSizeGen
    <*> currentSlotGen
    <*> participantsGen
    where
      -- For now we fix the maximum block size to an abstract size of 100
      maxBlockSizeGen = pure 100

  sigGen Env { k
             , maximumBlockSize
             , participants
             }
         St { currentSlot
            , subsips
            , asips
            , wssips
            , wrsips
            , ballots
            , voteResultSIPs
            , implementationSt
            , utxoSt
            }
    = Block
    <$> Header.headerGen currentSlot
    <*> transactionsGen
    where
      transactionsGen =
        Body.BBody <$> Body.transactionsGen
                         maximumBlockSize
                         Transaction.Env { Transaction.k = k
                                         , Transaction.currentSlot = currentSlot
                                         , Transaction.asips = asips
                                         , Transaction.participants = participants
                                         , Transaction.utxoEnv = UTxO.Env
                                         }
                         Transaction.St { Transaction.subsips = subsips
                                        , Transaction.wssips = wssips
                                        , Transaction.wrsips = wrsips
                                        , Transaction.ballots = ballots
                                        , Transaction.voteResultSIPs = voteResultSIPs
                                        , Transaction.implementationSt = implementationSt
                                        , Transaction.utxoSt = utxoSt
                                        }
