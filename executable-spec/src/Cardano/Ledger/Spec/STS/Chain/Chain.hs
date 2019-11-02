{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Data.Bimap (Bimap)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set as Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC

import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount, Slot)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Generators (currentSlotGen, kGen,
                     participantsGen, prvNoMajorityGen, prvNoQuorumGen, r_aGen,
                     stakeDistGen)
import qualified Cardano.Ledger.Generators.QuickCheck as Gen.QC
import           Cardano.Ledger.Spec.STS.Chain.Body (BODY)
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import           Cardano.Ledger.Spec.STS.Chain.Header (HEADER)
import qualified Cardano.Ledger.Spec.STS.Chain.Header as Header
import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
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
    , r_a :: !Float
      -- ^ adversary stake ratio
    , stakeDist :: !(Map Core.VKey Data.Stake)
    , prvNoQuorum :: !Word8
      -- How many times a revoting is allowed due to a no quorum result
    , prvNoMajority :: !Word8
      -- How many times a revoting is allowed due to a no majority result
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
    , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo))
    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
    , vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
    , apprvsips :: !(Set (Data.SIPHash hashAlgo))
    , implementationSt :: !(State (IMPLEMENTATION hashAlgo))
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
            , sipdb = Map.empty
            , ballots = Map.empty
            , vresips = Map.empty
            , apprvsips = Set.empty
            , implementationSt = Implementation.St ()
            , utxoSt = UTxO.St ()
            }
    ]

  transitionRules = [
    do
      TRC ( Env { k
                , maximumBlockSize
                , participants
                , r_a
                , stakeDist
                , prvNoQuorum
                , prvNoMajority
                }
          , St  { currentSlot
                , subsips
                , asips
                , wssips
                , wrsips
                , sipdb
                , ballots
                , vresips
                , apprvsips
                , implementationSt
                , utxoSt
                }
          , block@Block{ header, body }
          ) <- judgmentContext
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)

      -- First a HEAD transition in order to update the state
      Header.St
        { Header.currentSlot = currentSlot'
        , Header.wrsips = wrsips'
        , Header.asips = asips'
        , Header.vresips = vresips'
        , Header.apprvsips = apprvsips'
        } <- trans @(HEADER hashAlgo)
               $ TRC ( Header.Env { Header.k = k
                                  , Header.sipdb = sipdb
                                  , Header.ballots = ballots
                                  , Header.r_a = r_a
                                  , Header.stakeDist = stakeDist
                                  , Header.prvNoQuorum = prvNoQuorum
                                  , Header.prvNoMajority = prvNoMajority
                                  }
                     , Header.St { Header.currentSlot = currentSlot
                                 , Header.wrsips = wrsips
                                 , Header.asips = asips
                                 , Header.vresips = vresips
                                 , Header.apprvsips = apprvsips
                                 }
                     , header
                     )

      -- Second a BODY transition with the updated state from header
      Transaction.St
        { Transaction.subsips = subsips'
        , Transaction.wssips = wssips'
        , Transaction.wrsips = wrsips''
        , Transaction.sipdb = sipdb'
        , Transaction.ballots = ballots'
        , Transaction.implementationSt = implementationSt'
        , Transaction.utxoSt = utxoSt'
        } <- trans @(BODY hashAlgo)
               $ TRC ( Transaction.Env
                          { Transaction.k = k
                          , Transaction.currentSlot = currentSlot'
                          , Transaction.asips = asips'
                          , Transaction.participants = participants
                          , Transaction.apprvsips = apprvsips'
                          , Transaction.utxoEnv = UTxO.Env
                          }
                      , Transaction.St
                          { Transaction.subsips = subsips
                          , Transaction.wssips = wssips
                          , Transaction.wrsips = wrsips'
                          , Transaction.sipdb = sipdb
                          , Transaction.ballots = ballots
                          , Transaction.implementationSt = implementationSt
                          , Transaction.utxoSt = utxoSt
                          }
                      , body
                     )
      pure $! St { currentSlot = currentSlot'
                 , subsips = subsips'
                 , asips = asips'
                 , wssips = wssips'
                 , wrsips = wrsips''
                 , sipdb = sipdb'
                 , ballots = ballots'
                 , vresips = vresips'
                 , apprvsips = apprvsips'
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
    <*> r_aGen
    <*> stakeDistGen
    <*> prvNoQuorumGen
    <*> prvNoMajorityGen
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
            , sipdb
            , ballots
            --, vresips
            , apprvsips
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
                                         , Transaction.apprvsips = apprvsips
                                         , Transaction.participants = participants
                                         , Transaction.utxoEnv = UTxO.Env
                                         }
                         Transaction.St { Transaction.subsips = subsips
                                        , Transaction.wssips = wssips
                                        , Transaction.wrsips = wrsips
                                        , Transaction.sipdb = sipdb
                                        , Transaction.ballots = ballots
                                        , Transaction.implementationSt = implementationSt
                                        , Transaction.utxoSt = utxoSt
                                        }

instance ( HasTypeReps hashAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps (Data.Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         ) => Trace.QC.HasTrace (CHAIN hashAlgo) () where

  envGen _ = do
    someK <- Gen.QC.kGen
    someCurrentSlot <- Gen.QC.currentSlotGen
    -- TODO: for now we generate a constant set of keys. The set of participants
    -- could be an environment of the generator.
    someParticipants <- Gen.QC.participantsGen
    let env = Env { k = someK
                  -- For now we fix the maximum block size to an abstract size of 100
                  , maximumBlockSize = 100
                  , initialSlot = someCurrentSlot
                  , participants = someParticipants
                  }
    pure env

  sigGen
    _traceGenEnv
    Env { k
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
    = do
    someHeader <- Header.headerQCGen currentSlot
    someBody <- transactionsGen (Header.slot someHeader)
    pure $! Block { header = someHeader, body = someBody}
    where
      transactionsGen nextSlot = do
        transactions <-
          Body.transactionsQCGen
            maximumBlockSize
            Transaction.Env
              { Transaction.k = k
              , Transaction.currentSlot = nextSlot
              , Transaction.asips = asips
              , Transaction.participants = participants
              , Transaction.utxoEnv = UTxO.Env
              }
            Transaction.St
              { Transaction.subsips = subsips
              , Transaction.wssips = wssips
              , Transaction.wrsips = wrsips
              , Transaction.ballots = ballots
              , Transaction.voteResultSIPs = voteResultSIPs
              , Transaction.implementationSt = implementationSt
              , Transaction.utxoSt = utxoSt
              }
        pure $! Body.BBody transactions

  shrinkSignal Block { header, body } =
    -- TODO: for now we don't shrink the header.
    fmap (mkBlock . Trace.QC.shrinkSignal @(TRANSACTION hashAlgo) @()) (Body.transactions body)
    where
      mkBlock txs = Block {header = header, body = Body.BBody txs}
