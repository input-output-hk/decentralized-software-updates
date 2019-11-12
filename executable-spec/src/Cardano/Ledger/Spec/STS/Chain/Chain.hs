{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Data.Bimap (Bimap)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set as Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.DSIGN.Class (SignKeyDSIGN, SignedDSIGN,
                     VerKeyDSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount, Slot)

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


data CHAIN hashAlgo dsignAlgo


data Env hashAlgo dsignAlgo
  = Env
    { k :: !BlockCount
    , maximumBlockSize :: !Size
      -- ^ Maximum block size. The interpretation of this value depends on the
      -- instance of 'Sized'.
      --
    , initialSlot :: !Slot
    , participants :: !(Bimap (VerKeyDSIGN dsignAlgo) (SignKeyDSIGN dsignAlgo))
    , r_a :: !Float
      -- ^ Adversary stake ratio
    , stakeDist :: !(Map (VerKeyDSIGN dsignAlgo) Data.Stake)
    , prvNoQuorum :: !Word8
      -- ^ How many times a revoting is allowed due to a no quorum result
    , prvNoMajority :: !Word8
      -- How many times a revoting is allowed due to a no majority result
    }

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo), Eq (SignKeyDSIGN dsignAlgo)) => Eq (Env hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo), Show (SignKeyDSIGN dsignAlgo)) => Show (Env hashAlgo dsignAlgo)

data St hashAlgo dsignAlgo
  = St
    { currentSlot :: !Slot
    , subsips :: !(Map (Data.Commit hashAlgo dsignAlgo) (Data.SIP hashAlgo dsignAlgo))
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , wssips :: !(Map (Data.Commit hashAlgo dsignAlgo) Slot)
    , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo dsignAlgo))
    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map (VerKeyDSIGN dsignAlgo) Data.Confidence))
    , vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
    , apprvsips :: !(Set (Data.SIPHash hashAlgo))
    , implementationSt :: !(State (IMPLEMENTATION hashAlgo))
    , utxoSt :: !(State UTXO)
    }

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (St hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (St hashAlgo dsignAlgo)

data Block hashAlgo dsignAlgo
  = Block
    { header :: Signal (HEADER hashAlgo dsignAlgo)
    , body :: Signal (BODY hashAlgo dsignAlgo)
    }
    deriving (Eq, Show, Generic)

deriving instance ( Typeable dsignAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
                  , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
                  , HasTypeReps (VerKeyDSIGN dsignAlgo)
                  , Show (SignKeyDSIGN dsignAlgo)
                  ) => HasTypeReps (Block hashAlgo dsignAlgo)


instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
         , HasTypeReps (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         ) => Sized (Block hashAlgo dsignAlgo) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION hashAlgo dsignAlgo))


instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo) -- TODO: Remove Ord constraint on key hashes.
         , Ord (SignKeyDSIGN dsignAlgo) -- TODO: Remove Ord constraint on key hashes.
         , HasTypeReps (VerKeyDSIGN dsignAlgo)
         ) => STS (CHAIN hashAlgo dsignAlgo) where

  type Environment (CHAIN hashAlgo dsignAlgo) = Env hashAlgo dsignAlgo

  type State (CHAIN hashAlgo dsignAlgo) = St hashAlgo dsignAlgo

  type Signal (CHAIN hashAlgo dsignAlgo) = Block hashAlgo dsignAlgo

  data PredicateFailure (CHAIN hashAlgo dsignAlgo)
    = MaximumBlockSizeExceeded Size (Threshold Size)
    | ChainFailureBody (PredicateFailure (BODY hashAlgo dsignAlgo))
    | ChainFailureHeader (PredicateFailure (HEADER hashAlgo dsignAlgo))

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
        } <- trans @(HEADER hashAlgo dsignAlgo)
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
        } <- trans @(BODY hashAlgo dsignAlgo)
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


deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (PredicateFailure (CHAIN hashAlgo dsignAlgo))

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (PredicateFailure (CHAIN hashAlgo dsignAlgo))


instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo) -- TODO: Remove Ord constraint on key hashes.
         , Ord (SignKeyDSIGN dsignAlgo) -- TODO: Remove Ord constraint on key hashes.
         , HasTypeReps (VerKeyDSIGN dsignAlgo)
         ) => Embed (BODY hashAlgo dsignAlgo) (CHAIN hashAlgo dsignAlgo) where
  wrapFailed = ChainFailureBody

instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo) -- TODO: Remove Ord constraint on key hashes.
         , Ord (SignKeyDSIGN dsignAlgo) -- TODO: Remove Ord constraint on key hashes.
         , HasTypeReps (VerKeyDSIGN dsignAlgo)
         ) => Embed (HEADER hashAlgo dsignAlgo) (CHAIN hashAlgo dsignAlgo) where
  wrapFailed = ChainFailureHeader

--------------------------------------------------------------------------------
-- HasTrace instance
--------------------------------------------------------------------------------

instance ( HasTypeReps hashAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps (Data.Commit hashAlgo MockDSIGN)
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (SignedDSIGN MockDSIGN (Data.Commit hashAlgo MockDSIGN))
         , HasTypeReps (VerKeyDSIGN MockDSIGN)
         ) => STS.Gen.HasTrace (CHAIN hashAlgo MockDSIGN) () where

  envGen _ = do
    someK <- Gen.QC.k
    someCurrentSlot <- Gen.QC.currentSlot
    -- For now we generate a constant set of keys. The set of participants could
    -- be an environment of the generator.
    someParticipants <- Gen.QC.participants
    someRa <- Gen.QC.rA
    someStakeDist <- Gen.QC.stakeDist
    somePrvNoQuorum <- Gen.QC.prvNoQuorum
    somePrvNoMajority <- Gen.QC.prvNoMajority
    let env = Env { k = someK
                  -- For now we fix the maximum block size to an abstract size of 100
                  , maximumBlockSize = 100
                  , initialSlot = someCurrentSlot
                  , participants = someParticipants
                  , r_a = someRa
                  , stakeDist = someStakeDist
                  , prvNoQuorum = somePrvNoQuorum
                  , prvNoMajority = somePrvNoMajority
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
       , sipdb
       , ballots
       , apprvsips
       , implementationSt
       , utxoSt
       }
    = do
    someHeader <- Header.headerGen currentSlot
    let
      transactionEnv =
        Transaction.Env
          { Transaction.k = k
          , Transaction.currentSlot = Header.slot someHeader
          , Transaction.asips = asips
          , Transaction.participants = participants
          , Transaction.utxoEnv = UTxO.Env
          , Transaction.apprvsips = apprvsips
          }
      transactionSt =
        Transaction.St
          { Transaction.subsips = subsips
          , Transaction.wssips = wssips
          , Transaction.wrsips = wrsips
          , Transaction.ballots = ballots
          , Transaction.sipdb = sipdb
          , Transaction.implementationSt = implementationSt
          , Transaction.utxoSt = utxoSt
          }
    someBody <- Body.gen maximumBlockSize transactionEnv transactionSt
    pure $! Block { header = someHeader, body = someBody}

  shrinkSignal Block { header, body } =
    -- For now we don't shrink the header. Define this as needed.
    mkBlock <$> Body.shrink body
    where
      mkBlock shrunkBody = Block { header = header, body = shrunkBody }
