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

import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount, Slot)
import           Cardano.Binary (ToCBOR)

import qualified Cardano.Ledger.Generators.QuickCheck as Gen
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme)
import           Cardano.Ledger.Spec.Classes.Sizeable (HasSize, Size, Sizeable,
                     size)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.Ballot (Ballot)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.SIPsVoteResults (SIPsVoteResults)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.State.ActiveSUs (ActiveSUs)
import           Cardano.Ledger.Spec.State.BallotSUs (BallotSUs)
import           Cardano.Ledger.Spec.State.WhenRevealedSUs (WhenRevealedSUs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSUs (WhenSubmittedSUs)
import           Cardano.Ledger.Spec.State.RevealedSUs (RevealedSUs)
import           Cardano.Ledger.Spec.State.SubmittedSUs (SubmittedSUs)
import           Cardano.Ledger.Spec.STS.Chain.Body (BODY)
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import           Cardano.Ledger.Spec.STS.Chain.Header (HEADER)
import qualified Cardano.Ledger.Spec.STS.Chain.Header as Header
import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import           Cardano.Ledger.Spec.STS.Update.Data (UP)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import qualified Cardano.Ledger.Spec.Classes.IsSUCommit as IsSUCommit
import           Cardano.Ledger.Spec.STS.Update.GenApproval (GENAPPROVAL)
import           Cardano.Ledger.Spec.STS.Update (UPDATE)
import qualified Cardano.Ledger.Spec.STS.Update.GenApproval as GenApproval

import           Cardano.Ledger.Test.Mock (Mock)

data CHAIN p


data Env p
  = Env
    { k :: !BlockCount
    , maximumBlockSize :: !(Size p)
      -- ^ Maximum block size. The interpretation of this value depends on the
      -- instance of 'Sized'.
      --
    , initialSlot :: !Slot
    , participants :: !(Participants p)
    , r_a :: !Float
      -- ^ Adversary stake ratio
    , stakeDist :: !(StakeDistribution p)
    , prvNoQuorum :: !Word8
      -- ^ How many times a revoting is allowed due to a no quorum result
    , prvNoMajority :: !Word8
      -- ^ How many times a revoting is allowed due to a no majority result
    }

deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show (Size p)
                  ) => Show (Env p)

data St p
  = St
    { currentSlot :: !Slot
    , subsips :: !(SubmittedSIPs p)
    , subUPs :: !(SubmittedSUs (UP p) p)
    , asips :: !(ActiveSIPs p)
    , aUPs :: !(ActiveSUs (UP p) p)
    , wssips :: !(WhenSubmittedSIPs p)
    , wsUPs :: !(WhenSubmittedSUs (UP p) p)
    , wrsips :: !(WhenRevealedSIPs p)
    , wrUPs :: !(WhenRevealedSUs (UP p) p)
    , sipdb :: !(RevealedSIPs p)
    , updb :: !(RevealedSUs (UP p) p)
    , ballots :: !(Ballot p)
    , ballotUPs :: !(BallotSUs (UP p) p)
    , vresips :: !(SIPsVoteResults p)
    , apprvsips :: !(ApprovedSIPs p)
    , implementationSt :: !(State (IMPLEMENTATION p))
    , utxoSt :: !(State UTXO)
    }

deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show (IsSUCommit.CommitSU (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (St p)

data Block p
  = Block
    { header :: Signal (HEADER p)
    , body :: Signal (BODY p)
    }
    deriving (Generic)

deriving instance ( Hashable p
                  , Hashable (UP p)
                  , HasSigningScheme p
                  , HasSigningScheme (UP p)
                  , Show p
                  , Show (IsSUCommit.SUCommit (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (Block p)

deriving instance ( Typeable p
                  , HasTypeReps (Signal (HEADER p))
                  , HasTypeReps (Signal (BODY p))
                  ) => HasTypeReps (Block p)


instance ( Typeable p
         , Sized (Signal (TRANSACTION p))
         ) => Sized (Block p) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION p))


instance ( Hashable p
         , Sizeable p
         , HasSize p (Block p)
         , Show (Size p)
         , STS (HEADER p)
         , STS (BODY p)
         , Ord (IsSUCommit.CommitSU (UP p) p)
         , Ord (Data.UPHash p)
         ) => STS (CHAIN p) where

  type Environment (CHAIN p) = Env p

  type State (CHAIN p) = St p

  type Signal (CHAIN p) = Block p

  data PredicateFailure (CHAIN p)
    = MaximumBlockSizeExceeded (Size p) (Threshold (Size p))
    | ChainFailureBody (PredicateFailure (BODY p))
    | ChainFailureHeader (PredicateFailure (HEADER p))

  initialRules = [ do
    IRC Env { initialSlot } <- judgmentContext
    pure St { currentSlot = initialSlot
            , subsips = mempty
            , subUPs = mempty
            , asips = mempty
            , aUPs = mempty
            , wssips = mempty
            , wsUPs = mempty
            , wrsips = mempty
            , wrUPs = mempty
            , sipdb = mempty
            , updb = mempty
            , ballots = mempty
            , ballotUPs = mempty
            , vresips = mempty
            , apprvsips = mempty
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
                , subUPs
                , asips
                , aUPs
                , wssips
                , wsUPs
                , wrsips
                , wrUPs
                , sipdb
                , updb
                , ballots
                , ballotUPs
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
      -- TODO Implement update of the following state by HEADER STS:
            -- Header.wrUPs = wrUPs'
            -- Header.aUPs = aUPs'
            -- Header.vresUPs = vresUPs'
            -- Header.apprvUPs = apprvUPs'
      Header.St
        { Header.currentSlot = currentSlot'
        , Header.wrsips = wrsips'
        , Header.asips = asips'
        , Header.vresips = vresips'
        , Header.apprvsips = apprvsips'
        } <- trans @(HEADER p)
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
        , Transaction.subUPs = subUPs'
        , Transaction.wssips = wssips'
        , Transaction.wsUPs = wsUPs'
        , Transaction.wrsips = wrsips''
        , Transaction.wrUPs = wrUPs''
        , Transaction.sipdb = sipdb'
        , Transaction.updb = updb'
        , Transaction.ballots = ballots'
        , Transaction.ballotUPs = ballotUPs'
        , Transaction.implementationSt = implementationSt'
        , Transaction.utxoSt = utxoSt'
        } <- trans @(BODY p)
               $ TRC ( Transaction.Env
                          { Transaction.k = k
                          , Transaction.currentSlot = currentSlot'
                          , Transaction.asips = asips'
                          , Transaction.aUPs = aUPs
                            -- TODO: , Transaction.aUPs = aUPs'
                          , Transaction.participants = participants
                          , Transaction.stakeDist = stakeDist
                          , Transaction.apprvsips = apprvsips'
                          , Transaction.utxoEnv = UTxO.Env
                          }
                      , Transaction.St
                          { Transaction.subsips = subsips
                          , Transaction.subUPs = subUPs
                          , Transaction.wssips = wssips
                          , Transaction.wsUPs = wsUPs
                          , Transaction.wrsips = wrsips'
                          , Transaction.wrUPs = wrUPs
                          -- TODO: , Transaction.wrUPs = wrUPs'
                          , Transaction.sipdb = sipdb
                          , Transaction.updb = updb
                          , Transaction.ballots = ballots
                          , Transaction.ballotUPs = ballotUPs
                          , Transaction.implementationSt = implementationSt
                          , Transaction.utxoSt = utxoSt
                          }
                      , body
                     )
      pure $! St { currentSlot = currentSlot'
                 , subsips = subsips'
                 , subUPs = subUPs'
                 , asips = asips'
                 , aUPs = aUPs
                 -- TODO: , aUPs = aUPs'
                 , wssips = wssips'
                 , wsUPs = wsUPs'
                 , wrsips = wrsips''
                 , wrUPs = wrUPs''
                 , sipdb = sipdb'
                 , updb = updb'
                 , ballots = ballots'
                 , ballotUPs = ballotUPs'
                 , vresips = vresips'
                 , apprvsips = apprvsips'
                 , implementationSt = implementationSt'
                 , utxoSt = utxoSt'
                 }
    ]


deriving instance ( Show (Size p)
                  , Show (PredicateFailure (HEADER p))
                  , Show (PredicateFailure (BODY p))
                  ) => Show (PredicateFailure (CHAIN p))
deriving instance ( Eq (Size p)
                  , Eq (PredicateFailure (HEADER p))
                  , Eq (PredicateFailure (BODY p))
                  ) => Eq (PredicateFailure (CHAIN p))


instance ( STS (BODY p), STS (CHAIN p)
         ) => Embed (BODY p) (CHAIN p) where
  wrapFailed = ChainFailureBody

instance ( STS (HEADER p), STS (CHAIN p) ) => Embed (HEADER p) (CHAIN p) where
  wrapFailed = ChainFailureHeader

--------------------------------------------------------------------------------
-- HasTrace instance
--------------------------------------------------------------------------------

instance ( -- Sizeable p
         -- , STS (CHAIN p)
         -- -- TODO: the constraints below could be simplified by defining an HasTrace instance for BODY.
         -- , STS.Gen.HasTrace (TRANSACTION p) ()
         -- , HasSize p (Transaction.Tx p)
         --  ToCBOR SlotCount
           HasTypeReps (IsSUCommit.SUCommit (UP Mock) Mock)
         , HasTypeReps (IsSU.SU (UP Mock) Mock)
         , Show (GENAPPROVAL (UP Mock) Mock)
         , Embed (GENAPPROVAL (UP Mock) Mock) (UPDATE Mock)
         , Ord (IsSUCommit.CommitSU (UP Mock) Mock)
         , Eq (GENAPPROVAL (UP Mock) Mock)
         , Ord (Data.UPHash Mock)
         , Ord (IsSU.SU (UP Mock) Mock)
         , GenApproval.SUCommitGen (UP Mock) Mock
         , IsSUCommit.SUCommitHasHash
                          (UP Mock) Mock (UP Mock)
         , ToCBOR (IsSUCommit.CommitSU (UP Mock) Mock)
         , Show (IsSUCommit.CommitSU (UP Mock) Mock)
         , Show (IsSU.SU (UP Mock) Mock)
         , Show (Data.UPHash Mock)
         ) => STS.Gen.HasTrace (CHAIN Mock) () where

  envGen _ = do
    someK <- Gen.k
    someCurrentSlot <- Gen.currentSlot
    -- For now we generate a constant set of keys. The set of participants could
    -- be an environment of the generator.
    someParticipants <- Gen.participants
    someRa <- Gen.rA
    someStakeDist <- Gen.stakeDist someParticipants
    somePrvNoQuorum <- Gen.prvNoQuorum
    somePrvNoMajority <- Gen.prvNoMajority
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
        , stakeDist
        }
    St { currentSlot
       , subsips
       , subUPs
       , asips
       , aUPs
       , wssips
       , wsUPs
       , wrsips
       , wrUPs
       , sipdb
       , updb
       , ballots
       , ballotUPs
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
          , Transaction.aUPs = aUPs
          , Transaction.participants = participants
          , Transaction.stakeDist = stakeDist
          , Transaction.utxoEnv = UTxO.Env
          , Transaction.apprvsips = apprvsips
          }
      transactionSt =
        Transaction.St
          { Transaction.subsips = subsips
          , Transaction.subUPs = subUPs
          , Transaction.wssips = wssips
          , Transaction.wsUPs = wsUPs
          , Transaction.wrsips = wrsips
          , Transaction.wrUPs = wrUPs
          , Transaction.ballots = ballots
          , Transaction.ballotUPs = ballotUPs
          , Transaction.sipdb = sipdb
          , Transaction.updb = updb
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
