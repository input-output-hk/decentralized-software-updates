{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | SM-SOS conformance tests
module Cardano.Ledger.Spec.STS.SM.Update.Ideation.Properties.Conformance where

import           Control.Arrow (first)
import           Control.Monad.Except (Except)
import           Data.List.NonEmpty (fromList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Semigroup (sconcat)
import           Data.Set (Set)
import           Data.Typeable (cast)
import           Test.QuickCheck hiding (Success)

import qualified Ledger.Core as Core

import           Control.State.DataAutomata
import           Control.State.DataAutomata.Interpreter.Gen
import           Control.State.DataAutomata.Interpreter.Memory
import           Control.State.DataAutomata.Interpreter.Run
import qualified Control.State.DataAutomata.Interpreter.Run as Run
import           Control.State.DataAutomata.Interpreter.Trace
import           Control.State.DataAutomata.Test.Conformance

import           Cardano.Ledger.Spec.SM.Ideation
import           Cardano.Ledger.Spec.SM.Ideation.Full
import           Cardano.Ledger.Spec.SM.Vote hiding (SIPBallot)

import           Cardano.Ledger.Spec.Classes.Hashable (hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (SKey, VKey, sign)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as STS.StakeDistribution

import qualified Cardano.Ledger.Spec.STS.Update as Update
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (calcCommit)
import           Cardano.Ledger.Spec.STS.Update.Definitions (vThreshold)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Ideation.Data

import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))
import           Cardano.Ledger.Test.Mock (Mock)

import           Ledger.Core (BlockCount (BlockCount))

import           Cardano.Ledger.Update.Interface

data InitParams =
  InitParams
  { r_a :: !Float
  , k :: !Word
  , sipMapping :: !SIPMapping
  , participants :: ![Participant]
  , initStakeDist :: !StakeDistribution
  , currentSlot :: !Word
  }

mkIState :: InitParams -> IState Mock
mkIState InitParams {k, initStakeDist, r_a, currentSlot} =
  IState
    { iStateK = BlockCount $ fromIntegral k
    , iStateMaxVotingPeriods = 2
    , iStateStakeDist = elaborateStakeDist initStakeDist
    , iStateCurrentSlot = Core.Slot $ fromIntegral currentSlot
    , iStateSubsips = mempty
    , iStateSipdb = mempty
    , iStateBallot = mempty
    , iStateR_a = r_a
    , iStateWssips = mempty
    , iStateWrsips = mempty
    , iStateAsips = mempty
    , iStatevresips = mempty
    , iStateApprvsips = mempty
    , iStateApproval = mempty
    }

elaborateStakeDist :: StakeDistribution -> STS.StakeDistribution.StakeDistribution Mock
elaborateStakeDist (StakeDistribution stakeMap)
  =  STS.StakeDistribution.StakeDistribution
  $ Map.fromList
  $ fmap (first participantToKeyHash)
  $ Map.toList stakeMap
  where
    participantToKeyHash (Participant pid) = hash $ participantToVKey pid

participantToVKey :: Word -> VKey Mock
participantToVKey = VerKeyMockDSIGN . fromIntegral

participantToSKey :: Word -> SKey Mock
participantToSKey = SignKeyMockDSIGN . fromIntegral

type SIPMapping = Map Word (Int, Ideation.Data.SIPData, Participant)

-- | SM-SOS conformance test for the update interface.
--
--
prop_updateConforms :: Property
prop_updateConforms  =
  withMaxSuccess 5000 $
  conforms mkIState traceGen traceFilter runUpdateSOS updateModel initParams


initParams :: InitParams
initParams =
  InitParams
  { r_a = 0.3
  , k = 3
  , sipMapping = [(0, (someSalt, dummySIPData, someAuthor))] -- TODO: use some dummy SIP for now.
  , participants = someParticipants
  , initStakeDist = StakeDistribution $ Map.fromList $ zip someParticipants (repeat 100)
  -- TODO: revert this back to 0 and fix problem in module Cardano.Ledger.Spec.STS.Update.Ideation line 159.
  -- Core.Slot 0 -- We could make this configurable via the initial parameters.
  , currentSlot = 20
  }
  where
    someParticipants = Participant <$> [0 .. 10]

    someSalt = 93

    someAuthor = Participant 0

    dummySIPData :: Ideation.Data.SIPData
    dummySIPData =
      Ideation.Data.SIPData
      { Ideation.Data.url = Data.URL "foo"
      , Ideation.Data.metadata =
        Ideation.Data.SIPMetadata
        { Ideation.Data.versionFrom = (Ideation.Data.ProtVer 0, Ideation.Data.ApVer 0)
        , Ideation.Data.versionTo = (Ideation.Data.ProtVer 0, Ideation.Data.ApVer 0)
        , Ideation.Data.impactsConsensus = Ideation.Data.NoImpact
        , Ideation.Data.impactsParameters = []
        , Ideation.Data.votPeriodDuration = Core.SlotCount 20
        }
      }

traceGen :: InitParams -> GeneratorModel -> Gen InvalidTrace
traceGen _ m =
  automataInvalidTrace (actionGenerators m)
                       (Between (Lower 0) (Upper 200)) -- TODO: make it configurable
                       (initialMemory $ runnableModel m)
                       (automata $ runnableModel m)
                       ( [ "tick"
                         , "submit_0"
                         , "reveal_0"
                         , "vote_0"
                         ]
                       )

traceFilter :: InvalidTrace -> InvalidTrace
traceFilter = filter keep
  where
    keep action =
      -- We filter the internal actions.
      actionName action `notElem` ([ "wait"
                                   , "setInc"
                                   , "noverdict_0"
                                   , "majorityRejects"
                                   , "createVoteFor_0"
                                   , "majorityAgrees"
                                   , "createVoteAgainst_0"
                                   , "tally_0"
                                   , "reject_0"
                                   , "abstain_0"
                                   , "approve_0"
                                   , "vpend_0"
                                   , "active_0"
                                   ] :: Set ActionName
                                  )

runUpdateSOS :: IState Mock -> CAction -> Either (UIError Mock) (IState Mock)
runUpdateSOS iState (CAction "tick" ms)
  = slotTick slot iState
  where
    slot =
      case cast ms of
        Nothing          -> error "Wrong type for the slot tick"
        Just (s :: Word) -> Core.Slot (fromIntegral s)
-- TODO: when composing multiple automata we need to parse the action names.
runUpdateSOS iState (CAction "submit_0" _) =
  applyUpdatePayload updatePayload iState
    where
      updatePayload
        = Update.Ideation
        $ Ideation.Data.Submit sipCommit sip
        where
          sip = lookupSIP (sipMapping initParams) 0
          (_, _, Participant p)
            = fromMaybe (error "No SIP found") $ Map.lookup 0 $ sipMapping initParams
          sipCommit =
            Ideation.Data.SIPCommit
            { Ideation.Data.commit = commit
            , Ideation.Data.sipCommitAuthor = Ideation.Data.sipAuthor sip
            , Ideation.Data.upSig = sign commit (participantToSKey p)
            }
          commit = calcCommit sip
runUpdateSOS iState (CAction "reveal_0" _) =
  applyUpdatePayload updatePayload iState
  where
    updatePayload
      = Update.Ideation
      $ Ideation.Data.Reveal
      $ lookupSIP (sipMapping initParams) 0
runUpdateSOS iState (CAction "vote_0" vote) =
  -- Issue a vote for our only SIP. We need to take this from our SIP data.
  applyUpdatePayload updatePayload iState
  where
    updatePayload
      = Update.Ideation
      $ Ideation.Data.Vote
      $ Ideation.Data.VoteForSIP
        { Ideation.Data.votedsipHash = sipHash
        , Ideation.Data.confidence = confidence
        , Ideation.Data.voter = aVk
        , Ideation.Data.voterSig = sign (sipHash, confidence, aVk) (participantToSKey i)
        }
      where
        (_, sipData, _) = fromMaybe (error "No SIP found") $ Map.lookup 0 $ sipMapping initParams
        Vote (Participant i) aDecision =
          case cast vote of
            Nothing -> error "Wrong type for vote"
            Just v  -> v
        sipHash = Ideation.Data.SIPHash $ hash sipData -- TODO: we should cache this for efficiency.
        confidence = decisionToConfidence aDecision
        aVk = participantToVKey i
runUpdateSOS _iState act  = error $ "No dispatcher for action " ++ show act

updateModel :: GeneratorModel
updateModel =
  GeneratorModel
  { actionGenerators = tickerActsGen <> actsGenFromInitParams initParams
  , runnableModel =
      RunnableModel
      { initialMemory = Leaf (initTickerMem aCurrentSlot) :++ memoryFromInitParams initParams
      , automata = Sync ["tick"] (Single ticker) :|| automataFromInitParams initParams
      }
  }
  where
    aCurrentSlot = currentSlot initParams

decisionToConfidence :: Decision -> Data.Confidence
decisionToConfidence For = Data.For
decisionToConfidence Against = Data.Against
decisionToConfidence Abstain = Data.Abstain

lookupSIP :: SIPMapping -> Word -> Ideation.Data.SIP Mock
lookupSIP aSipMapping i =
  Ideation.Data.SIP
  { Ideation.Data.sipHash = Ideation.Data.SIPHash $ hash sipData
  , Ideation.Data.sipAuthor = participantToVKey p
  , Ideation.Data.sipSalt = salt
  , Ideation.Data.sipPayload = sipData
  }
  where
    (salt, sipData, Participant p)
      = fromMaybe (error "No SIP found") $ Map.lookup i $ aSipMapping


automataFromInitParams :: InitParams -> Automata
automataFromInitParams InitParams { sipMapping } =
  sconcat $ fmap fullModel $ fromList $ Map.keys sipMapping

memoryFromInitParams :: InitParams -> LTree Memory
memoryFromInitParams InitParams { r_a, initStakeDist, k, sipMapping, currentSlot}
  = sconcat
  $ fromList
  $ fmap (fullModelInitMem tauV initStakeDist k currentSlot)
  $ fmap (extractVPD . snd')
  $ Map.elems sipMapping
  where
    tauV = vThreshold r_a

    snd' (_, y, _) = y

    extractVPD :: Ideation.Data.SIPData -> Word
    extractVPD =
      -- We should make sure that Word and Word64 have the same representation
      -- in the architecture these tests are running.
      fromIntegral . Core.unSlotCount .  Ideation.Data.votPeriodDuration . Ideation.Data.metadata

actsGenFromInitParams :: InitParams -> Map ActionName (Gen Cell)
actsGenFromInitParams InitParams { participants, sipMapping } =
  sconcat $ fromList $ fmap (fullModelGens participants) $ Map.keys sipMapping

--------------------------------------------------------------------------------
-- Unit / manual tests. To help debigging in the case of failure.
--------------------------------------------------------------------------------

runTraceSample :: InvalidTrace -> Except (LTree State, Run.Error) (LTree Memory, LTree State)
runTraceSample aSample =
  runAutomata
    (initialMemory $ runnableModel updateModel)
    (automata $ runnableModel updateModel)
    (fmap (either id id) aSample)

-- | Why the reveal is invalid?
exampleInvalidReveal :: InvalidTrace
exampleInvalidReveal =
  [ Right (CAction (ActionName "majorityRejects") ())
  , Right
    (CAction
       (ActionName "createVoteAgainst_0")
       (Vote {vk = Participant 1, decision = Against}))
  , Right (CAction (ActionName "submit_0") ())
  , Right (CAction (ActionName "setInc") (3 :: Word))
  , Right (CAction (ActionName "tick") (23 :: Word))
  , Right (CAction (ActionName "wait") ())
  , Right (CAction (ActionName "tick") (26 :: Word))
  , Left (CAction (ActionName "reveal_0") ())
  ]

-- | Why is the vote invalid?
exampleInvalidVote :: InvalidTrace
exampleInvalidVote =
  [ Right (CAction (ActionName "setInc") (3 :: Word))
  , Right (CAction (ActionName "submit_0") ())
  , Right (CAction (ActionName "tick") (23 :: Word))
  , Right (CAction (ActionName "wait") ())
  , Right (CAction (ActionName "tick") (26 :: Word))
  , Right (CAction (ActionName "setInc") (3 :: Word))
  , Right (CAction (ActionName "reveal_0") ())
  , Right (CAction (ActionName "tick") (29 :: Word))
  , Right (CAction (ActionName "wait") ())
  , Right (CAction (ActionName "majorityRejects") ())
  , Right (CAction (ActionName "tick") (32 :: Word))
  , Right (CAction (ActionName "setInc") (3 :: Word))
  , Left
    (CAction (ActionName "vote_0") (Vote {vk = Participant 0, decision = For}))
  ]

-- | Why is the reveal valid? The submission is not stable!
exampleValidReveal :: InvalidTrace
exampleValidReveal =
  [ Right (CAction (ActionName "setInc") (2 :: Word))
  , Right (CAction (ActionName "tick") (2 :: Word))
  , Right (CAction (ActionName "submit_0") ())
  , Right (CAction (ActionName "wait") ())
  , Right (CAction (ActionName "tick") (4 :: Word))
  , Right (CAction (ActionName "majorityAgrees") ())
  , Right
    (CAction
       (ActionName "createVoteFor_0")
       (Vote {vk = Participant 8, decision = For}))
  , Right (CAction (ActionName "setInc") (3 :: Word))
  , Right (CAction (ActionName "tick") (7 :: Word))
  , Right (CAction (ActionName "wait") ())
  , Right (CAction (ActionName "reveal_0") ())
  ]

-- | Why ticks can't happen?
exampleTick :: InvalidTrace
exampleTick =
  [ Right (CAction (ActionName "setInc") (1 :: Word))
  , Right (CAction (ActionName "submit_0") ())
  , Right (CAction (ActionName "majorityRejects") ())
  , Right
    (CAction
       (ActionName "createVoteAgainst_0")
       (Vote {vk = Participant 7, decision = Against}))
  , Left (CAction (ActionName "tick") (21 :: Word))
  ]
