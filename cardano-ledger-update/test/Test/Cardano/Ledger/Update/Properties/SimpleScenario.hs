{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Definition of the update test generator
module Test.Cardano.Ledger.Update.Properties.SimpleScenario
  ( Simple
  , Scenario ( UpdateScenario
             , tsUpdateSpecs
             )
  )
where

import           Control.Applicative.Successors (Succs (Succs), getSuccs)
import           Control.Arrow (first)
import           Data.List (find)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word64)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements,
                     frequency, oneof, shrink, shrinkList, shrinkNothing,
                     vectorOf)

import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Block (BlockNo (BlockNo), unBlockNo)
import           Cardano.Slotting.Slot (SlotNo (SlotNo), unSlotNo)

import           Cardano.Ledger.Assert (allUnique, assertAndReturn)

import           Cardano.Ledger.Update.Env.StakeDistribution (Stake (Stake),
                     StakeDistribution)
import           Cardano.Ledger.Update.Proposal
import           Cardano.Ledger.Update.ProposalsState
                     (VotingPeriod (VotingPeriod), unVotingPeriod)

import qualified Cardano.Ledger.Update as Update
import qualified Cardano.Ledger.Update.Env.StakeDistribution as StakeDistribution
import qualified Cardano.Ledger.Update.Proposal as Proposal

import           Trace.Scenario

import           Test.Cardano.Ledger.Update.Data
import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT
import           Test.Cardano.Ledger.UpdateSpec


-- | Type tag to refer to simple update system scenarios. Simple in this context
-- means that the scenario uses as little knowledge of the system logic as
-- possible.
data Simple

instance HasScenario UpdateSUT Simple where
  data Scenario Simple =
    UpdateScenario
    { tsK                     :: !BlockNo
    , tsMaxVotingPeriods      :: !VotingPeriod
    , tsCurrentSlot           :: !SlotNo
    , tsSlotsPerEpoch         :: !SlotNo
    , tsAdversarialStakeRatio :: !Float
    , tsParticipants          :: !(Map Participant Stake)
    -- ^ The participants have an initial stake associated to them, which we use
    -- to elaborate an initial state distribution. We have a single stake
    -- distribution, which we will use for all experts (SIP and implementation)
    -- as well as for stakepools.
    , tsGenesisProtocol       :: !(Protocol MockImpl)
    , tsUpdateSpecs           :: ![UpdateSpec]
    , tsActions               :: [UpdateAction]
    -- ^ Pre-generated actions to be used in the elaborator.
    } deriving (Show)

  data ElaboratorSt Simple =
    UpdateElaboratorSt
    { remainingActions :: [UpdateAction]
      -- ^ Remaining (pre-generated) actions.
    } deriving (Show)

  canTerminate = null . remainingActions

  mkInitStates ts =
    ( mkInitialGeneratorState ts
    , UpdateSt (elaborateSUTInitialState ts)
    )

  nextAct testSetup st =
    case remainingActions st of
      a:as -> (elaborateAction testSetup a, st { remainingActions = as })
      []   -> error $  "The list of next actions cannot be empty when calling 'nextAct'. "
                    ++ "Was 'canTerminate' called prior to calling this function? "
                    ++ "Did 'canTerminate' returns 'True' when 'remainingActions' is empty?"

elaborateAction :: Scenario Simple -> UpdateAction -> SUTAct UpdateSUT
elaborateAction _ JustTick = TickAct
elaborateAction testSetup (SIPCommit specId)
  = UpdateAct . Update.Ideation . Proposal.Submit . getSIPSubmission
  $ lookupSpec testSetup specId
elaborateAction testSetup (SIPReveal specId)
  = UpdateAct . Update.Ideation . Proposal.Reveal . getSIPRevelation
  $ lookupSpec testSetup specId
elaborateAction _testSetup (SIPVote vote)
  = UpdateAct . Update.Ideation . Proposal.Cast
  $ vote
elaborateAction testSetup (ImplCommit specId)
  = UpdateAct . Update.Approval . Proposal.Submit . getImplSubmission
  $ lookupSpec testSetup specId
elaborateAction testSetup (ImplReveal specId)
  = UpdateAct . Update.Approval . Proposal.Reveal . getImplRevelation
  $ lookupSpec testSetup specId
elaborateAction _testSetup (ImplVote vote)
  = UpdateAct . Update.Approval . Proposal.Cast
  $ vote
elaborateAction _testSetup (ImplEndorsement endorsement)
  = UpdateAct . Update.Activation
  $ endorsement

lookupSpec :: Scenario Simple -> SpecId -> UpdateSpec
lookupSpec testSetup specId =
  case find ((== specId) . getUpdateSpecId) (tsUpdateSpecs testSetup) of
    Nothing -> error $ "Could not find specification with id " ++ show specId
    Just spec -> spec

-- | Pre-generated update actions.
--
-- Update specifications are shrunk, except for their references, hence we
-- cannot have a the data of the update spec in the action, since they would no
-- longer correspond to the update specification.
--
-- Note that votes and endorsements are also references to update specifications
-- so we can keep them in the payload.
data UpdateAction
  = JustTick
  | SIPCommit SpecId
  | SIPReveal SpecId
  | SIPVote (Vote MockSIP)
  | ImplCommit SpecId
  | ImplReveal SpecId
  | ImplVote (Vote MockImpl)
  | ImplEndorsement (Update.Endorsement MockSIP MockImpl)
  deriving (Show)

genSIPVote
  :: [Participant]
  -> Maybe VotersBehavior
  -> UpdateSpec
  -> Gen (Vote MockSIP)
genSIPVote participants mVotersBehavior aSpec = do
  participant  <- fmap MockVoter $ elements participants
  vConfidence  <- frequency $ confidenceFrequenciesFor
                            $ fromMaybe Uniform mVotersBehavior
  voteVerifies <- frequency [(8, pure True), (2, pure False)]
  pure $! MockVote (_id participant) (_id sip) vConfidence voteVerifies
  where
    sip = getSIP aSpec

genImplVote
  :: [Participant]
  -> Maybe VotersBehavior
  -> UpdateSpec
  -> Gen (Vote MockImpl)
genImplVote participants mVotersBehavior aSpec = do
  participant  <- fmap MockVoter $ elements participants
  vConfidence  <- frequency $ confidenceFrequenciesFor
                            $ fromMaybe Uniform mVotersBehavior
  voteVerifies <- frequency [(8, pure True), (2, pure False)]
  pure $! MockVote (_id participant) (_id impl) vConfidence voteVerifies
  where
    impl = getImpl aSpec

genEndorsement
  :: [Participant]
  -> UpdateSpec
  -> Gen (Update.Endorsement MockSIP MockImpl)
genEndorsement participants aSpec = do
  endorser <- fmap MockEndorser $ elements participants
  pure $! Update.Endorsement
          { Update.endorserId      = _id endorser
          , Update.endorsedVersion = protocolVersion aSpec
          }

mkInitialGeneratorState
  :: Scenario Simple -> ElaboratorSt Simple
mkInitialGeneratorState ts =
  UpdateElaboratorSt
  { remainingActions = tsActions ts
  }

elaborateSUTInitialState
  :: Scenario Simple -> IState
elaborateSUTInitialState ts =
  IState
  { iStateK                      = tsK ts
  , iStateMaxVotingPeriods       = tsMaxVotingPeriods ts
  , iStateSIPStakeDist           = elaborateStakeDist MockVoter ts
  , iStateImplStakeDist          = elaborateStakeDist MockVoter ts
  , iStateStakepoolsDistribution = elaborateStakeDist MockEndorser ts
  , iStateCurrentSlot            = tsCurrentSlot ts
  , iStateEpochFirstSlot         = tsCurrentSlot ts
  , iStateSlotsPerEpoch          = tsSlotsPerEpoch ts
  , iStateR_a                    = tsAdversarialStakeRatio ts
  , updateSt                     = Update.initialState (tsGenesisProtocol ts)
  }

elaborateStakeDist
  :: Identifiable a
  => (Participant -> a)
  -> Scenario Simple
  -> StakeDistribution (Id a)
elaborateStakeDist fParticipant ts
  = StakeDistribution.fromList
  $ fmap (first (_id . fParticipant))
  $ Map.toList
  $ tsParticipants ts

--------------------------------------------------------------------------------
-- Generator state supporting definitions
--------------------------------------------------------------------------------

data VotersBehavior
  = MostReject
  | MostAbstain
  | MostApprove
  | Uniform
  -- ^ There is an equal probability that a voter decides to reject, abstain, or
  -- approve a proposal.
  deriving (Eq, Show)

-- | Generate voters' behavior. We want to generate an approval behavior in most
-- of the cases, since we want a high probability of proposals moving to the
-- next phases.
genVotersBehavior :: Gen VotersBehavior
genVotersBehavior =
  frequency [ (3, pure MostApprove)
            , (1, pure MostAbstain)
            , (1, pure MostReject)
            , (1, pure Uniform)
            ]

confidenceFrequenciesFor :: VotersBehavior -> [(Int, Gen Proposal.Confidence)]
confidenceFrequenciesFor MostReject
  = [ (2, pure For)
    , (7, pure Against)
    , (1, pure Abstain)
    ]
confidenceFrequenciesFor MostAbstain
  = [ (2, pure For)
    , (1, pure Against)
    , (7, pure Abstain)
    ]
confidenceFrequenciesFor MostApprove
  = [ (7, pure For)
    , (2, pure Against)
    , (1, pure Abstain)
    ]
confidenceFrequenciesFor Uniform
  = [ (1, pure For)
    , (1, pure Against)
    , (1, pure Abstain)
    ]

data EndorsersBehavior
  = MostEndorse
  | MostDoNotEndorse
  | EqualEndorsementChance
  -- ^ Given an endorser there is an equal probability that it endorses as that
  -- it does not.
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Test setup generations
--------------------------------------------------------------------------------

instance Arbitrary (Scenario Simple) where
  arbitrary = do
    k   <- frequency [ (8, choose (1, 5))
                     -- , (2, ???) -- TODO: what is the maximum value we should test with?
                     ]
    mvp <- VotingPeriod <$> choose (1, 4)
    cs  <- SlotNo <$> choose (0, 10)
    -- We need to make sure @2 * 2 * k < spe@, since this is required by the
    -- @endorsement@ function.
    spe <- SlotNo <$> choose (2 * 2 * k + 1, 10 * k)
    ra  <- choose (0.00, 0.35)
    ps  <- genInitialStake
    gp  <- genGenesisProtocol
    ups <- genUpdateSpecs gp (Map.keys ps)

    acts <- genActions (Map.keys ps) ups

    pure $ UpdateScenario
           { tsK                     = BlockNo k
           , tsMaxVotingPeriods      = mvp
           , tsCurrentSlot           = cs
           , tsSlotsPerEpoch         = spe
           , tsAdversarialStakeRatio = ra
           , tsParticipants          = ps
           , tsGenesisProtocol       = gp
           , tsUpdateSpecs           = assertAndReturn (allIdsAreDifferent gp)
                                                       ups
           , tsActions               = acts
           }
      where
        allIdsAreDifferent gp scenarios = do
          allUnique $ fmap getUpdateSpecId scenarios
          allUnique $ fmap getSIPSubmission scenarios
          allUnique $ fmap getSIPRevelation scenarios
          allUnique $ fmap getImplSubmission scenarios
          allUnique $ fmap (mpId . getImpl) scenarios
          allUnique $  fmap (_id . getProtocol) scenarios
                    ++ [mpProtocolId gp]

  shrink ts
    = fmap pruneActions
    -- NOTE: that we do not prune random actions. Since they are used to generate
    -- invalid signals it does not matter if they refer to non existing
    -- proposals or participants.
    $ getSuccs
    $ UpdateScenario
    <$> Succs (tsK ts)
              (fmap BlockNo
               $ filter (/= 0)
               $ shrink
               $ unBlockNo
               $ tsK ts
              )
    <*> Succs (tsMaxVotingPeriods ts)
              (fmap VotingPeriod
               $ filter (/= 0)
               $ shrink
               $ unVotingPeriod
               $ tsMaxVotingPeriods ts
              )
    <*> Succs (tsCurrentSlot ts)
              (fmap SlotNo
              $ shrink
              $ unSlotNo
              $ tsCurrentSlot ts
              )
    <*> Succs (tsSlotsPerEpoch ts)
              (fmap SlotNo
              $ filter ((2 * 2 * unBlockNo (tsK ts)) <)
              $ shrink
              $ unSlotNo
              $ tsSlotsPerEpoch ts
              )
    <*> elemAndShrinks (tsAdversarialStakeRatio ts)
    <*> Succs (tsParticipants ts)
              (fmap Map.fromList
               $ filter (not . null)
               $ shrinkList shrinkStakeOnly
               $ Map.toList
               $ tsParticipants ts
              )
    <*> pure (tsGenesisProtocol ts)
    <*> Succs (tsUpdateSpecs ts)
              (filter (not . null)
               $ shrinkList shrinkUpdateSpec (tsUpdateSpecs ts)
              )
    <*> Succs (tsActions ts)
              (shrinkList shrinkNothing (tsActions ts))
              -- We do not shrink the actions since they refer to update
              -- specifications in the test setup. Shrinking them might
              -- invalidate the reference.
    where
      -- Votes and endorsements in the actions refer to participant id's. To
      -- avoid invalidating these actions we need to keep the participant
      -- un-shrunk.
      shrinkStakeOnly :: (Participant, Stake) -> [(Participant, Stake)]
      shrinkStakeOnly (participant, Stake stake) =
        [(participant, Stake shrunkStake) | shrunkStake <- shrink stake ]

-- | Remove from the test-setup's actions those that refer to update
-- specifications that do not exist in @tsUpdateSpecs@.
pruneActions :: Scenario Simple -> Scenario Simple
pruneActions ts =
  ts { tsActions = filter (\act -> any (act `belongsTo`) specs) (tsActions ts)}
  where
    specs = tsUpdateSpecs ts

shrinkUpdateSpec :: UpdateSpec -> [UpdateSpec]
shrinkUpdateSpec aSpec
  = getSuccs
  $ (\sipVpd implVpd ->
    aSpec { getSIPRevelation = (getSIPRevelation aSpec)
                               { reveals = (reveals (getSIPRevelation aSpec))
                                           { mpVotingPeriodDuration = SlotNo sipVpd }
                               }
          , getImplRevelation = (getImplRevelation aSpec)
                                { reveals = (reveals (getImplRevelation aSpec))
                                            { mpVotingPeriodDuration = SlotNo implVpd }
                                }
          }
    )
  <$> (elemAndShrinks $ unSlotNo $ votingPeriodDuration $ getSIP aSpec)
  <*> (elemAndShrinks $ unSlotNo $ votingPeriodDuration $ getImpl aSpec)

elemAndShrinks :: Arbitrary a => a -> Succs a
elemAndShrinks a = Succs a (shrink a)

-- | Generate an initial stake distribution.
genInitialStake :: Gen (Map Participant Stake)
genInitialStake = do
  -- In the tests we need to make sure we generate cases where conflicting
  -- proposals (i.e. proposals with the same version) are accepted at the same
  -- slot. By having 1 participant in 5/8 of the cases we increase the
  -- probabilities of generating such scenario since there's only one positive
  -- vote needed for approval.
  numParticipants  <- frequency [ (5, pure 1)
                                , (2, pure 2)
                                , (1, choose (3, 10))
                                ]
  let participants =  fmap mkParticipant [1, numParticipants]
  stakes           <- vectorOf (fromIntegral numParticipants) genStake
  pure $ Map.fromList
       $ zip participants stakes
  where
    genStake = StakeDistribution.Stake <$> choose (1, 5)

genGenesisProtocol :: Gen (Protocol MockImpl)
genGenesisProtocol =
  -- NOTE: we start always from zero. I cannot imagine the genesis protocol
  -- having influence in the update mechanism behavior, so I rather keep things
  -- simple (in this way we don't need to shrink the genesis protocol).
  pure $ MockProtocol
         { mpProtocolId        = ProtocolId 0
         , mpProtocolVersion   = Version 0
         , mpSupersedesId      = ProtocolId 0
         , mpSupersedesVersion = Version 0
         }

genUpdateSpecs
  :: Protocol MockImpl
  -> [Participant]
  -> Gen [UpdateSpec]
genUpdateSpecs genesisProtocol participants = do
  numUpdates <- choose (1, 20 :: Word64)
  genUpdates numUpdates (nextId (mpSupersedesId genesisProtocol)) [genesisProtocol] []
  where
    genUpdates 0 _             _         acc  = pure acc
    genUpdates n nextProtocolId protocols acc = do
      sipAuthor          <- elements participants
      implAuthor         <- elements participants
      supersedesProtocol <- elements protocols
      -- TODO: we should test that non-increasing versions are rejected. For
      -- this we could decrease the version in some cases, instead of always
      -- increasing it.
      -- We want to have high probability of generating update proposals with
      -- conflicting versions. In 2/3 of the cases we generate a proposals that
      -- increment the version by 1 which should increase the rate of
      -- conflicting proposals.
      versionIncrement     <- frequency [ (2, pure 1)
                                        , (1, choose (2, 5))
                                        ]
      -- The system allows a voting period duration of 0, so we generate such
      -- case as well.
      votingDuration       <- choose (0, 10)
      let
         ProtocolId i   = nextProtocolId
         nextVersion    =
           mpProtocolVersion supersedesProtocol `increaseVersion` versionIncrement
         updateSpec     =
            dummyProtocolUpdateSpec (SpecId i)
                                    sipAuthor
                                    implAuthor
                                    supersedesProtocol
                                    nextVersion
                                    (SlotNo votingDuration)
         protocolUpdate = getProtocol updateSpec
      genUpdates (n - 1) (nextId nextProtocolId) (protocolUpdate:protocols) (updateSpec:acc)

-- * Elaboration of SUT actions from pre-generated actions

-- ** Action generation

-- | Generate a list of random actions, which are either clock ticks or refer to
-- one of the given update specifications. In case of votes or endorsements,
-- the authors of these must belong to the given set of participants.
--
-- This generator generates votes and endorsements in such a way that it covers
-- combinations of the following scenarios for each update specification:
--
-- - The majority of participants (approve | reject | abstain | ignore) the SIP
-- - The majority of participants (approve | reject | abstain | ignore) the implementation.
-- - The majority of participants (endorse | do not endorse) the implementation.
--
genActions :: [Participant] -> [UpdateSpec] -> Gen [UpdateAction]
genActions participants specs = do
  -- TODO: the trace length might be determined based on parameters like @k@,
  -- number of slots per epoch, number of participants, etc. Also we can tune
  -- this based on the coverage metrics: if shorter traces cover all the
  -- relevant cases, then there is no need to use such high number.
  traceLength <- choose (1, 1000)
  let nrSpecs = length specs
  sipVotersBehaviors <- vectorOf nrSpecs genVotersBehavior
  implVotersBehavior <- vectorOf nrSpecs genVotersBehavior
  vectorOf traceLength
    $ oneof
    $ fmap (uncurry3 genActionsFor)
    $ zip3 sipVotersBehaviors
           implVotersBehavior
           specs
  where
    genActionsFor
      :: VotersBehavior
      -> VotersBehavior
      -> UpdateSpec
      -> Gen UpdateAction
    genActionsFor sipVotersBehavior implVotersBehavior aSpec =
      -- TODO: we also need to generate actions in which the signature of the
      -- action does not verify.
      frequency [ (20, genAction)
                , (1, pure JustTick)
                ]
      where
        genAction =
          oneof [ -- Ideation
                  pure $ SIPCommit $ getUpdateSpecId aSpec
                , pure $ SIPReveal $ getUpdateSpecId aSpec
                , fmap SIPVote
                  $ genSIPVote participants (Just sipVotersBehavior) aSpec
                  -- Approval
                , pure $ ImplCommit $ getUpdateSpecId aSpec
                , pure $ ImplReveal $ getUpdateSpecId aSpec
                , fmap ImplVote
                  $ genImplVote participants (Just implVotersBehavior) aSpec
                -- Activation
                , fmap ImplEndorsement
                  $ genEndorsement participants aSpec
                ]

    uncurry3 f (a, b, c) = f a b c

-- ** Action pruning

-- | Determine whether the action belongs to the given update specification.
belongsTo :: UpdateAction -> UpdateSpec -> Bool
belongsTo JustTick            _    = True
belongsTo (SIPCommit specId)  spec = specId == getUpdateSpecId spec
belongsTo (SIPReveal specId)  spec = specId == getUpdateSpecId spec
belongsTo (SIPVote vote)      spec = candidate vote == getSIPId spec
belongsTo (ImplCommit specId) spec = specId == getUpdateSpecId spec
belongsTo (ImplReveal specId) spec = specId == getUpdateSpecId spec
belongsTo (ImplVote vote)     spec = candidate vote == getImplId spec
belongsTo (ImplEndorsement endorsement) spec =
  Update.endorsedVersion endorsement == protocolVersion spec

-- NOTE: we might consider whether it'd make sense to shrink actions that refer
-- to participants that don't exist anymore.
