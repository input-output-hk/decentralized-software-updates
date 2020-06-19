{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Update.Properties where

import           Control.Arrow (first, second, (***))
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes)
import           Data.Word (Word64)
import           Test.Tasty (TestTree)
import           Test.Tasty.QuickCheck (testProperty)

-- TODO: import explicitly
import           Control.Applicative.Successors (Succs (Succs), getSuccs)
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements,
                     expectFailure, frequency, oneof, shrink, shrinkList,
                     vectorOf, withMaxSuccess)

import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Block (BlockNo (BlockNo), unBlockNo)
import           Cardano.Slotting.Slot (SlotNo (SlotNo), unSlotNo)

import           Cardano.Ledger.Assert (allUnique, assertAndReturn, prettyShow)

import           Cardano.Ledger.Update.Env.StakeDistribution (Stake (Stake),
                     StakeDistribution, getStake)
import           Cardano.Ledger.Update.Env.TracksSlotTime (currentSlot)
import           Cardano.Ledger.Update.Proposal
import           Cardano.Ledger.Update.ProposalsState
                     (Decision (Approved, Expired, Rejected, WithNoQuorum),
                     VotingPeriod (VotingPeriod), unVotingPeriod)

import qualified Cardano.Ledger.Update as Update
import qualified Cardano.Ledger.Update.Env.StakeDistribution as StakeDistribution
import qualified Cardano.Ledger.Update.Proposal as Proposal

import           Properties
import           SUTTest
import           Test.Cardano.Ledger.Update.Data
import           Test.Cardano.Ledger.Update.Interface
import           Test.Cardano.Ledger.UpdateSpec

-- TODO: we wouldn't need this if @IState@ was not polymorphic. This import is
-- needed only to bring the @HasStakeDistribution@ instance into scope. So
-- consider making @IState@ monomorphic.
import           Test.Cardano.Ledger.Update.TestCase ()


runTests :: [TestTree]
runTests = [
           ---------------------------------------------------------------------
           -- Ideation phase liveness properties
           ---------------------------------------------------------------------
             testProperty "SIP's are expired"
             $ expectFailure
             $ tracePropShow (sipsAreNot Expired)
                             showActionsAndStateOfUpdateSpec
           , testProperty "SIP's are rejected"
             $ expectFailure
             $ tracePropShow (sipsAreNot Rejected)
                             showActionsAndStateOfUpdateSpec
           , testProperty "SIP's get no-quorum"
             $ expectFailure
             $ tracePropShow (sipsAreNot WithNoQuorum)
                             showActionsAndStateOfUpdateSpec
           , testProperty "SIP's are approved"
             $ expectFailure
             $ tracePropShow (sipsAreNot Approved)
                             showActionsAndStateOfUpdateSpec
           ---------------------------------------------------------------------
           -- Implementation phase liveness properties
           ---------------------------------------------------------------------
           , testProperty "Implementations are expired"
             $ expectFailure
             $ tracePropShow (implsAreNot Expired)
                             showActionsAndStateOfUpdateSpec
           , testProperty "Implementations are rejected"
             $ expectFailure
             $ withMaxSuccess 100
             $ tracePropShow (implsAreNot Rejected)
                             showActionsAndStateOfUpdateSpec
           , testProperty "Implementations get no-quorum"
             $ expectFailure
             $ tracePropShow (implsAreNot WithNoQuorum)
                             showActionsAndStateOfUpdateSpec
           -- We do not test that implementations are approved since that is
           -- implicitly tested when we test that updates are activated.
           ---------------------------------------------------------------------
           -- Activation phase liveness properties
           ---------------------------------------------------------------------
           , testProperty "Implementations are activated"
             $ expectFailure
             $ tracePropShow updatesAreNotActivated
                             showActionsAndStateOfUpdateSpec
           , testProperty "Implementations are queued"
             $ expectFailure
             $ tracePropShow updatesAreNotQueued
                             showActionsAndStateOfUpdateSpec
           , testProperty "Updates are discarded due to being expired"
             $ expectFailure
             $ tracePropShow (updatesAreNotDiscardedDueToBeing Update.Expired)
                             showActionsAndStateOfUpdateSpec
           , testProperty "Updates are discarded due to being unsupported"
             $ expectFailure
             $ withMaxSuccess 10000
             $ tracePropShow (updatesAreNotDiscardedDueToBeing Update.Unsupported)
                             showActionsAndStateOfUpdateSpec
           , testProperty "Updates are discarded due to being obsoleted"
             $ expectFailure
             $ withMaxSuccess 10000
             $ tracePropShow (updatesAreNotDiscardedDueToBeing Update.Obsoleted)
                             showActionsAndStateOfUpdateSpec
           ---------------------------------------------------------------------
           -- Update system safety properties
           ---------------------------------------------------------------------
           -- , testProperty "Approved SIP's meet the adoption threshold"
           --   $ traceProp prop_approvedSIPsHaveEnoughVotes

           -- TODO: scheduled proposals cannot be canceled.
           ---------------------------------------------------------------------
           -- Coverage testing
           ---------------------------------------------------------------------
           -- TODO: test that the cases below are generated:
           -- - SIP's and implementations got approved in the second voting period.
           -- - SIP's and implementations got approved in the last voting period.
           -- - protocol updates got canceled:
           --   - before approval
           --   - when the update was waiting in the queue
           --   - when the update was a candidate
           -- - protocol updated is adopted after being endorsed across several
           --   epochs.
           ]

--------------------------------------------------------------------------------
-- Definition of the system under test interface
--------------------------------------------------------------------------------

data UpdateSUT

instance SUT UpdateSUT where

  newtype SUTSt UpdateSUT = UpdateSt { unUpdateSt :: IState MockSIP MockImpl }
    deriving (Show)

  data SUTAct UpdateSUT
    = TickAct SlotNo
    -- ^ The slot number determines how many slots to tick, relative to the
    -- current slot, which is known to the @apply@ function.
    | UpdateAct (Update.Payload MockSIP MockImpl)
    -- TODO: we need actions to update the environment: in particular an action
    -- for changing the stake.
    deriving (Show)

  apply sutAct (UpdateSt st)
    = either (const Nothing) (Just . UpdateSt)
    $ case sutAct of
        TickAct slot      -> slotTick (currentSlot st + slot) st
        UpdateAct payload -> applyUpdate payload st
        -- TODO: here we need a case for applying a stake change.

instance Update.HasIdeationState (SUTSt UpdateSUT) MockSIP where
  getIdeationState (UpdateSt st) = Update.getIdeationState st

instance Update.HasApprovalState (SUTSt UpdateSUT) MockImpl where
  getApprovalState (UpdateSt st) = Update.getApprovalState st

instance Update.HasActivationState (SUTSt UpdateSUT) MockSIP MockImpl where
  getActivationState (UpdateSt st) = Update.getActivationState st

--------------------------------------------------------------------------------
-- Definition of the update test generator
--------------------------------------------------------------------------------

data UpdateTestGen

instance TestGen UpdateSUT UpdateTestGen where
  data TestSetup UpdateTestGen =
    UpdateTestSetup
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
    , tsUpdateProposals       :: ![ProposalScenario]
    } deriving (Eq, Show)

  data GenSt UpdateTestGen =
    UpdateGenSt { unUpdateTestGen :: ![ProposalState] }
    deriving (Show)

  canTerminate =
    all (finalState . genMode) . unUpdateTestGen


  mkInitStates ts =
    ( mkInitialGeneratorState ts
    , UpdateSt (elaborateSUTInitialState ts)
    )

  update (UpdateGenSt states) (UpdateSt sutSt) =
    UpdateGenSt $ fmap (updateGenSt sutSt) states

  genAct setup (UpdateGenSt proposalsStates) = do
    acts <- catMaybes <$> traverse (tryGenAct setup) proposalsStates
    case acts of
      [] -> pure $ TickAct 1
            -- It was not possible to generate an action, so we need to generate
            -- a slot tick. We tick with slot 1, since we do not want to miss
            -- the slot ticks in the generators (otherwise a proposal that
            -- should be approved might get expired).
      _  -> frequency [ -- We want to make sure that time will pass regardless
                        -- of whether actions can be generated for the proposals
                        -- in the test setup. Remember that there will be
                        -- invalid actions generated for the different
                        -- proposals, so the chances of not generating an action
                        -- might be quite low.
                        (2, pure $! TickAct 1)
                      , (8, elements acts)
                      ]

updateGenSt
  :: IState MockSIP MockImpl
  -> ProposalState
  -> ProposalState
updateGenSt sutSt proposalState =
  proposalState
  { genMode =
      case stateOf updateSpec sutSt of
        -- Ideation
        SIP StablySubmitted         -> RevealingSIP
        SIP StablyRevealed          -> VotingForSIP
        SIP (IsStably Approved)     -> SubmittingImpl
        SIP (IsStably Rejected)     -> Done
        SIP (IsStably WithNoQuorum) -> Done
        SIP (IsStably Expired)      -> Done
        -- Implementation
        Implementation StablySubmitted         -> RevealingImpl
        Implementation StablyRevealed          -> VotingForImpl
        -- Note that an approved implementation goes to the activation phase,
        -- where it can enter the queue, the endorsement period, or be
        -- discarded.
        Implementation (IsStably Rejected)     -> Done
        Implementation (IsStably WithNoQuorum) -> Done
        Implementation (IsStably Expired)      -> Done
        -- Activation
        Activated             -> Done
        BeingEndorsed         -> Endorsing
        Queued                -> Waiting
        ActivationExpired     -> Done
        ActivationCanceled    -> Done
        ActivationUnsupported -> Done
        Obsoleted             -> Done
        -- Otherwise stay in the same mode.
        _ -> genMode proposalState

  }
  where
    updateSpec = spec (scenario proposalState)

tryGenAct
  :: TestSetup UpdateTestGen -> ProposalState -> Gen (Maybe (SUTAct UpdateSUT))
tryGenAct setup st
  | genMode st == Done = pure Nothing
  | otherwise          =  oneof [ randomAction setup st
                                , bestEffortGenerator setup st
                                ]

randomAction
  :: TestSetup UpdateTestGen -> ProposalState -> Gen (Maybe (SUTAct UpdateSUT))
randomAction setup st
  = fmap Just
  $ oneof [ genSIPSubmission st
          , genSIPRevelation st
          , genSIPVote setup st
          ]

genSIPSubmission :: ProposalState ->  Gen (SUTAct UpdateSUT)
genSIPSubmission st =
  pure $! UpdateAct . Update.Ideation . Proposal.Submit
       $! getSIPSubmission
       $  spec
       $  scenario st

genImplSubmission :: ProposalState ->  Gen (SUTAct UpdateSUT)
genImplSubmission st =
  pure $! UpdateAct . Update.Approval . Proposal.Submit
       $! getImplSubmission
       $  spec
       $  scenario st

genSIPRevelation :: ProposalState -> Gen (SUTAct UpdateSUT)
genSIPRevelation st =
  pure $! UpdateAct . Update.Ideation . Proposal.Reveal
       $! getSIPRevelation
       $  spec
       $  scenario st

genImplRevelation :: ProposalState -> Gen (SUTAct UpdateSUT)
genImplRevelation st =
  pure $! UpdateAct . Update.Approval . Proposal.Reveal
       $! getImplRevelation
       $  spec
       $  scenario st

genSIPVote :: TestSetup UpdateTestGen -> ProposalState -> Gen (SUTAct UpdateSUT)
genSIPVote setup st = do
  participant  <- fmap MockVoter $ elements $ Map.keys $ tsParticipants setup
  vConfidence  <- frequency $ confidenceFrequenciesFor
                            $ sipVotersBehavior
                            $ scenario st
  voteVerifies <- frequency [(8, pure True), (2, pure False)]
  pure $! UpdateAct . Update.Ideation . Proposal.Cast
       $! MockVote (_id participant) (_id sip) vConfidence voteVerifies
  where
    sip = getSIP $ spec $ scenario st

genImplVote :: TestSetup UpdateTestGen -> ProposalState -> Gen (SUTAct UpdateSUT)
genImplVote setup st = do
  participant  <- fmap MockVoter $ elements $ Map.keys $ tsParticipants setup
  vConfidence  <- frequency $ confidenceFrequenciesFor
                            $ implVotersBehavior
                            $ scenario st
  voteVerifies <- frequency [(8, pure True), (2, pure False)]
  pure $! UpdateAct . Update.Approval . Proposal.Cast
       $! MockVote (_id participant) (_id impl) vConfidence voteVerifies
  where
    impl = getImpl $ spec $ scenario st

genMaybeEndorsement
  :: TestSetup UpdateTestGen -> ProposalState -> Gen (Maybe (SUTAct UpdateSUT))
genMaybeEndorsement setup st =
  frequency [ (yes, fmap Just genEndorsement)
            , (no , pure Nothing)
            ]
  where
    (yes, no)      = endorsementFrequency $ endorsersBehavior $ scenario st
    genEndorsement = do
      endorser <- fmap MockEndorser $ elements $ Map.keys $ tsParticipants setup
      pure $! UpdateAct . Update.Activation
           $! Update.Endorsement
              { Update.endorserId      = _id endorser
              , Update.endorsedVersion = protocolVersion $ spec $ scenario st
              }

bestEffortGenerator
  :: TestSetup UpdateTestGen -> ProposalState -> Gen (Maybe (SUTAct UpdateSUT))
bestEffortGenerator setup st =
  case genMode st of
    SubmittingSIP -> fmap Just (genSIPSubmission st)
    RevealingSIP  -> fmap Just (genSIPRevelation st)
    VotingForSIP  -> fmap Just (genSIPVote setup st)
    -- Implementation
    SubmittingImpl -> fmap Just (genImplSubmission st)
    RevealingImpl  -> fmap Just (genImplRevelation st)
    VotingForImpl  -> fmap Just (genImplVote setup st)
    -- Activation
    Endorsing      -> genMaybeEndorsement setup st
    Waiting        -> pure Nothing
    Done           -> pure Nothing

mkInitialGeneratorState
  :: TestSetup UpdateTestGen -> GenSt UpdateTestGen
mkInitialGeneratorState ts =
  UpdateGenSt $ fmap idleProposalState $ tsUpdateProposals ts
  where
    idleProposalState someScenario =
      ProposalState
      { scenario = someScenario
      , genMode  = SubmittingSIP
      }

elaborateSUTInitialState
  :: TestSetup UpdateTestGen -> IState MockSIP MockImpl
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
  -> TestSetup UpdateTestGen
  -> StakeDistribution (Id a)
elaborateStakeDist fParticipant ts
  = StakeDistribution.fromList
  $ fmap (first (_id . fParticipant))
  $ Map.toList
  $ tsParticipants ts

--------------------------------------------------------------------------------
-- Generator state supporting definitions
--------------------------------------------------------------------------------

data ProposalScenario =
  ProposalScenario
  { spec               :: !UpdateSpec
  , sipVotersBehavior  :: !VotersBehavior
  , implVotersBehavior :: !VotersBehavior
  , endorsersBehavior  :: !EndorsersBehavior
  } deriving (Eq, Show)

data VotersBehavior
  = MostReject
  | MostAbstain
  | MostApprove
  | Uniform
  -- ^ There is an equal probability that a voter decides to reject, abstain, or
  -- approve a proposal.
  deriving (Eq, Show)

genVotersBehavior :: Gen VotersBehavior
genVotersBehavior = elements [ MostReject, MostAbstain, MostApprove, Uniform ]

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

genEndorsersBehavior :: Gen EndorsersBehavior
genEndorsersBehavior = elements [ MostEndorse
                                , MostDoNotEndorse
                                , EqualEndorsementChance
                                ]

-- | Given an endorsers behavior, return a tuple where the first component is
-- the endorsement generator weight (see @frequency@), and the second component
-- is the no-endorsement generator weight.
endorsementFrequency :: EndorsersBehavior -> (Int, Int)
endorsementFrequency MostEndorse            = (8, 2)
endorsementFrequency MostDoNotEndorse       = (2, 8)
endorsementFrequency EqualEndorsementChance = (1, 1)

data ProposalState =
  ProposalState
  { scenario :: !ProposalScenario
  , genMode  :: !GenMode
  } deriving (Show)

-- | State of the generator for a single proposal.
data GenMode
  = SubmittingSIP
  | RevealingSIP
  | VotingForSIP
  | SubmittingImpl
  | RevealingImpl
  | VotingForImpl
  | Endorsing
  | Waiting
  | Done
  deriving (Eq, Show)

finalState :: GenMode -> Bool
finalState Done    = True
finalState Waiting = True -- The waiting state is a final state in the sense
                          -- that if all other proposals are done or waiting,
                          -- then there is no further action that can be taken
                          -- and the generation should terminate.
finalState _       = False

--------------------------------------------------------------------------------
-- Test setup generations
--------------------------------------------------------------------------------

instance Arbitrary (TestSetup UpdateTestGen) where
  arbitrary = do
    k   <- frequency [ (8, choose (1, 5))
                     -- , (2, ???) -- TODO: what is the maximum value we should test with?
                     ]
    mvp <- VotingPeriod <$> choose (1, 4)
    cs  <- SlotNo <$> choose (0, 10)
    spe <- SlotNo <$> choose (1, k * 10)
    ra  <- choose (0.00, 0.35)
    ps <- genInitialStake
    gp  <- genGenesisProtocol
    ups <- genProposalScenarios gp (Map.keys ps)
    pure $ UpdateTestSetup
           { tsK                     = BlockNo k
           , tsMaxVotingPeriods      = mvp
           , tsCurrentSlot           = cs
           , tsSlotsPerEpoch         = spe
           , tsAdversarialStakeRatio = ra
           , tsParticipants          = ps
           , tsGenesisProtocol       = gp
           , tsUpdateProposals       = assertAndReturn (allIdsAreDifferent gp)
                                                       ups
           }
      where
        allIdsAreDifferent gp scenarios = do
          allUnique $ fmap (getUpdateSpecId . spec) scenarios
          allUnique $ fmap (getSIPSubmission . spec) scenarios
          allUnique $ fmap (getSIPRevelation . spec) scenarios
          allUnique $ fmap (getImplSubmission . spec) scenarios
          allUnique $ fmap (mpId . getImpl . spec) scenarios
          allUnique $  fmap (_id . getProtocol . spec) scenarios
                    ++ [mpProtocolId gp]

  shrink ts
    = getSuccs
    $ UpdateTestSetup
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
              $ filter (/= 0)
              $ shrink
              $ unSlotNo
              $ tsSlotsPerEpoch ts
              )
    <*> elemAndShrinks (tsAdversarialStakeRatio ts)
    <*> Succs (tsParticipants ts)
              (fmap (Map.fromList . fmap (mkParticipant *** Stake))
              $ filter (not . null)
              $ shrink
              $ fmap ((unParticipantId . _id) *** getStake)
              $ Map.toList
              $ tsParticipants ts
              )
    <*> pure (tsGenesisProtocol ts)
    <*> Succs (tsUpdateProposals ts)
              (filter (not . null)
               $ shrinkList shrinkProposalScenario (tsUpdateProposals ts)
              )

shrinkProposalScenario :: ProposalScenario -> [ProposalScenario]
shrinkProposalScenario aScenario =
  [ aScenario { spec = spec'} | spec' <- shrinkUpdateSpec (spec aScenario)]

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
  numParticipants  <- choose (1, 10)
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

genProposalScenarios
  :: Protocol MockImpl
  -> [Participant]
  -> Gen [ProposalScenario]
genProposalScenarios genesisProtocol participants = do
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
      versionIncrement     <- choose (1, 5)
      aSipVotersBehavior   <- genVotersBehavior
      anImplVotersBehavior <- genVotersBehavior
      anEndorsersBehavior  <- genEndorsersBehavior
      votingDuration       <- choose (0, 10)
      let
         ProtocolId i   = nextProtocolId
         nextVersion    =
           mpProtocolVersion supersedesProtocol `increaseVersion` versionIncrement
         updateSpec     =
            dummyProtocolUpdateSpec i
                                    sipAuthor
                                    implAuthor
                                    supersedesProtocol
                                    nextVersion
                                    (SlotNo votingDuration)
         protocolUpdate = getProtocol updateSpec
         proposalScenario =
           ProposalScenario
           { spec               = updateSpec
           , sipVotersBehavior  = aSipVotersBehavior
           , implVotersBehavior = anImplVotersBehavior
           , endorsersBehavior  = anEndorsersBehavior
           }
      genUpdates (n - 1) (nextId nextProtocolId) (protocolUpdate:protocols) (proposalScenario:acc)

--------------------------------------------------------------------------------
-- Liveness properties
--------------------------------------------------------------------------------

sipsAreNot :: Decision -> Trace UpdateSUT UpdateTestGen -> Bool
sipsAreNot decision =
  updatesAreNot getSIPId (\sipId st -> Update.isSIP sipId decision st)

implsAreNot :: Decision -> Trace UpdateSUT UpdateTestGen -> Bool
implsAreNot decision =
  updatesAreNot getImplId (\iid st -> Update.isImplementation iid decision st)

updatesAreNotActivated :: Trace UpdateSUT UpdateTestGen -> Bool
updatesAreNotActivated = updatesAreNot getProtocolId Update.isTheCurrentVersion

updatesAreNotQueued :: Trace UpdateSUT UpdateTestGen -> Bool
updatesAreNotQueued = updatesAreNot getProtocolId Update.isQueued

updatesAreNotDiscardedDueToBeing
  :: Update.Reason -> Trace UpdateSUT UpdateTestGen -> Bool
updatesAreNotDiscardedDueToBeing reason =
  updatesAreNot getProtocolId (\pid st -> Update.isDiscardedDueToBeing pid reason st)

updatesAreNot
  :: (UpdateSpec -> a)
  -> (a -> SUTSt UpdateSUT -> Bool)
  -> Trace UpdateSUT UpdateTestGen
  -> Bool
updatesAreNot updateSpecToA predA trace =
  not $ any anyProtocolSatisfy (validStates trace)
  where
    anyProtocolSatisfy st = any (`predA` st) as
    as                    = fmap (updateSpecToA . spec)
                          $ tsUpdateProposals
                          $ testSetup trace

--------------------------------------------------------------------------------
-- Safety properties
--------------------------------------------------------------------------------

-- | SIP's can get approved only if they have enough votes.
--
-- TODO: in a different property we can specify that a SIP can only get approved
-- @stableAfter@ slots after the voting period ends.
prop_approvedSIPsHaveEnoughVotes :: Trace UpdateSUT UpdateTestGen -> Bool
prop_approvedSIPsHaveEnoughVotes = undefined

--------------------------------------------------------------------------------
-- Custom trace rendering
--------------------------------------------------------------------------------

-- | Show the actions and the state of the update specs in the test setup.
showActionsAndStateOfUpdateSpec :: Trace UpdateSUT UpdateTestGen -> String
showActionsAndStateOfUpdateSpec trace
  =  prettyShow (testSetup trace)
  ++ "\nInitial state: \n"
  ++ prettyShow (stateOfSpecsAt (initialState trace))
  ++ "\nEvents: \n"
  ++ prettyShow (fmap (second stateOfSpecsAt) (validTransitions trace))
  where
    updateSpecs = fmap spec $ tsUpdateProposals $ testSetup trace
    stateOfSpecsAt st = fmap (`stateOf` (unUpdateSt st)) updateSpecs
