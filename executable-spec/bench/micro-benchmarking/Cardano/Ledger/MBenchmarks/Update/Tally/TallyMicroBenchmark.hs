{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Cardano.Ledger.MBenchmarks.Update.Tally.TallyMicroBenchmark where

import            Data.List (foldr, zip, repeat, take, map)
import qualified  Data.Map.Strict as Map

import           Ledger.Core (BlockCount (BlockCount), Slot (Slot),
                     SlotCount (SlotCount), (*.), (+.))
import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))

import            Cardano.Ledger.Spec.State.ProposalsState (ProposalsState (ProposalsState), tally)
import            Cardano.Ledger.Spec.State.ProposalState ( ProposalState (ProposalState)
                                                          , Decision (Rejected, NoQuorum, Expired, Accepted, Undecided)
                                                          , VotingPeriod (VotingPeriod) 
                                                          , decision
                                                          )
import           Cardano.Ledger.Spec.STS.Update.Data
                     (Confidence (Abstain, Against, For), Stake (Stake))
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution (StakeDistribution))                                          
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)
import           Cardano.Ledger.Test.Mock (Mock)


data BenchmarkParams =
  BenchmarkParams
  { participants   :: Word
  -- ^ Number of participants.
  , concurrentSIPs :: Word
  -- ^ SIP's that are active at the same time. We assume their voting period
  -- overlaps exactly, which is the worst case.
  }
  deriving (Show, Eq)


type TallyResult = [Decision]

-- | Run the tally benchmark
benchmarkTally = undefined

-- | Get number of participants and run the tally
runTally :: BenchmarkParams -> TallyResult
runTally params = getDecisions $ tally 
                                    k 
                                    currentSlot 
                                    (stakeDist $ fromIntegral $ participants params) 
                                    r_a 
                                    (createProposalsStateMap params currentSlot)
    where
        -- create final list of all decisions
        getDecisions (ProposalsState proposalsMap) = foldr 
                                                        (\(_, pState) accum ->  
                                                            decision pState : accum 
                                                        ) 
                                                        [] 
                                                   $ Map.toList proposalsMap
        k = BlockCount 2 -- very small to ensure stability

        currentSlot = Slot 1000 -- extremely large to ensure stability of voting period end

        createListofHashVKeys pts = map (hash . VerKeyMockDSIGN) [1 .. pts]

        -- uniform stake distribution with a stake of 1 for each stakeholder
        stakeDist ptcnts = StakeDistribution $ Map.fromList 
                                            $ zip (createListofHashVKeys ptcnts) 
                                                  (map (Stake) $ replicate ptcnts 1)

        r_a = 0.49 -- adversary ratio

        createProposalsStateMap :: BenchmarkParams -> Slot -> ProposalsState Mock Word
        createProposalsStateMap bps currSlot = ProposalsState
                                $ Map.fromList
                                $ foldr (\i accum -> (hash i, createProposalState bps currSlot) : accum)
                                        []
                                        ([1 .. (concurrentSIPs bps)]) 
            where
                createProposalState :: BenchmarkParams -> Slot -> ProposalState Mock
                createProposalState bparams currSlot = 
                    ProposalState
                        (Slot 1)  -- Revealed slot - extremely small to ensure stability of voting period end
                        (SlotCount 1) -- Voting Period Duration - likewise
                        (VotingPeriod 1) -- current voting period
                        (VotingPeriod 1)  -- max voting periods allowed
                        (createBallotMap bparams)
                        Undecided -- to enable tally
                    where
                        createBallotMap bp = Map.fromList listOfBallots 
                            where
                                listOfBallots = zip  (createListofHashVKeys (fromIntegral $ participants bp))
                                                     (take (fromIntegral $ participants bp) $ repeat For)
                                    
                                        
                                                     


                                        

