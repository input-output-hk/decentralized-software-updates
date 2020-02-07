{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.OddChain where

import           Crypto.Number.Generate (generateBetween)
import           Data.Time.Calendar (Day (ModifiedJulianDay))
import           Data.Time.Clock (UTCTime (UTCTime), utctDay, utctDayTime)
import           Data.Word (Word64)
import           Test.QuickCheck (Property, tabulate, withMaxSuccess)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Cardano.Slotting.Slot (unEpochSize)

import           Ouroboros.Consensus.BlockchainTime.Mock
                     (countSlotLengthChanges)
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (ProtocolInfo (ProtocolInfo), pInfoConfig,
                     pInfoInitLedger, pInfoInitState)
import           Ouroboros.Consensus.Protocol.Abstract
                     (SecurityParam (SecurityParam))

import           Test.ThreadNet.General (TestConfig (TestConfig),
                     TestConfigBlock (TestConfigBlock), forgeEbbEnv, nodeInfo,
                     numCoreNodes, numSlots, plainTestNodeInitialization,
                     prop_general, rekeying, runTestNetwork, slotLengths)
import           Test.ThreadNet.TxGen (TxGen, testGenTx)
import           Test.ThreadNet.Util (roundRobinLeaderSchedule)
import           Test.Util.Orphans.Arbitrary ()

import           OddChain (GenTx (OddTx), OddBlock, Phase (Decrease, Increase),
                     Tx (Tx), mkBftTopLevelConfig, mkExtLedgerState,
                     oddBlockPayload, phase)


tests :: TestTree
tests = testGroup "Odd Chain" [
      testProperty "simple convergence" $
        prop_simple_oddchain_convergence k
    ]
  where
    k = SecurityParam 5

-- | At a high level, this property tests if the consensus layer is behaving as
-- expected. This is:
--
-- - The competitive chains at the end of the simulation respect the expected
--   bound on fork length.
--
-- - The nodes do not leak file handles.
--
-- - Blocks are exchanged without unexpected delays.
--
-- - The nodes' chains grow without unexpected delays.
--
-- - No blocks are unduly rejected
--
-- - New blocks are promptly conveyed to neighboring nodes.
--
prop_simple_oddchain_convergence :: SecurityParam
                                 -> TestConfig
                                 -> Property
prop_simple_oddchain_convergence
  k
  testConfig@TestConfig{numCoreNodes, numSlots, slotLengths}
  = tabulate "slot length changes" [show $ countSlotLengthChanges numSlots slotLengths]
  $ withMaxSuccess 1000
  $ prop_general
      countOddTxs
      k
      testConfig
      (Just $ roundRobinLeaderSchedule numCoreNodes numSlots) -- Q: What happens if I pass 'Nothing' here?
      Nothing
      -- Bounds the length of competing suffixes at the end of the test. Just 0
      -- means we expect full consensus. Just 1 means they might are allowed to
      -- disagree about the last block. Nothing uses a crude approximation.
      (const False)
      0 -- Block number of the first proper block after genesis.
      testOutput
  where
    testOutput =
        runTestNetwork testConfig numSlotsPerEpoch TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo = \nid ->
                plainTestNodeInitialization
                ProtocolInfo
                { pInfoConfig     =
                    mkBftTopLevelConfig k nid numCoreNodes (unEpochSize numSlotsPerEpoch) slotLengths startTime
                , pInfoInitState  = () -- :: NodeState  (BlockProtocol b)
                , pInfoInitLedger = mkExtLedgerState
                }
            , rekeying = Nothing
            }
    ----------------------------------------------------------------------------
    -- We're using constants for now:
    ----------------------------------------------------------------------------
    numSlotsPerEpoch = 10
    startTime =
        UTCTime
        { utctDay = ModifiedJulianDay 58849 -- 01-01-2020
        , utctDayTime = 0
        }
    ----------------------------------------------------------------------------
    -- END: We're using constants for now.
    ----------------------------------------------------------------------------


countOddTxs :: OddBlock -> Word64
countOddTxs = fromIntegral . length . oddBlockPayload

instance TxGen OddBlock where
  testGenTx _numCoreNodes _slotNo _cfg ledgerSt
    = fmap (OddTx . Tx . fromIntegral)
    $ case phase ledgerSt of
        Decrease i -> generateBetween (-1000)              (fromIntegral (i-1))
        Increase i -> generateBetween (fromIntegral (i+1)) 1000
