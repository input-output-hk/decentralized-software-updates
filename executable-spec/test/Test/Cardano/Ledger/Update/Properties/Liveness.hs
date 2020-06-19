-- | Properties that can be used to show that the system allows certain things
-- from happening.
--
-- TODO: this module was developed before the idea of dividing the trace in
-- events per update specification. We should determine whether these liveness
-- properties are more easily expressed using event sequences (and checking that
-- a certain event does (not) occur in an event sequence for some update spec).
module Test.Cardano.Ledger.Update.Properties.Liveness
  ( sipsAreNot
  , implsAreNot
  , updatesAreNotDiscardedDueToBeing
  , updatesAreNotQueued
  , updatesAreNotActivated
  )
where

import qualified Cardano.Ledger.Update as Update

import           Cardano.Ledger.Update.ProposalsState (Decision)

import           Trace

import           Test.Cardano.Ledger.Update.Properties.SimpleScenario
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT
import           Test.Cardano.Ledger.UpdateSpec


sipsAreNot :: Decision -> Trace UpdateSUT Simple -> Bool
sipsAreNot decision =
  updatesAreNot getSIPId (\sipId st -> Update.isSIP sipId decision st)

implsAreNot :: Decision -> Trace UpdateSUT Simple -> Bool
implsAreNot decision =
  updatesAreNot getImplId (\iid st -> Update.isImplementation iid decision st)

updatesAreNotActivated :: Trace UpdateSUT Simple -> Bool
updatesAreNotActivated = updatesAreNot getProtocolId Update.isTheCurrentVersion

updatesAreNotQueued :: Trace UpdateSUT Simple -> Bool
updatesAreNotQueued = updatesAreNot getProtocolId Update.isQueued

updatesAreNotDiscardedDueToBeing
  :: Update.Reason -> Trace UpdateSUT Simple -> Bool
updatesAreNotDiscardedDueToBeing reason =
  updatesAreNot getProtocolId (\pid st -> Update.isDiscardedDueToBeing pid reason st)

updatesAreNot
  :: (UpdateSpec -> a)
  -> (a -> SUTSt UpdateSUT -> Bool)
  -> Trace UpdateSUT Simple
  -> Bool
updatesAreNot updateSpecToA predA trace =
  not $ any anyProtocolSatisfy (validStates trace)
  where
    anyProtocolSatisfy st = any (`predA` st) as
    as                    = fmap updateSpecToA
                          $ tsUpdateSpecs
                          $ scenario trace
