
module Cardano.Ledger.Debug where

import           Control.Monad.State (MonadState, get)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Debug.Trace as Debug
import qualified Text.Pretty.Simple as Pretty


dumpState :: (Show s, MonadState s m) => m ()
dumpState = do
  state <- get
  let stateStr = Text.Lazy.unpack $ Pretty.pShow state
  Debug.traceM ("Current state: \n" ++ stateStr )
