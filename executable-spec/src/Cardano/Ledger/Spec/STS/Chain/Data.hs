{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Ledger.Spec.STS.Chain.Data where

import           Cardano.Prelude (HeapWords, heapWords, heapWords2)
import           Ledger.Core (Slot (Slot))
import           Data.Map.Strict (Map)
import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)

--import           Cardano.Ledger.Spec.STS.Transaction.Transaction (TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Transaction.Transaction as Dummy
import qualified Cardano.Ledger.Spec.STS.Update.Data as DataUpd

data EnvTransactions
  = EnvTransactions
    { currSlot :: Slot
    , envIdeation :: DataUpd.EnvIdeation
    }
  deriving (Eq, Show)

data StTransactions
  = StTransactions
  { txToSlot :: Map Transaction Slot
  , stIdeation :: DataUpd.StIdeation
  }
  deriving (Eq, Show, Semigroup, Monoid)

data EnvChain
  = EnvChain
    { initialSlot :: !Slot
    , maximumBlockSize :: !WordCount
    -- ^ Maximum block size. For now we measure this in number of 'Word's using
    -- 'heapWords' from 'Cardano.Prelude.HeapWords'.
    , transactionsEnv :: EnvTransactions
    }
  deriving (Eq, Show)

data StChain
  = StChain
    { currentSlot :: !Slot
    -- , submitted :: Map Dummy.Transaction Slot
    --, txToSlotChain :: Map Transaction Slot
    , transactionsSt :: StTransactions
    }
  deriving (Eq, Show)

data Block
  = Block
    { slot :: !Slot
    , transactions :: ![Transaction]
    }
    deriving (Eq, Show)

instance HeapWords Block where
  heapWords (Block (Slot s) transactions) = heapWords2 s transactions

-- | Type wrapper that gives more information about what the 'Slot' represents.
newtype CurrentSlot = CurrentSlot Slot deriving (Eq, Show)

data Transaction
  = Dummy Dummy.Transaction
  | Update DataUpd.UpdateTx
  deriving (Eq, Show, Ord)

-- unDummy :: Transaction -> Dummy.Transaction
-- unDummy (Dummy t) = t

unTransaction :: Transaction -> Either (Dummy.Transaction) (DataUpd.UpdateTx)
unTransaction (Dummy t) = Left t
unTransaction (Update t) = Right t

instance HeapWords Transaction where
  heapWords (Dummy dummyTx) = heapWords dummyTx
  heapWords (Update updateTx) = heapWords updateTx

