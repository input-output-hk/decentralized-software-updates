
module Cardano.Ledger.Spec.Classes.HasSalt where

class HasSalt a where
  salt :: a -> Int
