{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: if we continue using the STS framework we might want to add this to the
-- 'Embed' typeclass.
module Cardano.Ledger.Spec.STS.CanExtract where

import           Data.Maybe (catMaybes)

class CanExtract e a where
  extractAll :: e -> [a]

extract :: CanExtract e a => (a -> Maybe b) -> e -> [b]
extract f = catMaybes . fmap f . extractAll
