{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

-- We declare orphan instances for 'WalletArgs' so that we can define command
-- line parsers for values of this type.
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import           GHC.Generics (Generic)
import           Data.Word (Word64)
import           Options.Generic (ParseRecord, ParseField, ParseFields, unwrapRecord, type (<?>), (:::), Wrapped, Unwrapped)

import           OddWallet (WalletConfig (WalletConfig), runWallet, WalletTracer)
import qualified OddWallet


data WalletArgs w =
  WalletArgs
  { nodeId         :: w ::: Word64         <?> "Node identifier"
  , enabledTracers :: w ::: [WalletTracer] <?> "Tracers to enable"
  } deriving (Generic)

instance ParseRecord (WalletArgs Wrapped)
deriving instance Show (WalletArgs Unwrapped)

-- Orphan instances for 'WalletTracers'
instance ParseRecord WalletTracer
instance ParseField  WalletTracer
instance ParseFields WalletTracer

walletArgsToConfig :: WalletArgs Unwrapped -> WalletConfig
walletArgsToConfig WalletArgs { nodeId, enabledTracers } =
  WalletConfig
  { OddWallet.nodeId        = nodeId
  , OddWallet.enabledTracers = enabledTracers
  }

main :: IO ()
main = do
  config <- fmap walletArgsToConfig $ unwrapRecord "Odd wallet"
  runWallet config
