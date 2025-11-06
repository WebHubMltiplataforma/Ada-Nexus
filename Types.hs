{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

module AdaNexus.Dex.Types where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger

data PoolDatum = PoolDatum
    { poolLiquidityA  :: Integer
    , poolLiquidityB  :: Integer
    , poolTotalSupply :: Integer
    , poolFee         :: Integer
    , poolProvider    :: PubKeyHash
    } deriving Show

PlutusTx.unstableMakeIsData ''PoolDatum

data PoolAction
    = AddLiquidity Integer Integer
    | RemoveLiquidity Integer
    | SwapAforB Integer
    | SwapBforA Integer
    deriving Show

PlutusTx.unstableMakeIsData ''PoolAction