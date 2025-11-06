{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module AdaNexus.Dex.PoolValidator where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Ada as Ada

import           AdaNexus.Dex.Types

{-# INLINABLE validateLiquidityPool #-}
validateLiquidityPool :: PoolDatum -> PoolAction -> ScriptContext -> Bool
validateLiquidityPool datum action ctx =
    case action of
        AddLiquidity amountA amountB ->
            let
                info = scriptContextTxInfo ctx
                totalLiquidity = poolLiquidityA datum + poolLiquidityB datum
                newLiquidityA = poolLiquidityA datum + amountA
                newLiquidityB = poolLiquidityB datum + amountB
                ratioMaintained = checkRatio (poolLiquidityA datum) (poolLiquidityB datum) amountA amountB
            in
                ratioMaintained && validateProviderPayment info datum amountA amountB
        
        RemoveLiquidity liquidityTokens ->
            let
                info = scriptContextTxInfo ctx
                totalSupply = poolTotalSupply datum
                share = liquidityTokens `divide` totalSupply
                amountA = share * poolLiquidityA datum
                amountB = share * poolLiquidityB datum
            in
                amountA > 0 && amountB > 0 &&
                validateWithdrawalPayment info datum amountA amountB

{-# INLINABLE checkRatio #-}
checkRatio :: Integer -> Integer -> Integer -> Integer -> Bool
checkRatio reserveA reserveB amountA amountB =
    if reserveA == 0 || reserveB == 0
        then True  -- Primer proveedor de liquidez
        else (amountA * reserveB) == (amountB * reserveA)

dexPoolValidator :: Validator
dexPoolValidator = mkValidatorScript $$(PlutusTx.compile [|| wrappedPoolValidator ||])
    where
        wrappedPoolValidator = mkUntypedValidator validateLiquidityPool