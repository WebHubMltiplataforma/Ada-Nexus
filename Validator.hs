{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module AdaNexus.Marketplace.Validator where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Ada                 as Ada
import           Ledger.Value
import           Plutus.Contract
import           Plutus.V1.Ledger.Scripts

import           AdaNexus.Marketplace.Types

{-# INLINABLE validateMarketplaceAction #-}
validateMarketplaceAction :: MarketplaceDatum -> MarketplaceAction -> ScriptContext -> Bool
validateMarketplaceAction datum action ctx =
    case action of
        BuyNFT -> 
            let
                info = scriptContextTxInfo ctx
                sellerPayment = valuePaidTo info (marketSeller datum)
                creatorPayment = valuePaidTo info (marketCreator datum)
                totalPrice = marketPrice datum
                royaltyAmount = (totalPrice * marketRoyalty datum) `divide` 100
                sellerAmount = totalPrice - royaltyAmount
            in
                sellerPayment >= Ada.lovelaceValueOf sellerAmount &&
                creatorPayment >= Ada.lovelaceValueOf royaltyAmount &&
                checkNFTOwnershipTransfer info datum
        
        CancelListing ->
            txSignedBy info (marketSeller datum)

        UpdatePrice newPrice ->
            txSignedBy info (marketSeller datum) &&
            newPrice > 0

{-# INLINABLE checkNFTOwnershipTransfer #-}
checkNFTOwnershipTransfer :: TxInfo -> MarketplaceDatum -> Bool
checkNFTOwnershipTransfer info datum =
    any (\i -> txInInfoResolved i == marketNFT datum) (txInfoInputs info)

marketplaceValidator :: Validator
marketplaceValidator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
    where
        wrappedValidator = mkUntypedValidator validateMarketplaceAction