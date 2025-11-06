{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Marketplace where

import           Plutus.Contract
import           PlutusTx         (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup(..), unless)
import           Ledger           (Address, Validator, ScriptContext, scriptAddress, Value, TokenName, PubKeyHash, Datum (..), DatumHash, TxOutTx, txOutAddress, txOutValue, txOutDatum, pubKeyHashAddress, TxInfo, txInfoSignatories, txInfoOutputs, txInfoInputs, txInInfoResolved, txInfoData, findDatum, ScriptPurpose (Spending), getContinuingOutputs, unspentOutputs, txId, TxOut, ValidatorHash, PaymentPubKeyHash, PaymentPubKey (PaymentPubKey), pubKeyHash, scriptAddress, Datum (..), OutputDatum (..))
import qualified Ledger.Ada       as Ada
import           Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Tx (ChainIndexTxOut(..))
import           Prelude (Semigroup (..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           Schema (ToSchema, ToArgument)

data MarketplaceDatum = MarketplaceDatum
    { seller :: PubKeyHash
    , price  :: Value
    , royaltyRecipient :: PubKeyHash
    , royaltyPercent :: Integer -- 1% = 1, 100% = 100
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

data MarketplaceAction = Buy | Cancel
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MarketplaceAction

{-# INLINABLE mkMarketplaceValidator #-}
mkMarketplaceValidator :: MarketplaceDatum -> MarketplaceAction -> ScriptContext -> Bool
mkMarketplaceValidator datum action ctx =
    case action of
        Buy -> 
            let
                info = scriptContextTxInfo ctx
                -- Check that the input (the NFT) is being spent
                ownInput = case findOwnInput ctx of
                    Just input -> input
                    Nothing    -> traceError "marketplace input missing"
                ownValue = txOutValue (txInInfoResolved ownInput)
                -- Check that the output to the seller is the price minus royalty
                sellerOutput = pubKeyHashAddress (seller datum) Nothing
                royaltyOutput = pubKeyHashAddress (royaltyRecipient datum) Nothing
                totalPrice = price datum
                royaltyAmount = (totalPrice * royaltyPercent datum) `divide` 100
                sellerAmount = totalPrice - royaltyAmount
                -- Check that the seller gets the sellerAmount and the royalty recipient gets royaltyAmount
                -- Also, the buyer gets the NFT (the ownValue) and the marketplace fee is paid (if any)
                -- For simplicity, we assume the buyer is the one who is spending the price and the NFT is sent to the buyer.
                -- We are not handling the marketplace fee here, only royalty.
            in
                -- Check that the seller gets sellerAmount and royaltyRecipient gets royaltyAmount
                valuePaidTo info (seller datum) >= sellerAmount &&
                valuePaidTo info (royaltyRecipient datum) >= royaltyAmount
        Cancel ->
            -- Only the seller can cancel
            txSignedBy (scriptContextTxInfo ctx) (seller datum)

marketplaceValidator :: Validator
marketplaceValidator = mkValidatorScript $$(PlutusTx.compile [|| mkMarketplaceValidator ||])

marketplaceAddress :: Address
marketplaceAddress = scriptAddress marketplaceValidator

-- Off-chain code

list :: AsContractError e => MarketplaceDatum -> Contract w s e ()
list datum = do
    let tx = Constraints.mustPayToTheScript datum (price datum)
    ledgerTx <- submitTxConstraints marketplaceValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Listed NFT for sale: " ++ show datum

buy :: AsContractError e => ValidatorHash -> DatumHash -> Contract w s e ()
buy validatorHash datumHash = do
    (oref, o, datum) <- findMarketplaceUTxO validatorHash datumHash
    let MarketplaceDatum{ seller = sellerPkh, price = p, royaltyRecipient = royaltyPkh, royaltyPercent = royaltyPct } = datum
    let royaltyAmount = (p * royaltyPct) `divide` 100
        sellerAmount = p - royaltyAmount
        action = Buy
        lookups = Constraints.validatorLookups marketplaceValidator <>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData action) <>
             Constraints.mustPayToPubKey (PaymentPubKeyHash sellerPkh) sellerAmount <>
             Constraints.mustPayToPubKey (PaymentPubKeyHash royaltyPkh) royaltyAmount
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Bought NFT: " ++ show datum

cancel :: AsContractError e => ValidatorHash -> DatumHash -> Contract w s e ()
cancel validatorHash datumHash = do
    (oref, o, datum) <- findMarketplaceUTxO validatorHash datumHash
    let action = Cancel
        lookups = Constraints.validatorLookups marketplaceValidator <>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData action)
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Cancelled listing: " ++ show datum

findMarketplaceUTxO :: AsContractError e => ValidatorHash -> DatumHash -> Contract w s e (TxOutRef, ChainIndexTxOut, MarketplaceDatum)
findMarketplaceUTxO validatorHash datumHash = do
    utxos <- utxosAt (scriptAddress marketplaceValidator)
    let xs = [ (oref, o, datum) 
             | (oref, o) <- Map.toList utxos
             , let datumHash' = case _ciTxOutDatum o of
                    OutputDatumHash h -> h
                    _                 -> error "expected datum hash"
             , datumHash' == datumHash
             , Just datum <- [PlutusTx.fromData $ getDatum o]
             ]
    case xs of
        [x] -> return x
        _   -> throwError "UTxO not found"

getDatum :: ChainIndexTxOut -> Data
getDatum o = case _ciTxOutDatum o of
    OutputDatum d -> d
    _             -> error "expected datum"
