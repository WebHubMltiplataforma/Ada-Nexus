{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module MarketValidator where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Ada                 as Ada
import           Ledger.Value
import           Plutus.Contract
import           Plutus.V1.Ledger.Scripts

-- Datum para el marketplace
data MarketDatum = MarketDatum
    { mdSeller :: PubKeyHash
    , mdPrice  :: Integer
    , mdRoyalty :: Integer  -- Porcentaje de regalías (0-100)
    , mdCreator :: PubKeyHash
    } deriving Show

PlutusTx.unstableMakeIsData ''MarketDatum

-- Redeemer para las acciones del marketplace
data MarketAction = Buy | Cancel
    deriving Show

PlutusTx.unstableMakeIsData ''MarketAction

{-# INLINABLE mkMarketValidator #-}
mkMarketValidator :: MarketDatum -> MarketAction -> ScriptContext -> Bool
mkMarketValidator datum action ctx =
    case action of
        Buy -> 
            let
                info = scriptContextTxInfo ctx
                sellerPaid = valuePaidTo info (mdSeller datum)
                creatorPaid = valuePaidTo info (mdCreator datum)
                totalPrice = mdPrice datum
                royaltyAmount = (totalPrice * mdRoyalty datum) `divide` 100
                sellerAmount = totalPrice - royaltyAmount
            in
                sellerPaid >= Ada.lovelaceValueOf sellerAmount &&
                creatorPaid >= Ada.lovelaceValueOf royaltyAmount
        
        Cancel ->
            txSignedBy (scriptContextTxInfo ctx) (mdSeller datum)

marketValidator :: Validator
marketValidator = mkValidatorScript $$(PlutusTx.compile [|| mkMarketValidator ||])