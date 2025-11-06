#!/bin/bash

echo "🚀 Desplegando Marketplace de ADA Nexus..."

# Configuración
NETWORK="${1:-preprod}"
CONTRACT_FILE="dist/contracts/marketplace.plutus"
SENDER_ADDRESS="$ADDRESS_PREPROD"
SENDER_SKEY="$SKEY_PREPROD"

# Verificar variables de entorno
if [ -z "$SENDER_ADDRESS" ] || [ -z "$SENDER_SKEY" ]; then
    echo "Error: Variables de entorno no configuradas"
    exit 1
fi

# Obtener UTxO del sender
echo "Obteniendo UTxOs..."
UTXO=$(cardano-cli query utxo --address $SENDER_ADDRESS --testnet-magic 1 --out-file /dev/stdout | jq -r 'to_entries[] | select(.value.value.lovelace > 1000000) | .key' | head -n1)

if [ -z "$UTXO" ]; then
    echo "Error: No se encontró UTxO adecuado"
    exit 1
fi

# Compilar contrato si no existe
if [ ! -f "$CONTRACT_FILE" ]; then
    echo "Compilando contrato..."
    ./compile-contracts.sh $NETWORK
fi

# Crear transacción de despliegue
echo "Creando transacción de despliegue..."

cardano-cli transaction build \
    --testnet-magic 1 \
    --tx-in $UTXO \
    --tx-out $(cat $CONTRACT_FILE.addr)+2000000 \
    --tx-out-datum-embed-file "config-files/marketplace-datum.json" \
    --change-address $SENDER_ADDRESS \
    --out-file tx.marketplace.raw

# Firmar transacción
cardano-cli transaction sign \
    --tx-body-file tx.marketplace.raw \
    --signing-key-file $SENDER_SKEY \
    --testnet-magic 1 \
    --out-file tx.marketplace.signed

# Enviar transacción
cardano-cli transaction submit \
    --testnet-magic 1 \
    --tx-file tx.marketplace.signed

echo "✅ Marketplace desplegado exitosamente!"
echo "Contract Address: $(cat $CONTRACT_FILE.addr)"