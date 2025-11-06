#!/bin/bash

echo "🔨 Compilando contratos de ADA Nexus..."

# Configuración
PROJECT_DIR="plutus-contracts"
BUILD_DIR="dist"
NETWORK="${1:-preprod}"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Función para imprimir mensajes
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Verificar que estamos en el directorio correcto
if [ ! -d "$PROJECT_DIR" ]; then
    print_error "Directorio $PROJECT_DIR no encontrado"
    exit 1
fi

cd $PROJECT_DIR

# Limpiar builds anteriores
print_status "Limpiando builds anteriores..."
rm -rf $BUILD_DIR
cabal clean

# Configurar para la red correcta
print_status "Configurando para red: $NETWORK"

# Compilar contratos
print_status "Compilando contratos Plutus..."

# Compilar marketplace
print_status "Compilando Marketplace Validator..."
cabal build marketplace-contract

if [ $? -eq 0 ]; then
    print_status "Marketplace compilado exitosamente"
else
    print_error "Falló la compilación del Marketplace"
    exit 1
fi

# Compilar DEX
print_status "Compilando DEX Pool Validator..."
cabal build dex-contract

if [ $? -eq 0 ]; then
    print_status "DEX compilado exitosamente"
else
    print_error "Falló la compilación del DEX"
    exit 1
fi

# Compilar contratos de pagos
print_status "Compilando Payment Contracts..."
cabal build payment-contract

if [ $? -eq 0 ]; then
    print_status "Payment contracts compilados exitosamente"
else
    print_error "Falló la compilación de payment contracts"
    exit 1
fi

# Ejecutar tests
print_status "Ejecutando tests..."
cabal test

if [ $? -eq 0 ]; then
    print_status "Todos los tests pasaron"
else
    print_error "Algunos tests fallaron"
    exit 1
fi

# Crear directorio de distribución
mkdir -p ../$BUILD_DIR/contracts

# Copiar contratos compilados
print_status "Copiando contratos compilados..."
find . -name "*.plutus" -exec cp {} ../$BUILD_DIR/contracts/ \;

print_status "✅ Compilación completada exitosamente!"
print_status "Contratos disponibles en: $BUILD_DIR/contracts/"