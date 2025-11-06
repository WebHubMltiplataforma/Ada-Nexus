export const NETWORK_CONFIG = {
  mainnet: {
    name: 'mainnet',
    blockfrostUrl: 'https://cardano-mainnet.blockfrost.io/api/v0'
  },
  preprod: {
    name: 'preprod', 
    blockfrostUrl: 'https://cardano-preprod.blockfrost.io/api/v0'
  },
  testnet: {
    name: 'testnet',
    blockfrostUrl: 'https://cardano-testnet.blockfrost.io/api/v0'
  }
}

export const WALLETS = {
  NAMI: 'nami',
  ETERNL: 'eternl', 
  FLINT: 'flint',
  YOROI: 'yoroi'
} as const