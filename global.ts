export interface WalletInfo {
  isConnected: boolean
  address: string
  balance: {
    ada: string
    assets: Array<{
      unit: string
      quantity: string
    }>
  }
}

export interface NFT {
  id: string
  name: string
  image: string
  price: string
  creator: string
  royalty: number
}

export interface TokenPair {
  from: string
  to: string
  amount: string
}