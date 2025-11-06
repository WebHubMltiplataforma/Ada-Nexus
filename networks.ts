export const NETWORK_CONFIG = {
    mainnet: {
        networkId: 1,
        networkName: 'mainnet',
        blockfrostUrl: 'https://cardano-mainnet.blockfrost.io/api/v0',
        explorerUrl: 'https://cexplorer.io',
        marketplaceContract: 'addr1...',
        dexContract: 'addr1...'
    },
    preprod: {
        networkId: 0,
        networkName: 'preprod',
        blockfrostUrl: 'https://cardano-preprod.blockfrost.io/api/v0',
        explorerUrl: 'https://preprod.cexplorer.io',
        marketplaceContract: 'addr_test1...',
        dexContract: 'addr_test1...'
    },
    testnet: {
        networkId: 0,
        networkName: 'testnet',
        blockfrostUrl: 'https://cardano-testnet.blockfrost.io/api/v0',
        explorerUrl: 'https://testnet.cexplorer.io',
        marketplaceContract: 'addr_test1...',
        dexContract: 'addr_test1...'
    }
};

export const getNetworkConfig = (network: string) => {
    return NETWORK_CONFIG[network as keyof typeof NETWORK_CONFIG] || NETWORK_CONFIG.preprod;
};