import { BlockFrostAPI } from '@blockfrost/blockfrost-js';

export class CardanoNodeService {
    private api: BlockFrostAPI;
    private network: string;

    constructor(projectId: string, network: string) {
        this.api = new BlockFrostAPI({ projectId });
        this.network = network;
    }

    async getTransactionInfo(txHash: string): Promise<any> {
        try {
            const tx = await this.api.txs(txHash);
            const utxos = await this.api.txsUtxos(txHash);
            
            return {
                hash: tx.hash,
                block: tx.block,
                block_height: tx.block_height,
                block_time: tx.block_time,
                slot: tx.slot,
                fees: tx.fees,
                inputs: utxos.inputs,
                outputs: utxos.outputs,
                metadata: tx.metadata
            };
        } catch (error) {
            throw new Error(`Failed to fetch transaction: ${error}`);
        }
    }

    async getAddressInfo(address: string): Promise<any> {
        try {
            const addressInfo = await this.api.addresses(address);
            const utxos = await this.api.addressesUtxos(address);
            const transactions = await this.api.addressesTransactions(address);
            
            return {
                address: addressInfo.address,
                balance: addressInfo.amount,
                stake_address: addressInfo.stake_address,
                utxos: utxos,
                transactions: transactions.map((tx: any) => ({
                    tx_hash: tx.tx_hash,
                    tx_index: tx.tx_index,
                    block_height: tx.block_height
                }))
            };
        } catch (error) {
            throw new Error(`Failed to fetch address info: ${error}`);
        }
    }

    async submitTransaction(cborTx: string): Promise<string> {
        try {
            const result = await this.api.txSubmit(cborTx);
            return result;
        } catch (error) {
            throw new Error(`Failed to submit transaction: ${error}`);
        }
    }

    async getLatestBlock(): Promise<any> {
        try {
            return await this.api.blocksLatest();
        } catch (error) {
            throw new Error(`Failed to fetch latest block: ${error}`);
        }
    }

    async getAssetInfo(asset: string): Promise<any> {
        try {
            return await this.api.assetsById(asset);
        } catch (error) {
            throw new Error(`Failed to fetch asset info: ${error}`);
        }
    }
}