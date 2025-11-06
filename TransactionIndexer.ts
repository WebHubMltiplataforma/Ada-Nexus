import { BlockFrostAPI } from '@blockfrost/blockfrost-js';

export class TransactionIndexer {
    private api: BlockFrostAPI;
    private lastProcessedSlot: number = 0;

    constructor(projectId: string) {
        this.api = new BlockFrostAPI({ projectId });
    }

    async startIndexing(): Promise<void> {
        while (true) {
            try {
                const latestBlock = await this.api.blocksLatest();
                await this.processNewBlocks(latestBlock.slot);
                await this.delay(5000); // Esperar 5 segundos
            } catch (error) {
                console.error('Error in indexer:', error);
                await this.delay(10000);
            }
        }
    }

    private async processNewBlocks(currentSlot: number): Promise<void> {
        if (currentSlot <= this.lastProcessedSlot) return;

        for (let slot = this.lastProcessedSlot + 1; slot <= currentSlot; slot++) {
            const block = await this.api.blocks(slot);
            const transactions = await this.api.blocksTxs(slot);
            
            for (const txHash of transactions) {
                await this.processTransaction(txHash);
            }
        }

        this.lastProcessedSlot = currentSlot;
    }

    private async processTransaction(txHash: string): Promise<void> {
        const tx = await this.api.txs(txHash);
        const utxos = await this.api.txsUtxos(txHash);
        
        // Procesar metadata para NFTs
        if (tx.metadata) {
            await this.processMetadata(tx.metadata, txHash);
        }

        // Indexar en base de datos
        await this.saveTransactionToDB(tx, utxos);
    }

    private async delay(ms: number): Promise<void> {
        return new Promise(resolve => setTimeout(resolve, ms));
    }
}