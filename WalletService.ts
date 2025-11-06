import { CardanoWallet, Lucid, Blockfrost } from 'lucid-cardano';

export class NexusWalletService {
    private lucid: Lucid;
    private wallet: CardanoWallet | null = null;

    constructor(network: string) {
        const blockfrostApiKey = process.env.BLOCKFROST_API_KEY;
        this.lucid = await Lucid.new(
            new Blockfrost(`https://cardano-${network}.blockfrost.io/api/v0`, blockfrostApiKey),
            network
        );
    }

    async connectWallet(walletName: string): Promise<boolean> {
        try {
            const api = await window.cardano[walletName].enable();
            this.lucid.selectWallet(api);
            this.wallet = api;
            return true;
        } catch (error) {
            console.error('Error connecting wallet:', error);
            return false;
        }
    }

    async getBalance(): Promise<string> {
        if (!this.wallet) throw new Error('Wallet not connected');
        
        const utxos = await this.wallet.getUtxos();
        const balance = utxos.reduce((total, utxo) => {
            return total + parseInt(utxo.amount.find(asset => asset.unit === 'lovelace')?.quantity || '0');
        }, 0);
        
        return (balance / 1000000).toString(); // Convert to ADA
    }

    async createTransaction(recipient: string, amount: number, assets?: any[]): Promise<string> {
        const tx = await this.lucid
            .newTx()
            .payToAddress(recipient, { lovelace: BigInt(amount * 1000000) })
            .complete();
            
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        return txHash;
    }
}