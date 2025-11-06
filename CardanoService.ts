import { Lucid, Blockfrost } from 'lucid-cardano';

export class CardanoService {
  private lucid: Lucid | null = null;
  private wallet: any = null;

  async initialize(network: 'mainnet' | 'preprod' | 'testnet'): Promise<void> {
    const apiKey = this.getBlockfrostApiKey(network);
    const baseUrl = this.getBlockfrostUrl(network);
    
    this.lucid = await Lucid.new(
      new Blockfrost(baseUrl, apiKey),
      network === 'mainnet' ? 'Mainnet' : 'Preview'
    );
  }

  async connectWallet(walletName: string): Promise<boolean> {
    try {
      if (!this.lucid) throw new Error('Lucid not initialized');
      
      // Para desarrollo - simulación
      if (import.meta.env.DEV) {
        console.log('Simulating wallet connection to:', walletName);
        await new Promise(resolve => setTimeout(resolve, 1000));
        return true;
      }
      
      // En producción
      const api = await (window as any).cardano[walletName].enable();
      this.lucid.selectWallet(api);
      this.wallet = api;
      
      return true;
    } catch (error) {
      console.error('Wallet connection failed:', error);
      return false;
    }
  }

  async getWalletBalance(): Promise<{ ada: string; assets: any[] }> {
    // Simulación para desarrollo
    if (import.meta.env.DEV) {
      return {
        ada: '125.75',
        assets: [
          { unit: 'NEXUS', quantity: '1000000' },
          { unit: 'asset1', quantity: '10' }
        ]
      };
    }

    if (!this.lucid || !this.wallet) {
      throw new Error('Wallet not connected');
    }

    const utxos = await this.lucid.wallet.getUtxos();
    const balance = this.calculateBalance(utxos);
    
    return balance;
  }

  async sendToken(
    recipient: string,
    policyId: string,
    assetName: string,
    amount: number
  ): Promise<string> {
    if (!this.lucid) throw new Error('Lucid not initialized');

    const tx = await this.lucid
      .newTx()
      .payToAddress(recipient, { 
        [policyId + assetName]: BigInt(amount) 
      })
      .complete();

    const signedTx = await tx.sign().complete();
    return await signedTx.submit();
  }

  private calculateBalance(utxos: any[]): { ada: string; assets: any[] } {
    let totalLovelace = 0n;
    const assets: { [key: string]: bigint } = {};

    utxos.forEach(utxo => {
      Object.entries(utxo.assets).forEach(([unit, quantity]) => {
        if (unit === 'lovelace') {
          totalLovelace += quantity as bigint;
        } else {
          assets[unit] = (assets[unit] || 0n) + (quantity as bigint);
        }
      });
    });

    return {
      ada: (Number(totalLovelace) / 1000000).toString(),
      assets: Object.entries(assets).map(([unit, quantity]) => ({
        unit,
        quantity: quantity.toString()
      }))
    };
  }

  private getBlockfrostApiKey(network: string): string {
    const keys = {
      mainnet: import.meta.env.VITE_BLOCKFROST_MAINNET_API_KEY,
      preprod: import.meta.env.VITE_BLOCKFROST_PREPROD_API_KEY,
      testnet: import.meta.env.VITE_BLOCKFROST_TESTNET_API_KEY
    };
    return keys[network as keyof typeof keys] || 'testnet-key';
  }

  private getBlockfrostUrl(network: string): string {
    const baseUrls = {
      mainnet: 'https://cardano-mainnet.blockfrost.io/api/v0',
      preprod: 'https://cardano-preprod.blockfrost.io/api/v0',
      testnet: 'https://cardano-testnet.blockfrost.io/api/v0'
    };
    return baseUrls[network as keyof typeof baseUrls] || baseUrls.preprod;
  }
}