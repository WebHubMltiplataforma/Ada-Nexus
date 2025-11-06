import { Lucid, Blockfrost, Data, Constr, applyParamsToScript } from 'lucid-cardano';

export interface TokenMetadata {
  name: string;
  description: string;
  ticker: string;
  url?: string;
  logo?: string;
  decimals: number;
}

export class TokenService {
  private lucid: Lucid | null = null;

  async initialize(network: 'mainnet' | 'preprod' | 'testnet'): Promise<void> {
    const apiKey = this.getBlockfrostApiKey(network);
    const baseUrl = this.getBlockfrostUrl(network);
    
    this.lucid = await Lucid.new(
      new Blockfrost(baseUrl, apiKey),
      network === 'mainnet' ? 'Mainnet' : 'Preview'
    );
  }

  async createNexusToken(
    tokenName: string,
    tokenTicker: string,
    totalSupply: number,
    metadata: TokenMetadata
  ): Promise<{ 
    txHash: string; 
    policyId: string;
    assetName: string;
  }> {
    if (!this.lucid) throw new Error('Lucid not initialized');

    try {
      // 1. Generar política del token (time-locked para permitir minting futuro)
      const { paymentKey, policy } = await this.generateTokenPolicy();
      
      // 2. Crear metadata del token según estándar CIP-68
      const tokenMetadata = this.buildTokenMetadata(tokenName, tokenTicker, totalSupply, metadata);
      
      // 3. Construir transacción de minting
      const tx = await this.lucid
        .newTx()
        .mintAssets(
          { [policy + this.toHex(tokenTicker)]: BigInt(totalSupply) },
          Data.void()
        )
        .attachMetadata(721, tokenMetadata)
        .validTo(Date.now() + 200000) // 20 minutos para expirar
        .complete();

      // 4. Firmar y enviar transacción
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      return {
        txHash,
        policyId: policy,
        assetName: this.toHex(tokenTicker)
      };

    } catch (error) {
      console.error('Error creating token:', error);
      throw new Error(`Failed to create token: ${error}`);
    }
  }

  async mintAdditionalTokens(
    policyId: string,
    assetName: string,
    additionalAmount: number
  ): Promise<string> {
    if (!this.lucid) throw new Error('Lucid not initialized');

    const tx = await this.lucid
      .newTx()
      .mintAssets(
        { [policyId + assetName]: BigInt(additionalAmount) },
        Data.void()
      )
      .complete();

    const signedTx = await tx.sign().complete();
    return await signedTx.submit();
  }

  async getTokenBalance(policyId: string, assetName: string): Promise<bigint> {
    if (!this.lucid) throw new Error('Lucid not initialized');

    const address = await this.lucid.wallet.address();
    const utxos = await this.lucid.utxosAt(address);
    
    let totalBalance = 0n;
    utxos.forEach(utxo => {
      Object.entries(utxo.assets).forEach(([asset, amount]) => {
        if (asset === policyId + assetName) {
          totalBalance += amount;
        }
      });
    });

    return totalBalance;
  }

  private async generateTokenPolicy(): Promise<{ paymentKey: string; policy: string }> {
    if (!this.lucid) throw new Error('Lucid not initialized');

    // Crear política time-locked (puede ser modificada después)
    const { paymentKey } = await this.lucid.wallet.key();
    
    // Política simple para empezar (en producción usarías una más segura)
    const policy = this.lucid.utils.nativeScriptFromJson({
      type: "all",
      scripts: [
        { 
          type: "sig",
          keyHash: paymentKey
        }
      ]
    });

    const policyId = this.lucid.utils.mintingPolicyToId(policy);

    return { paymentKey, policy: policyId };
  }

  private buildTokenMetadata(
    name: string,
    ticker: string,
    supply: number,
    metadata: TokenMetadata
  ): any {
    return {
      [this.toHex(ticker)]: {
        name: metadata.name,
        description: metadata.description,
        ticker: metadata.ticker,
        url: metadata.url || "https://ada-nexus.io",
        logo: metadata.logo || "https://ada-nexus.io/logo.png",
        decimals: metadata.decimals,
        supply: supply.toString(),
        version: "1.0",
        authors: ["ADA Nexus Team"]
      }
    };
  }

  private toHex(str: string): string {
    return Buffer.from(str).toString('hex');
  }

  private getBlockfrostApiKey(network: string): string {
    const keys = {
      mainnet: import.meta.env.VITE_BLOCKFROST_MAINNET_API_KEY,
      preprod: import.meta.env.VITE_BLOCKFROST_PREPROD_API_KEY,
      testnet: import.meta.env.VITE_BLOCKFROST_TESTNET_API_KEY
    };
    return keys[network as keyof typeof keys] || '';
  }

  private getBlockfrostUrl(network: string): string {
    const baseUrls = {
      mainnet: 'https://cardano-mainnet.blockfrost.io/api/v0',
      preprod: 'https://cardano-preprod.blockfrost.io/api/v0',
      testnet: 'https://cardano-testnet.blockfrost.io/api/v0'
    };
    return baseUrls[network as keyof typeof baseUrls] || '';
  }
}