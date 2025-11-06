import { P2POffer, P2PTrade, ChatMessage, UserReputation } from '../types/p2p';

export class P2PService {
  private offers: P2POffer[] = [];
  private trades: P2PTrade[] = [];
  private reputations: Map<string, UserReputation> = new Map();

  constructor() {
    this.initializeSampleData();
  }

  // Crear nueva oferta P2P
  async createOffer(offerData: Omit<P2POffer, 'id' | 'createdAt' | 'status' | 'reputation' | 'completedTrades'>): Promise<string> {
    const newOffer: P2POffer = {
      ...offerData,
      id: this.generateId(),
      createdAt: new Date(),
      status: 'active',
      reputation: this.calculateReputation(offerData.seller),
      completedTrades: this.getCompletedTrades(offerData.seller)
    };

    this.offers.push(newOffer);
    return newOffer.id;
  }

  // Obtener ofertas activas
  async getActiveOffers(filters?: {
    token?: string;
    minAmount?: number;
    maxAmount?: number;
    paymentMethod?: string;
  }): Promise<P2POffer[]> {
    let filteredOffers = this.offers.filter(offer => offer.status === 'active');

    if (filters) {
      if (filters.token) {
        filteredOffers = filteredOffers.filter(offer => 
          offer.token.toLowerCase().includes(filters.token!.toLowerCase())
        );
      }
      if (filters.minAmount) {
        filteredOffers = filteredOffers.filter(offer => 
          offer.amount >= filters.minAmount!
        );
      }
      if (filters.maxAmount) {
        filteredOffers = filteredOffers.filter(offer => 
          offer.amount <= filters.maxAmount!
        );
      }
      if (filters.paymentMethod) {
        filteredOffers = filteredOffers.filter(offer =>
          offer.paymentMethods.some(method => 
            method.toLowerCase().includes(filters.paymentMethod!.toLowerCase())
          )
        );
      }
    }

    return filteredOffers;
  }

  // Iniciar un trade
  async initiateTrade(offerId: string, buyer: string, amount: number): Promise<string> {
    const offer = this.offers.find(o => o.id === offerId && o.status === 'active');
    if (!offer) {
      throw new Error('Offer not found or not active');
    }

    if (amount < offer.minTransaction || amount > offer.maxTransaction) {
      throw new Error('Amount outside allowed range');
    }

    const tokenAmount = amount / offer.pricePerToken;

    const newTrade: P2PTrade = {
      id: this.generateId(),
      offerId,
      buyer,
      seller: offer.seller,
      tokenAmount,
      fiatAmount: amount,
      status: 'pending',
      createdAt: new Date(),
      chatMessages: []
    };

    this.trades.push(newTrade);
    return newTrade.id;
  }

  // Marcar trade como pagado
  async markAsPaid(tradeId: string): Promise<void> {
    const trade = this.trades.find(t => t.id === tradeId);
    if (!trade) {
      throw new Error('Trade not found');
    }

    trade.status = 'paid';
    
    // En una implementación real, aquí se liberarían los tokens del escrow
    await this.simulateTokenTransfer(trade);
  }

  // Completar trade
  async completeTrade(tradeId: string): Promise<void> {
    const trade = this.trades.find(t => t.id === tradeId);
    if (!trade) {
      throw new Error('Trade not found');
    }

    if (trade.status !== 'paid') {
      throw new Error('Trade must be paid first');
    }

    trade.status = 'completed';
    await this.updateReputations(trade);
  }

  // Enviar mensaje de chat
  async sendMessage(tradeId: string, sender: string, message: string): Promise<void> {
    const trade = this.trades.find(t => t.id === tradeId);
    if (!trade) {
      throw new Error('Trade not found');
    }

    const newMessage: ChatMessage = {
      id: this.generateId(),
      tradeId,
      sender,
      message,
      timestamp: new Date(),
      type: 'text'
    };

    trade.chatMessages.push(newMessage);
  }

  // Obtener reputación de usuario
  async getUserReputation(address: string): Promise<UserReputation> {
    if (this.reputations.has(address)) {
      return this.reputations.get(address)!;
    }

    // Crear reputación por defecto
    const defaultReputation: UserReputation = {
      address,
      totalTrades: 0,
      successfulTrades: 0,
      rating: 5.0,
      joinedDate: new Date()
    };

    this.reputations.set(address, defaultReputation);
    return defaultReputation;
  }

  // Métodos privados
  private generateId(): string {
    return Math.random().toString(36).substr(2, 9);
  }

  private calculateReputation(address: string): number {
    const reputation = this.reputations.get(address);
    return reputation ? reputation.rating : 5.0;
  }

  private getCompletedTrades(address: string): number {
    const reputation = this.reputations.get(address);
    return reputation ? reputation.successfulTrades : 0;
  }

  private async simulateTokenTransfer(trade: P2PTrade): Promise<void> {
    // En implementación real, aquí se transferirían los tokens
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  private async updateReputations(trade: P2PTrade): Promise<void> {
    // Actualizar reputación del vendedor
    const sellerRep = await this.getUserReputation(trade.seller);
    sellerRep.totalTrades += 1;
    sellerRep.successfulTrades += 1;
    sellerRep.rating = Math.min(5.0, sellerRep.rating + 0.1);
    this.reputations.set(trade.seller, sellerRep);

    // Actualizar reputación del comprador
    const buyerRep = await this.getUserReputation(trade.buyer);
    buyerRep.totalTrades += 1;
    buyerRep.successfulTrades += 1;
    buyerRep.rating = Math.min(5.0, buyerRep.rating + 0.1);
    this.reputations.set(trade.buyer, buyerRep);

    // Actualizar oferta
    const offer = this.offers.find(o => o.id === trade.offerId);
    if (offer) {
      offer.completedTrades += 1;
    }
  }

  private initializeSampleData(): void {
    // Datos de ejemplo para demostración
    this.offers = [
      {
        id: '1',
        seller: 'addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp',
        token: 'NEXUS',
        amount: 5000,
        pricePerToken: 0.15,
        totalPrice: 750,
        paymentMethods: ['Bank Transfer', 'PayPal', 'Wise'],
        minTransaction: 10,
        maxTransaction: 500,
        terms: 'Payment within 15 minutes. ID verification required for first trade.',
        status: 'active',
        createdAt: new Date(Date.now() - 2 * 60 * 60 * 1000), // 2 horas atrás
        reputation: 4.8,
        completedTrades: 47
      },
      {
        id: '2',
        seller: 'addr1q8q566q7gdk3y0l72p05l2q2y9nz74j6q5h5g3n4m5k3j4h5g6f7d8s9a0s2d3f4g5h6j7k8l9p0o1i2u3y4t5r',
        token: 'ADA',
        amount: 10000,
        pricePerToken: 0.45,
        totalPrice: 4500,
        paymentMethods: ['Cash App', 'Zelle', 'Venmo'],
        minTransaction: 20,
        maxTransaction: 1000,
        terms: 'Fast transactions only. No chargebacks.',
        status: 'active',
        createdAt: new Date(Date.now() - 1 * 60 * 60 * 1000), // 1 hora atrás
        reputation: 4.9,
        completedTrades: 123
      }
    ];

    // Reputaciones de ejemplo
    this.reputations.set('addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp', {
      address: 'addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp',
      totalTrades: 52,
      successfulTrades: 47,
      rating: 4.8,
      joinedDate: new Date('2024-01-15')
    });

    this.reputations.set('addr1q8q566q7gdk3y0l72p05l2q2y9nz74j6q5h5g3n4m5k3j4h5g6f7d8s9a0s2d3f4g5h6j7k8l9p0o1i2u3y4t5r', {
      address: 'addr1q8q566q7gdk3y0l72p05l2q2y9nz74j6q5h5g3n4m5k3j4h5g6f7d8s9a0s2d3f4g5h6j7k8l9p0o1i2u3y4t5r',
      totalTrades: 135,
      successfulTrades: 123,
      rating: 4.9,
      joinedDate: new Date('2023-11-20')
    });
  }
}