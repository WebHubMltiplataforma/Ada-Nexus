export interface P2POffer {
  id: string;
  seller: string;
  token: string;
  amount: number;
  pricePerToken: number;
  totalPrice: number;
  paymentMethods: string[];
  minTransaction: number;
  maxTransaction: number;
  terms: string;
  status: 'active' | 'completed' | 'cancelled';
  createdAt: Date;
  reputation: number;
  completedTrades: number;
}

export interface P2PTrade {
  id: string;
  offerId: string;
  buyer: string;
  seller: string;
  tokenAmount: number;
  fiatAmount: number;
  status: 'pending' | 'paid' | 'completed' | 'disputed';
  createdAt: Date;
  escrowAddress?: string;
  chatMessages: ChatMessage[];
}

export interface ChatMessage {
  id: string;
  tradeId: string;
  sender: string;
  message: string;
  timestamp: Date;
  type: 'text' | 'system';
}

export interface UserReputation {
  address: string;
  totalTrades: number;
  successfulTrades: number;
  rating: number;
  joinedDate: Date;
}