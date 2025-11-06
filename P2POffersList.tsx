import React, { useState, useEffect } from 'react';
import { P2PService } from '../../services/P2PService';
import { P2POffer } from '../../types/p2p';
import { P2PChat } from './P2PChat';

interface P2POffersListProps {
  mode: 'buy' | 'myOffers';
}

export const P2POffersList: React.FC<P2POffersListProps> = ({ mode }) => {
  const [offers, setOffers] = useState<P2POffer[]>([]);
  const [loading, setLoading] = useState(true);
  const [filters, setFilters] = useState({
    token: '',
    minAmount: '',
    maxAmount: '',
    paymentMethod: ''
  });
  const [selectedTrade, setSelectedTrade] = useState<string | null>(null);

  const p2pService = new P2PService();

  useEffect(() => {
    loadOffers();
  }, [filters]);

  const loadOffers = async () => {
    setLoading(true);
    try {
      const filterParams = {
        token: filters.token || undefined,
        minAmount: filters.minAmount ? parseFloat(filters.minAmount) : undefined,
        maxAmount: filters.maxAmount ? parseFloat(filters.maxAmount) : undefined,
        paymentMethod: filters.paymentMethod || undefined
      };

      const activeOffers = await p2pService.getActiveOffers(filterParams);
      
      if (mode === 'myOffers') {
        // Filtrar ofertas propias (en producción usarías la dirección real del usuario)
        const myAddress = 'addr1qyourselleraddresshere...';
        setOffers(activeOffers.filter(offer => offer.seller === myAddress));
      } else {
        setOffers(activeOffers);
      }
    } catch (error) {
      console.error('Error loading offers:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleStartTrade = async (offerId: string) => {
    try {
      // En producción, obtendrías la dirección real del comprador
      const buyerAddress = 'addr1qyourbuyeraddresshere...';
      const tradeId = await p2pService.initiateTrade(offerId, buyerAddress, 50); // $50 de ejemplo
      
      setSelectedTrade(tradeId);
      alert('Trade started! Check the chat to coordinate with the seller.');
    } catch (error) {
      console.error('Error starting trade:', error);
      alert('Error starting trade. Please try again.');
    }
  };

  const formatAddress = (address: string) => {
    return `${address.slice(0, 8)}...${address.slice(-8)}`;
  };

  if (loading) {
    return <div className="loading">Loading offers...</div>;
  }

  return (
    <div className="offers-list">
      {selectedTrade ? (
        <P2PChat 
          tradeId={selectedTrade} 
          onBack={() => setSelectedTrade(null)}
        />
      ) : (
        <>
          <div className="filters">
            <input
              type="text"
              placeholder="Filter by token..."
              value={filters.token}
              onChange={(e) => setFilters(prev => ({ ...prev, token: e.target.value }))}
            />
            <input
              type="number"
              placeholder="Min amount"
              value={filters.minAmount}
              onChange={(e) => setFilters(prev => ({ ...prev, minAmount: e.target.value }))}
            />
            <input
              type="number"
              placeholder="Max amount"
              value={filters.maxAmount}
              onChange={(e) => setFilters(prev => ({ ...prev, maxAmount: e.target.value }))}
            />
            <input
              type="text"
              placeholder="Payment method"
              value={filters.paymentMethod}
              onChange={(e) => setFilters(prev => ({ ...prev, paymentMethod: e.target.value }))}
            />
          </div>

          {offers.length === 0 ? (
            <div className="no-offers">
              <h3>No offers found</h3>
              <p>Try adjusting your filters or create a new offer.</p>
            </div>
          ) : (
            <div className="offers-grid">
              {offers.map(offer => (
                <div key={offer.id} className="offer-card">
                  <div className="offer-header">
                    <div className="seller-info">
                      <div className="seller-avatar">
                        {offer.seller.slice(2, 4).toUpperCase()}
                      </div>
                      <div className="seller-details">
                        <div className="seller-address">
                          {formatAddress(offer.seller)}
                        </div>
                        <div className="seller-reputation">
                          ⭐ {offer.reputation} • {offer.completedTrades} trades
                        </div>
                      </div>
                    </div>
                    <div className="offer-price">
                      ${offer.pricePerToken.toFixed(3)}
                    </div>
                  </div>

                  <div className="offer-details">
                    <div className="detail-row">
                      <span>Token:</span>
                      <strong>{offer.token}</strong>
                    </div>
                    <div className="detail-row">
                      <span>Available:</span>
                      <strong>{offer.amount.toLocaleString()} {offer.token}</strong>
                    </div>
                    <div className="detail-row">
                      <span>Limits:</span>
                      <span>${offer.minTransaction} - ${offer.maxTransaction}</span>
                    </div>
                    <div className="detail-row">
                      <span>Payment Methods:</span>
                      <div className="payment-tags">
                        {offer.paymentMethods.map(method => (
                          <span key={method} className="payment-tag">{method}</span>
                        ))}
                      </div>
                    </div>
                  </div>

                  <div className="offer-terms">
                    <p>{offer.terms}</p>
                  </div>

                  <div className="offer-actions">
                    {mode === 'buy' && (
                      <button 
                        className="buy-btn"
                        onClick={() => handleStartTrade(offer.id)}
                      >
                        Buy {offer.token}
                      </button>
                    )}
                    {mode === 'myOffers' && (
                      <button className="cancel-btn">
                        Cancel Offer
                      </button>
                    )}
                  </div>

                  <div className="offer-time">
                    Posted {offer.createdAt.toLocaleDateString()}
                  </div>
                </div>
              ))}
            </div>
          )}
        </>
      )}
    </div>
  );
};