import React, { useState } from 'react';
import { CreateP2POffer } from './CreateP2POffer';
import { P2POffersList } from './P2POffersList';
import './P2PStyles.css';

export const P2PTrading: React.FC = () => {
  const [activeTab, setActiveTab] = useState<'buy' | 'sell' | 'myOffers'>('buy');

  return (
    <div className="p2p-trading">
      <div className="p2p-header">
        <h1>P2P Trading</h1>
        <p>Trade crypto directly with other users securely</p>
      </div>

      <div className="p2p-navigation">
        <button 
          className={`p2p-nav-btn ${activeTab === 'buy' ? 'active' : ''}`}
          onClick={() => setActiveTab('buy')}
        >
          🔍 Buy Crypto
        </button>
        <button 
          className={`p2p-nav-btn ${activeTab === 'sell' ? 'active' : ''}`}
          onClick={() => setActiveTab('sell')}
        >
          💰 Sell Crypto
        </button>
        <button 
          className={`p2p-nav-btn ${activeTab === 'myOffers' ? 'active' : ''}`}
          onClick={() => setActiveTab('myOffers')}
        >
          📊 My Offers
        </button>
      </div>

      <div className="p2p-content">
        {activeTab === 'buy' && (
          <div className="p2p-section">
            <h2>Available Offers</h2>
            <P2POffersList mode="buy" />
          </div>
        )}

        {activeTab === 'sell' && (
          <div className="p2p-section">
            <h2>Create Sell Offer</h2>
            <CreateP2POffer />
          </div>
        )}

        {activeTab === 'myOffers' && (
          <div className="p2p-section">
            <h2>My Active Offers</h2>
            <P2POffersList mode="myOffers" />
          </div>
        )}
      </div>

      <div className="p2p-info">
        <h3>⚡ How P2P Trading Works</h3>
        <div className="info-steps">
          <div className="step">
            <div className="step-number">1</div>
            <div className="step-content">
              <strong>Find an Offer</strong>
              <p>Browse available buy/sell offers from verified traders</p>
            </div>
          </div>
          <div className="step">
            <div className="step-number">2</div>
            <div className="step-content">
              <strong>Start Trade</strong>
              <p>Initiate trade and chat with the counterparty</p>
            </div>
          </div>
          <div className="step">
            <div className="step-number">3</div>
            <div className="step-content">
              <strong>Secure Escrow</strong>
              <p>Tokens are held in secure escrow during the trade</p>
            </div>
          </div>
          <div className="step">
            <div className="step-number">4</div>
            <div className="step-content">
              <strong>Complete Trade</strong>
              <p>Release funds once payment is confirmed</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};