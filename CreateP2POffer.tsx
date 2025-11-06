import React, { useState } from 'react';
import { P2PService } from '../../services/P2PService';

interface CreateP2POfferProps {
  onOfferCreated?: () => void;
}

export const CreateP2POffer: React.FC<CreateP2POfferProps> = ({ onOfferCreated }) => {
  const [formData, setFormData] = useState({
    token: 'NEXUS',
    amount: '',
    pricePerToken: '',
    paymentMethods: [] as string[],
    minTransaction: '',
    maxTransaction: '',
    terms: ''
  });

  const [isSubmitting, setIsSubmitting] = useState(false);
  const p2pService = new P2PService();

  const paymentOptions = [
    'Bank Transfer',
    'PayPal',
    'Wise',
    'Cash App',
    'Zelle',
    'Venmo',
    'Revolut',
    'Skrill',
    'Payoneer',
    'Western Union'
  ];

  const handlePaymentMethodToggle = (method: string) => {
    setFormData(prev => ({
      ...prev,
      paymentMethods: prev.paymentMethods.includes(method)
        ? prev.paymentMethods.filter(m => m !== method)
        : [...prev.paymentMethods, method]
    }));
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setIsSubmitting(true);

    try {
      // En producción, obtendrías la dirección real del wallet
      const sellerAddress = 'addr1qyourselleraddresshere...';

      await p2pService.createOffer({
        seller: sellerAddress,
        token: formData.token,
        amount: parseFloat(formData.amount),
        pricePerToken: parseFloat(formData.pricePerToken),
        totalPrice: parseFloat(formData.amount) * parseFloat(formData.pricePerToken),
        paymentMethods: formData.paymentMethods,
        minTransaction: parseFloat(formData.minTransaction),
        maxTransaction: parseFloat(formData.maxTransaction),
        terms: formData.terms
      });

      alert('Offer created successfully!');
      setFormData({
        token: 'NEXUS',
        amount: '',
        pricePerToken: '',
        paymentMethods: [],
        minTransaction: '',
        maxTransaction: '',
        terms: ''
      });

      onOfferCreated?.();
    } catch (error) {
      console.error('Error creating offer:', error);
      alert('Error creating offer. Please try again.');
    } finally {
      setIsSubmitting(false);
    }
  };

  const calculateTotal = () => {
    if (formData.amount && formData.pricePerToken) {
      return (parseFloat(formData.amount) * parseFloat(formData.pricePerToken)).toFixed(2);
    }
    return '0';
  };

  return (
    <div className="create-offer">
      <form onSubmit={handleSubmit} className="offer-form">
        <div className="form-grid">
          <div className="form-group">
            <label htmlFor="token">Token to Sell</label>
            <select
              id="token"
              value={formData.token}
              onChange={(e) => setFormData(prev => ({ ...prev, token: e.target.value }))}
              required
            >
              <option value="NEXUS">NEXUS</option>
              <option value="ADA">ADA</option>
              <option value="BTC">BTC</option>
              <option value="ETH">ETH</option>
            </select>
          </div>

          <div className="form-group">
            <label htmlFor="amount">Total Amount</label>
            <input
              type="number"
              id="amount"
              value={formData.amount}
              onChange={(e) => setFormData(prev => ({ ...prev, amount: e.target.value }))}
              placeholder="e.g., 1000"
              step="0.000001"
              required
            />
          </div>

          <div className="form-group">
            <label htmlFor="pricePerToken">Price per Token (USD)</label>
            <input
              type="number"
              id="pricePerToken"
              value={formData.pricePerToken}
              onChange={(e) => setFormData(prev => ({ ...prev, pricePerToken: e.target.value }))}
              placeholder="e.g., 0.15"
              step="0.000001"
              required
            />
          </div>

          <div className="form-group total-display">
            <label>Total Value (USD)</label>
            <div className="total-amount">${calculateTotal()}</div>
          </div>
        </div>

        <div className="form-group">
          <label>Transaction Limits (USD)</label>
          <div className="range-inputs">
            <input
              type="number"
              placeholder="Min amount"
              value={formData.minTransaction}
              onChange={(e) => setFormData(prev => ({ ...prev, minTransaction: e.target.value }))}
              required
            />
            <span>to</span>
            <input
              type="number"
              placeholder="Max amount"
              value={formData.maxTransaction}
              onChange={(e) => setFormData(prev => ({ ...prev, maxTransaction: e.target.value }))}
              required
            />
          </div>
        </div>

        <div className="form-group">
          <label>Accepted Payment Methods</label>
          <div className="payment-methods">
            {paymentOptions.map(method => (
              <div key={method} className="payment-method">
                <input
                  type="checkbox"
                  id={method}
                  checked={formData.paymentMethods.includes(method)}
                  onChange={() => handlePaymentMethodToggle(method)}
                />
                <label htmlFor={method}>{method}</label>
              </div>
            ))}
          </div>
        </div>

        <div className="form-group">
          <label htmlFor="terms">Trade Terms & Conditions</label>
          <textarea
            id="terms"
            value={formData.terms}
            onChange={(e) => setFormData(prev => ({ ...prev, terms: e.target.value }))}
            placeholder="Specify your trade terms, payment deadlines, verification requirements, etc."
            rows={4}
          />
        </div>

        <button 
          type="submit" 
          className="submit-btn"
          disabled={isSubmitting || formData.paymentMethods.length === 0}
        >
          {isSubmitting ? 'Creating Offer...' : '🚀 Create P2P Offer'}
        </button>
      </form>
    </div>
  );
};