import React, { useState } from 'react';
import { P2PService } from '../../services/P2PService';

interface CreateOrderProps {
  onOrderCreated: () => void;
}

export const CreateOrder: React.FC<CreateOrderProps> = ({ onOrderCreated }) => {
  const [type, setType] = useState<'buy' | 'sell'>('sell');
  const [token, setToken] = useState('NEXUS');
  const [amount, setAmount] = useState('');
  const [price, setPrice] = useState('');
  const [loading, setLoading] = useState(false);

  const p2pService = new P2PService();

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    try {
      await p2pService.createOrder({
        type,
        token,
        amount: parseFloat(amount),
        price: parseFloat(price),
        total: parseFloat(amount) * parseFloat(price),
        seller: 'current_user_address', // En realidad, obtener de la wallet
      });
      onOrderCreated();
      setAmount('');
      setPrice('');
    } catch (error) {
      console.error('Error creating order:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div style={{ 
      background: 'var(--background-light)', 
      padding: '1.5rem', 
      borderRadius: '12px',
      marginBottom: '2rem'
    }}>
      <h3>Create P2P Order</h3>
      <form onSubmit={handleSubmit}>
        <div style={{ marginBottom: '1rem' }}>
          <label style={{ display: 'block', marginBottom: '0.5rem' }}>Type</label>
          <select 
            value={type} 
            onChange={(e) => setType(e.target.value as 'buy' | 'sell')}
            style={{ width: '100%', padding: '0.5rem', borderRadius: '6px' }}
          >
            <option value="sell">Sell</option>
            <option value="buy">Buy</option>
          </select>
        </div>
        <div style={{ marginBottom: '1rem' }}>
          <label style={{ display: 'block', marginBottom: '0.5rem' }}>Token</label>
          <select 
            value={token} 
            onChange={(e) => setToken(e.target.value)}
            style={{ width: '100%', padding: '0.5rem', borderRadius: '6px' }}
          >
            <option value="NEXUS">NEXUS</option>
            <option value="ADA">ADA</option>
          </select>
        </div>
        <div style={{ marginBottom: '1rem' }}>
          <label style={{ display: 'block', marginBottom: '0.5rem' }}>Amount</label>
          <input 
            type="number" 
            value={amount} 
            onChange={(e) => setAmount(e.target.value)}
            required
            style={{ width: '100%', padding: '0.5rem', borderRadius: '6px' }}
          />
        </div>
        <div style={{ marginBottom: '1rem' }}>
          <label style={{ display: 'block', marginBottom: '0.5rem' }}>Price (ADA per token)</label>
          <input 
            type="number" 
            value={price} 
            onChange={(e) => setPrice(e.target.value)}
            required
            step="0.000001"
            style={{ width: '100%', padding: '0.5rem', borderRadius: '6px' }}
          />
        </div>
        <button 
          type="submit" 
          disabled={loading}
          style={{
            width: '100%',
            padding: '0.75rem',
            background: 'var(--primary-color)',
            color: 'white',
            border: 'none',
            borderRadius: '6px',
            cursor: 'pointer'
          }}
        >
          {loading ? 'Creating...' : 'Create Order'}
        </button>
      </form>
    </div>
  );
};