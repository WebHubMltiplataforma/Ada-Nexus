import React, { useState, useEffect } from 'react';
import { P2PService, P2POrder } from '../../services/P2PService';

interface OrderListProps {
  onOrderUpdate: () => void;
}

export const OrderList: React.FC<OrderListProps> = ({ onOrderUpdate }) => {
  const [orders, setOrders] = useState<P2POrder[]>([]);
  const [loading, setLoading] = useState(true);

  const p2pService = new P2PService();

  const loadOrders = async () => {
    setLoading(true);
    try {
      const orders = await p2pService.getOrders();
      setOrders(orders);
    } catch (error) {
      console.error('Error loading orders:', error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadOrders();
  }, []);

  const handleExecuteOrder = async (orderId: string) => {
    try {
      await p2pService.executeOrder(orderId, 'buyer_address'); // En realidad, obtener de la wallet
      onOrderUpdate();
      loadOrders();
    } catch (error) {
      console.error('Error executing order:', error);
    }
  };

  const handleCancelOrder = async (orderId: string) => {
    try {
      await p2pService.cancelOrder(orderId);
      onOrderUpdate();
      loadOrders();
    } catch (error) {
      console.error('Error cancelling order:', error);
    }
  };

  if (loading) {
    return <div>Loading orders...</div>;
  }

  return (
    <div>
      <h3>P2P Orders</h3>
      {orders.length === 0 ? (
        <p>No orders available.</p>
      ) : (
        <div style={{ display: 'grid', gap: '1rem' }}>
          {orders.map(order => (
            <div key={order.id} style={{ 
              background: 'var(--background-dark)', 
              padding: '1rem', 
              borderRadius: '8px',
              border: '1px solid rgba(255,255,255,0.1)'
            }}>
              <div style={{ display: 'flex', justifyContent: 'space-between' }}>
                <div>
                  <div><strong>{order.type.toUpperCase()}</strong> {order.amount} {order.token}</div>
                  <div>Price: {order.price} ADA</div>
                  <div>Total: {order.total} ADA</div>
                  <div>Seller: {order.seller.slice(0, 8)}...</div>
                </div>
                <div>
                  {order.type === 'sell' && (
                    <button 
                      onClick={() => handleExecuteOrder(order.id)}
                      style={{
                        padding: '0.5rem 1rem',
                        background: 'var(--success-color)',
                        color: 'white',
                        border: 'none',
                        borderRadius: '6px',
                        cursor: 'pointer',
                        marginRight: '0.5rem'
                      }}
                    >
                      Buy
                    </button>
                  )}
                  {order.seller === 'current_user_address' && ( // En realidad, comparar con la dirección del usuario
                    <button 
                      onClick={() => handleCancelOrder(order.id)}
                      style={{
                        padding: '0.5rem 1rem',
                        background: 'var(--error-color)',
                        color: 'white',
                        border: 'none',
                        borderRadius: '6px',
                        cursor: 'pointer'
                      }}
                    >
                      Cancel
                    </button>
                  )}
                </div>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
};