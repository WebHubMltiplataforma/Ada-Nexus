import React, { useState, useEffect } from 'react';
import { P2PService } from '../../services/P2PService';
import { P2PTrade, ChatMessage } from '../../types/p2p';

interface P2PChatProps {
  tradeId: string;
  onBack: () => void;
}

export const P2PChat: React.FC<P2PChatProps> = ({ tradeId, onBack }) => {
  const [trade, setTrade] = useState<P2PTrade | null>(null);
  const [message, setMessage] = useState('');
  const [loading, setLoading] = useState(true);

  const p2pService = new P2PService();

  useEffect(() => {
    loadTrade();
  }, [tradeId]);

  const loadTrade = async () => {
    // En una implementación real, obtendrías el trade del servicio
    // Por ahora simulamos
    setLoading(false);
  };

  const sendMessage = async () => {
    if (!message.trim()) return;

    try {
      // En producción, usarías la dirección real del usuario
      const sender = 'addr1qyouruseraddresshere...';
      await p2pService.sendMessage(tradeId, sender, message);
      setMessage('');
      // Recargar mensajes
    } catch (error) {
      console.error('Error sending message:', error);
    }
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      sendMessage();
    }
  };

  const markAsPaid = async () => {
    try {
      await p2pService.markAsPaid(tradeId);
      alert('Marked as paid! Waiting for seller confirmation.');
    } catch (error) {
      console.error('Error marking as paid:', error);
    }
  };

  const completeTrade = async () => {
    try {
      await p2pService.completeTrade(tradeId);
      alert('Trade completed successfully!');
      onBack();
    } catch (error) {
      console.error('Error completing trade:', error);
    }
  };

  if (loading) {
    return <div className="loading">Loading trade...</div>;
  }

  return (
    <div className="p2p-chat">
      <div className="chat-header">
        <button onClick={onBack} className="back-btn">← Back</button>
        <h3>Trade #{tradeId}</h3>
        <div className="trade-status">Status: Pending</div>
      </div>

      <div className="trade-info">
        <div className="info-card">
          <div className="info-item">
            <span>Amount:</span>
            <strong>1,000 NEXUS</strong>
          </div>
          <div className="info-item">
            <span>Price:</span>
            <strong>$150.00</strong>
          </div>
          <div className="info-item">
            <span>Payment Method:</span>
            <strong>Bank Transfer</strong>
          </div>
        </div>
      </div>

      <div className="chat-messages">
        <div className="system-message">
          ⚡ Trade started. Please coordinate payment details with the seller.
        </div>
        
        <div className="message received">
          <div className="message-sender">Seller</div>
          <div className="message-content">
            Hello! Please send payment to my bank account. I'll release the tokens once confirmed.
          </div>
          <div className="message-time">2:30 PM</div>
        </div>

        <div className="message sent">
          <div className="message-sender">You</div>
          <div className="message-content">
            Great! I'll send the payment now. Please confirm once you receive it.
          </div>
          <div className="message-time">2:32 PM</div>
        </div>
      </div>

      <div className="chat-actions">
        <button onClick={markAsPaid} className="action-btn paid-btn">
          ✅ I've Sent Payment
        </button>
        <button onClick={completeTrade} className="action-btn complete-btn">
          🎉 Complete Trade
        </button>
      </div>

      <div className="chat-input">
        <textarea
          value={message}
          onChange={(e) => setMessage(e.target.value)}
          onKeyPress={handleKeyPress}
          placeholder="Type your message..."
          rows={2}
        />
        <button onClick={sendMessage} className="send-btn">
          Send
        </button>
      </div>
    </div>
  );
};