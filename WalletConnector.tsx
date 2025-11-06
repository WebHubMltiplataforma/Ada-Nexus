import React, { useState } from 'react'
import { CardanoService } from '../../services/CardanoService'

interface WalletConnectorProps {
  onWalletConnected: (address: string, balance: any) => void;
}

export const WalletConnector: React.FC<WalletConnectorProps> = ({ onWalletConnected }) => {
  const [cardanoService] = useState(new CardanoService())
  const [isConnecting, setIsConnecting] = useState(false)

  const connectWallet = async (walletName: string) => {
    try {
      setIsConnecting(true)
      
      // Simulación de conexión - en producción usarías el servicio real
      await new Promise(resolve => setTimeout(resolve, 1000))
      
      const mockAddress = 'addr1qxhyajcrgsrhk335sr9uvrsmuwxnknk9f547f53e9nxfc92dtwpt3uge70wdftndq4wtx5ja86jqeznhl6kgpdnqlkyqcxtvaf'
      const mockBalance = {
        ada: '125.75',
        assets: [
          { unit: 'asset1', quantity: '10' },
          { unit: 'asset2', quantity: '5' }
        ]
      }
      
      onWalletConnected(mockAddress, mockBalance)
    } catch (error) {
      console.error('Failed to connect wallet:', error)
      alert('Failed to connect wallet. Please try again.')
    } finally {
      setIsConnecting(false)
    }
  }

  return (
    <div className="wallet-connector">
      {isConnecting ? (
        <div style={{ 
          display: 'flex', 
          flexDirection: 'column', 
          alignItems: 'center',
          gap: '1rem'
        }}>
          <div className="loading-spinner"></div>
          <p>Connecting wallet...</p>
        </div>
      ) : (
        <div className="wallet-options">
          <h3 style={{ marginBottom: '2rem', color: 'var(--text-primary)' }}>
            Connect Your Wallet
          </h3>
          <button 
            onClick={() => connectWallet('nami')}
            className="wallet-button"
            style={{
              width: '100%',
              padding: '1rem',
              margin: '0.5rem 0',
              border: 'none',
              borderRadius: '8px',
              background: 'var(--primary-color)',
              color: 'white',
              fontSize: '1rem',
              cursor: 'pointer',
              transition: 'background 0.3s ease'
            }}
          >
            Connect Nami Wallet
          </button>
          <button 
            onClick={() => connectWallet('eternl')}
            className="wallet-button"
            style={{
              width: '100%',
              padding: '1rem',
              margin: '0.5rem 0',
              border: 'none',
              borderRadius: '8px',
              background: 'var(--primary-color)',
              color: 'white',
              fontSize: '1rem',
              cursor: 'pointer',
              transition: 'background 0.3s ease'
            }}
          >
            Connect Eternl Wallet
          </button>
          <button 
            onClick={() => connectWallet('flint')}
            className="wallet-button"
            style={{
              width: '100%',
              padding: '1rem',
              margin: '0.5rem 0',
              border: 'none',
              borderRadius: '8px',
              background: 'var(--primary-color)',
              color: 'white',
              fontSize: '1rem',
              cursor: 'pointer',
              transition: 'background 0.3s ease'
            }}
          >
            Connect Flint Wallet
          </button>
        </div>
      )}
    </div>
  )
}