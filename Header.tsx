import React from 'react'

interface HeaderProps {
  isConnected: boolean
  address: string
  balance: any
  onDisconnect: () => void
}

export const Header: React.FC<HeaderProps> = ({ 
  isConnected, 
  address, 
  balance, 
  onDisconnect 
}) => {
  const formatAddress = (addr: string) => {
    if (!addr) return ''
    return `${addr.slice(0, 8)}...${addr.slice(-8)}`
  }

  return (
    <header className="app-header">
      <div className="logo-section">
        <div className="logo-icon">⚡</div>
        <span className="logo-text">ADA Nexus</span>
      </div>
      
      {isConnected && (
        <div className="wallet-display">
          <div className="balance-display">
            <div className="ada-amount">{balance?.ada || '0'} ADA</div>
            <div className="assets-count">{balance?.assets?.length || 0} assets</div>
          </div>
          <div className="wallet-address">
            {formatAddress(address)}
          </div>
          <button 
            onClick={onDisconnect}
            className="disconnect-button"
          >
            Disconnect
          </button>
        </div>
      )}
    </header>
  )
}