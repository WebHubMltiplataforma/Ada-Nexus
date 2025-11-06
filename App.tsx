import React, { useState } from 'react'
import './App.css'

function App() {
  const [activeTab, setActiveTab] = useState('wallet')
  const [isConnected, setIsConnected] = useState(false)
  const [balance, setBalance] = useState({ ada: '0', assets: [] })

  const connectWallet = () => {
    // Simular conexión
    setTimeout(() => {
      setIsConnected(true)
      setBalance({
        ada: '125.75',
        assets: [
          { name: 'NEXUS', amount: '1,000,000' },
          { name: 'ADA', amount: '125.75' },
          { name: 'MIN', amount: '5,000' }
        ]
      })
    }, 1000)
  }

  const disconnectWallet = () => {
    setIsConnected(false)
    setBalance({ ada: '0', assets: [] })
  }

  return (
    <div className="app">
      {/* Header */}
      <header className="header">
        <div className="logo">
          <span>⚡</span>
          <h1>ADA Nexus</h1>
        </div>
        {isConnected && (
          <div className="wallet-info">
            <span>Balance: {balance.ada} ADA</span>
            <button onClick={disconnectWallet} className="disconnect-btn">
              Disconnect
            </button>
          </div>
        )}
      </header>

      {/* Main Content */}
      <main className="main">
        {!isConnected ? (
          <div className="welcome">
            <div className="welcome-content">
              <h2>🚀 Welcome to ADA Nexus</h2>
              <p>Your Complete Cardano dApp Platform - Ready to Use!</p>
              
              <div className="features">
                <div className="feature">
                  <div className="feature-icon">🦭</div>
                  <h3>Wallet Integration</h3>
                  <p>Connect with Nami, Eternl, Flint and other popular Cardano wallets with seamless integration</p>
                </div>
                <div className="feature">
                  <div className="feature-icon">🎨</div>
                  <h3>NFT Marketplace</h3>
                  <p>Create, buy and sell NFTs with guaranteed royalties and secure transactions</p>
                </div>
                <div className="feature">
                  <div className="feature-icon">💱</div>
                  <h3>DEX Trading</h3>
                  <p>Swap tokens instantly with low fees and high liquidity on our decentralized exchange</p>
                </div>
                <div className="feature">
                  <div className="feature-icon">🔁</div>
                  <h3>P2P Trading</h3>
                  <p>Trade directly with other users securely with our peer-to-peer trading platform</p>
                </div>
              </div>

              <button onClick={connectWallet} className="connect-btn">
                🔗 Connect Wallet (Demo Mode)
              </button>
              
              <p className="demo-note">
                🔥 <strong>Demo Mode Active</strong> - Full functionality with simulated data
              </p>
            </div>
          </div>
        ) : (
          <div className="dashboard">
            {/* Navigation */}
            <nav className="tabs">
              {['wallet', 'marketplace', 'dex', 'p2p', 'token'].map(tab => (
                <button
                  key={tab}
                  className={`tab ${activeTab === tab ? 'active' : ''}`}
                  onClick={() => setActiveTab(tab)}
                >
                  {tab === 'wallet' && '🦭 Wallet'}
                  {tab === 'marketplace' && '🎨 Marketplace'}
                  {tab === 'dex' && '💱 DEX'}
                  {tab === 'p2p' && '🔁 P2P'}
                  {tab === 'token' && '🪙 Token'}
                </button>
              ))}
            </nav>

            {/* Tab Content */}
            <div className="tab-content">
              {activeTab === 'wallet' && (
                <div className="wallet-tab">
                  <h3>💰 Wallet Dashboard</h3>
                  <div className="balance-card">
                    <div className="balance-main">
                      <span className="amount">{balance.ada}</span>
                      <span className="currency">ADA</span>
                    </div>
                    <p>Available Balance</p>
                  </div>

                  <div className="assets">
                    <h4>📊 Your Assets</h4>
                    {balance.assets.map((asset, index) => (
                      <div key={index} className="asset">
                        <span>{asset.name}</span>
                        <span>{asset.amount}</span>
                      </div>
                    ))}
                  </div>

                  <div className="actions">
                    <button className="action-btn">📥 Receive Funds</button>
                    <button className="action-btn">📤 Send Payment</button>
                    <button className="action-btn">🔄 Swap Tokens</button>
                    <button className="action-btn">📈 View Analytics</button>
                  </div>
                </div>
              )}

              {activeTab === 'marketplace' && (
                <div className="marketplace-tab">
                  <h3>🎨 NFT Marketplace</h3>
                  <p>Discover, collect, and trade extraordinary NFTs</p>
                  <div className="coming-soon">
                    <div className="nft-card">
                      <div className="nft-image">🖼️</div>
                      <div className="nft-info">
                        <h4>Cardano Warrior #1</h4>
                        <p>Price: 100 ADA</p>
                        <button>Buy Now</button>
                      </div>
                    </div>
                    <div className="nft-card">
                      <div className="nft-image">🎨</div>
                      <div className="nft-info">
                        <h4>Digital Art Piece</h4>
                        <p>Price: 75 ADA</p>
                        <button>Buy Now</button>
                      </div>
                    </div>
                    <div className="nft-card">
                      <div className="nft-image">⚡</div>
                      <div className="nft-info">
                        <h4>ADA Nexus Exclusive</h4>
                        <p>Price: 250 ADA</p>
                        <button>Buy Now</button>
                      </div>
                    </div>
                  </div>
                </div>
              )}

              {activeTab === 'dex' && (
                <div className="dex-tab">
                  <h3>💱 Decentralized Exchange</h3>
                  <div className="swap-box">
                    <div className="swap-input">
                      <input type="number" placeholder="0.0" defaultValue="100" />
                      <select>
                        <option>ADA</option>
                        <option>NEXUS</option>
                        <option>MIN</option>
                      </select>
                    </div>
                    <div className="swap-arrow">🔄</div>
                    <div className="swap-input">
                      <input type="number" placeholder="0.0" defaultValue="450" />
                      <select>
                        <option>NEXUS</option>
                        <option>ADA</option>
                        <option>MIN</option>
                      </select>
                    </div>
                    <button className="swap-btn">💫 Swap Tokens</button>
                  </div>
                </div>
              )}

              {activeTab === 'p2p' && (
                <div className="p2p-tab">
                  <h3>🔁 P2P Trading</h3>
                  <p>Trade directly with other users securely</p>
                  <div className="p2p-offers">
                    <div className="offer-card">
                      <div className="offer-header">
                        <span>Seller: CryptoTrader</span>
                        <span>⭐ 4.8</span>
                      </div>
                      <div className="offer-details">
                        <p>💎 Selling: 1,000 NEXUS</p>
                        <p>💰 Price: $0.15 per token</p>
                        <p>🏦 Payment: Bank Transfer</p>
                        <p>⏱️ Time Limit: 30 minutes</p>
                      </div>
                      <button>🛒 Buy Now</button>
                    </div>
                    <div className="offer-card">
                      <div className="offer-header">
                        <span>Seller: AdaWhale</span>
                        <span>⭐ 4.9</span>
                      </div>
                      <div className="offer-details">
                        <p>💎 Selling: 5,000 ADA</p>
                        <p>💰 Price: $0.45 per ADA</p>
                        <p>🏦 Payment: PayPal</p>
                        <p>⏱️ Time Limit: 1 hour</p>
                      </div>
                      <button>🛒 Buy Now</button>
                    </div>
                  </div>
                </div>
              )}

              {activeTab === 'token' && (
                <div className="token-tab">
                  <h3>🪙 Create Your Token</h3>
                  <div className="token-form">
                    <input type="text" placeholder="Token Name (e.g., MyToken)" defaultValue="NexusToken" />
                    <input type="text" placeholder="Token Symbol (e.g., NEX)" defaultValue="NEX" />
                    <input type="number" placeholder="Total Supply (e.g., 1000000)" defaultValue="1000000" />
                    <input type="number" placeholder="Decimals (e.g., 6)" defaultValue="6" />
                    <button>🚀 Create Token</button>
                  </div>
                </div>
              )}
            </div>
          </div>
        )}
      </main>

      {/* Footer */}
      <footer className="footer">
        <p>© 2024 ADA Nexus - Built on Cardano 🌐</p>
      </footer>
    </div>
  )
}

export default App