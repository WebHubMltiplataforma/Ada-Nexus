import React, { useState } from 'react';

interface WalletDashboardProps {
  network: 'mainnet' | 'preprod' | 'testnet';
}

export const WalletDashboard: React.FC<WalletDashboardProps> = ({ network }) => {
  const [walletInfo, setWalletInfo] = useState<any>(null);
  const [balance, setBalance] = useState({ 
    ada: '0', 
    assets: [] as any[] 
  });
  const [isLoading, setIsLoading] = useState(false);

  // Datos de ejemplo para la demo
  const demoWallets = [
    { name: 'nami', icon: '🦭', description: 'Nami Wallet' },
    { name: 'eternl', icon: '⚡', description: 'Eternl Wallet' },
    { name: 'flint', icon: '🔥', description: 'Flint Wallet' },
    { name: 'yoroi', icon: '🎯', description: 'Yoroi Wallet' }
  ];

  const demoBalance = {
    ada: '125.75',
    assets: [
      { unit: 'NEXUS', quantity: '1000000', name: 'Nexus Token' },
      { unit: 'asset1', quantity: '150', name: 'Test Asset' },
      { unit: 'asset2', quantity: '500', name: 'Demo Token' }
    ]
  };

  const connectWallet = async (walletName: string) => {
    setIsLoading(true);
    
    // Simular conexión a wallet
    await new Promise(resolve => setTimeout(resolve, 1500));
    
    setWalletInfo({
      name: walletName,
      address: 'addr1qxhyajcrgsrhk335sr9uvrsmuwxnknk9f547f53e9nxfc92dtwpt3uge70wdftndq4wtx5ja86jqeznhl6kgpdnqlkyqcxtvaf',
      network: network
    });
    
    setBalance(demoBalance);
    setIsLoading(false);
  };

  const disconnectWallet = () => {
    setWalletInfo(null);
    setBalance({ ada: '0', assets: [] });
  };

  const sendTransaction = async (recipient: string, amount: string) => {
    if (!walletInfo) return;
    
    setIsLoading(true);
    // Simular envío de transacción
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    alert(`✅ Simulated transaction sent!\nTo: ${recipient}\nAmount: ${amount} ADA`);
    setIsLoading(false);
  };

  return (
    <div className="wallet-dashboard">
      <div className="dashboard-header">
        <h2>Wallet Dashboard</h2>
        <div className="network-badge">
          Network: {network.toUpperCase()}
        </div>
      </div>

      {!walletInfo ? (
        <div className="connection-section">
          <div className="connection-header">
            <h3>🔗 Connect Your Wallet</h3>
            <p>Choose a Cardano wallet to connect to ADA Nexus</p>
          </div>

          <div className="wallets-grid">
            {demoWallets.map(wallet => (
              <div key={wallet.name} className="wallet-card">
                <div className="wallet-icon">{wallet.icon}</div>
                <h4>{wallet.description}</h4>
                <p>Connect to your {wallet.description}</p>
                <button
                  onClick={() => connectWallet(wallet.name)}
                  disabled={isLoading}
                  className="connect-btn"
                >
                  {isLoading ? 'Connecting...' : 'Connect'}
                </button>
              </div>
            ))}
          </div>

          <div className="wallet-help">
            <h4>📖 Don't have a wallet?</h4>
            <p>Install one of these popular Cardano wallets:</p>
            <ul>
              <li><a href="https://namiwallet.io" target="_blank" rel="noopener noreferrer">Nami Wallet</a> - Easy to use browser extension</li>
              <li><a href="https://eternl.io" target="_blank" rel="noopener noreferrer">Eternl Wallet</a> - Feature-rich browser extension</li>
              <li><a href="https://flint-wallet.com" target="_blank" rel="noopener noreferrer">Flint Wallet</a> - Secure and user-friendly</li>
            </ul>
          </div>
        </div>
      ) : (
        <div className="wallet-connected">
          <div className="wallet-header">
            <div className="wallet-info">
              <h3>✅ Connected to {walletInfo.name}</h3>
              <div className="wallet-address">
                {walletInfo.address.slice(0, 15)}...{walletInfo.address.slice(-15)}
              </div>
            </div>
            <button onClick={disconnectWallet} className="disconnect-btn">
              Disconnect
            </button>
          </div>

          {/* Balance Section */}
          <div className="balance-section">
            <div className="balance-card">
              <div className="balance-main">
                <span className="balance-amount">{balance.ada}</span>
                <span className="balance-unit">ADA</span>
              </div>
              <div className="balance-details">
                <div className="assets-count">
                  {balance.assets.length} assets in wallet
                </div>
              </div>
            </div>

            {balance.assets.length > 0 && (
              <div className="assets-section">
                <h4>Your Assets</h4>
                <div className="assets-list">
                  {balance.assets.map((asset, index) => (
                    <div key={index} className="asset-item">
                      <div className="asset-info">
                        <span className="asset-name">{asset.name}</span>
                        <span className="asset-quantity">{asset.quantity}</span>
                      </div>
                      <span className="asset-unit">{asset.unit}</span>
                    </div>
                  ))}
                </div>
              </div>
            )}
          </div>

          {/* Send Transaction Section */}
          <div className="send-section">
            <h4>📤 Send Transaction</h4>
            <form onSubmit={(e) => {
              e.preventDefault();
              const formData = new FormData(e.currentTarget);
              const recipient = formData.get('recipient') as string;
              const amount = formData.get('amount') as string;
              
              if (recipient && amount) {
                sendTransaction(recipient, amount);
              }
            }} className="send-form">
              <div className="form-group">
                <label>Recipient Address:</label>
                <input
                  type="text"
                  name="recipient"
                  placeholder="addr1..."
                  required
                  defaultValue="addr1qxhyajcrgsrhk335sr9uvrsmuwxnknk9f547f53e9nxfc92dtwpt3uge70wdftndq4wtx5ja86jqeznhl6kgpdnqlkyqcxtvaf"
                />
              </div>
              <div className="form-group">
                <label>Amount (ADA):</label>
                <input
                  type="number"
                  name="amount"
                  placeholder="0.0"
                  step="0.000001"
                  min="0.000001"
                  required
                  defaultValue="10"
                />
              </div>
              <button type="submit" disabled={isLoading} className="send-btn">
                {isLoading ? 'Sending...' : 'Send ADA'}
              </button>
            </form>
          </div>

          {/* Transaction History */}
          <div className="history-section">
            <h4>📋 Recent Activity</h4>
            <div className="transaction-list">
              <div className="transaction-item">
                <div className="tx-type receive">Received</div>
                <div className="tx-details">
                  <span className="tx-amount">+50 ADA</span>
                  <span className="tx-date">Today, 14:30</span>
                </div>
              </div>
              <div className="transaction-item">
                <div className="tx-type send">Sent</div>
                <div className="tx-details">
                  <span className="tx-amount">-25 ADA</span>
                  <span className="tx-date">Yesterday, 09:15</span>
                </div>
              </div>
              <div className="transaction-item">
                <div className="tx-type receive">Received</div>
                <div className="tx-details">
                  <span className="tx-amount">+100 NEXUS</span>
                  <span className="tx-date">Dec 12, 2024</span>
                </div>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};