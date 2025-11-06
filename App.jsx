console.log('🎯 App.jsx is loading...');

import React, { useState } from 'react'

// App principal - EXTREMADAMENTE SIMPLE
function App() {
  const [isConnected, setIsConnected] = useState(false)
  
  console.log('🔁 App component rendering, isConnected:', isConnected);

  const connectWallet = () => {
    console.log('🔄 Connecting wallet...');
    setIsConnected(true);
  }

  const disconnectWallet = () => {
    console.log('🔌 Disconnecting wallet...');
    setIsConnected(false);
  }

  // Estilos en línea para garantizar que se apliquen
  const styles = {
    app: {
      minHeight: '100vh',
      background: '#0f172a',
      color: 'white',
      fontFamily: 'Arial, sans-serif'
    },
    header: {
      background: '#1e293b',
      padding: '1rem 2rem',
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      borderBottom: '2px solid #334155'
    },
    logo: {
      fontSize: '1.5rem',
      fontWeight: 'bold',
      color: '#00a3ff'
    },
    main: {
      padding: '2rem',
      maxWidth: '1200px',
      margin: '0 auto',
      textAlign: 'center'
    },
    title: {
      fontSize: '3rem',
      marginBottom: '1rem',
      background: 'linear-gradient(135deg, #00a3ff, #7b61ff)',
      WebkitBackgroundClip: 'text',
      WebkitTextFillColor: 'transparent'
    },
    subtitle: {
      fontSize: '1.2rem',
      color: '#cbd5e1',
      marginBottom: '3rem'
    },
    button: {
      background: '#00d395',
      color: 'white',
      border: 'none',
      padding: '1rem 2rem',
      fontSize: '1.1rem',
      borderRadius: '8px',
      cursor: 'pointer',
      margin: '0.5rem'
    },
    disconnectBtn: {
      background: '#dc2626',
      color: 'white',
      border: 'none',
      padding: '0.5rem 1rem',
      borderRadius: '6px',
      cursor: 'pointer'
    },
    card: {
      background: '#1e293b',
      padding: '2rem',
      borderRadius: '12px',
      margin: '1rem 0',
      border: '1px solid #334155'
    },
    features: {
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fit, minmax(250px, 1fr))',
      gap: '1.5rem',
      margin: '2rem 0'
    },
    feature: {
      background: '#334155',
      padding: '1.5rem',
      borderRadius: '8px',
      textAlign: 'center'
    }
  }

  return (
    <div style={styles.app}>
      {/* Header */}
      <header style={styles.header}>
        <div style={styles.logo}>⚡ ADA Nexus</div>
        {isConnected && (
          <button style={styles.disconnectBtn} onClick={disconnectWallet}>
            Disconnect
          </button>
        )}
      </header>

      {/* Main Content */}
      <main style={styles.main}>
        {!isConnected ? (
          <div>
            <h1 style={styles.title}>🚀 Welcome to ADA Nexus</h1>
            <p style={styles.subtitle}>Your Cardano dApp Platform</p>
            
            <div style={styles.features}>
              <div style={feature}>
                <div style={{fontSize: '3rem', marginBottom: '1rem'}}>🦭</div>
                <h3>Wallet Integration</h3>
                <p>Connect with Cardano wallets</p>
              </div>
              <div style={feature}>
                <div style={{fontSize: '3rem', marginBottom: '1rem'}}>🎨</div>
                <h3>NFT Marketplace</h3>
                <p>Buy and sell NFTs</p>
              </div>
              <div style={feature}>
                <div style={{fontSize: '3rem', marginBottom: '1rem'}}>💱</div>
                <h3>DEX Trading</h3>
                <p>Swap tokens easily</p>
              </div>
            </div>

            <button style={styles.button} onClick={connectWallet}>
              🔗 Connect Wallet
            </button>
            
            <div style={{...styles.card, background: '#f59e0b22', borderColor: '#f59e0b'}}>
              <strong>Demo Mode:</strong> Fully functional interface with simulated data
            </div>
          </div>
        ) : (
          <div>
            <h1 style={styles.title}>💰 Wallet Connected</h1>
            <p style={styles.subtitle}>Your ADA Nexus Dashboard</p>
            
            <div style={styles.card}>
              <h2>Balance: 125.75 ADA</h2>
              <p>Address: addr1...xyz</p>
            </div>
            
            <div style={styles.features}>
              <div style={feature}>
                <h3>🎨 Marketplace</h3>
                <p>Browse NFTs</p>
                <button style={{...styles.button, padding: '0.5rem 1rem', fontSize: '1rem'}}>
                  Explore
                </button>
              </div>
              <div style={feature}>
                <h3>💱 DEX</h3>
                <p>Trade tokens</p>
                <button style={{...styles.button, padding: '0.5rem 1rem', fontSize: '1rem'}}>
                  Swap
                </button>
              </div>
              <div style={feature}>
                <h3>🔁 P2P</h3>
                <p>Peer-to-peer trading</p>
                <button style={{...styles.button, padding: '0.5rem 1rem', fontSize: '1rem'}}>
                  Trade
                </button>
              </div>
            </div>
          </div>
        )}
      </main>

      {/* Footer */}
      <footer style={{
        background: '#1e293b',
        padding: '2rem',
        textAlign: 'center',
        color: '#cbd5e1',
        marginTop: 'auto',
        borderTop: '2px solid #334155'
      }}>
        <p>© 2024 ADA Nexus - Built on Cardano</p>
      </footer>
    </div>
  )
}

// Estilo de feature reutilizable
const feature = {
  background: '#334155',
  padding: '1.5rem',
  borderRadius: '8px',
  textAlign: 'center'
}

console.log('✅ App component defined');
export default App