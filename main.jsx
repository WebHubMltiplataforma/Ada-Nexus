console.log('🎯 main.jsx started executing');

// Verificar que React está disponible
if (typeof React === 'undefined') {
  console.error('❌ React is not defined');
  document.getElementById('root').innerHTML = `
    <div style="padding: 2rem; text-align: center; background: #dc2626; color: white;">
      <h1>React Not Loaded</h1>
      <p>Check the browser console for module errors</p>
    </div>
  `;
} else {
  console.log('✅ React is available:', typeof React);
}

import React from 'react';
import ReactDOM from 'react-dom/client';

console.log('✅ React imports successful');

const App = () => {
  const [connected, setConnected] = React.useState(false);
  
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
    main: {
      padding: '2rem',
      textAlign: 'center',
      flex: 1
    },
    button: {
      background: '#00d395',
      color: 'white',
      border: 'none',
      padding: '1rem 2rem',
      fontSize: '1.2rem',
      borderRadius: '8px',
      cursor: 'pointer',
      marginTop: '2rem'
    }
  };

  return (
    <div style={styles.app}>
      <header style={styles.header}>
        <h1 style={{color: '#00a3ff', margin: 0}}>⚡ ADA Nexus</h1>
        {connected && <span>Connected</span>}
      </header>
      
      <main style={styles.main}>
        <h2 style={{fontSize: '2.5rem', marginBottom: '1rem'}}>
          {connected ? '🎉 Wallet Connected!' : '🚀 Welcome to ADA Nexus'}
        </h2>
        
        <p style={{fontSize: '1.2rem', color: '#cbd5e1', marginBottom: '2rem'}}>
          {connected ? 'Your Cardano dashboard is ready!' : 'Your complete Cardano dApp platform'}
        </p>
        
        <button 
          style={styles.button}
          onClick={() => setConnected(!connected)}
        >
          {connected ? '🔌 Disconnect' : '🔗 Connect Wallet'}
        </button>
        
        {connected && (
          <div style={{
            background: '#1e293b',
            padding: '2rem',
            borderRadius: '12px',
            marginTop: '2rem',
            maxWidth: '500px',
            margin: '2rem auto'
          }}>
            <h3>💰 Wallet Balance</h3>
            <p style={{fontSize: '2rem', color: '#00d395', margin: '1rem 0'}}>125.75 ADA</p>
            <p style={{color: '#cbd5e1'}}>Address: addr1qxy...zabc</p>
          </div>
        )}
      </main>
      
      <footer style={{
        background: '#1e293b',
        padding: '2rem',
        textAlign: 'center',
        color: '#cbd5e1',
        borderTop: '2px solid #334155'
      }}>
        <p>© 2024 ADA Nexus - Built on Cardano</p>
      </footer>
    </div>
  );
};

console.log('✅ App component created');

// Renderizar la aplicación
try {
  const root = ReactDOM.createRoot(document.getElementById('root'));
  root.render(React.createElement(App));
  console.log('✅ App rendered successfully');
} catch (error) {
  console.error('❌ Rendering error:', error);
  document.getElementById('root').innerHTML = `
    <div style="padding: 2rem; text-align: center; background: #dc2626; color: white;">
      <h1>Rendering Error</h1>
      <p>${error.message}</p>
    </div>
  `;
}