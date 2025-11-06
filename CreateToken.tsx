import React, { useState } from 'react';
import { TokenService, TokenMetadata } from '../../services/TokenService';

export const CreateToken: React.FC = () => {
  const [tokenService] = useState(new TokenService());
  const [isCreating, setIsCreating] = useState(false);
  const [result, setResult] = useState<{ txHash: string; policyId: string; assetName: string } | null>(null);

  const [tokenData, setTokenData] = useState({
    name: 'Nexus Token',
    ticker: 'NEXUS',
    totalSupply: 1000000,
    description: 'Utility token for ADA Nexus platform',
    decimals: 6,
    url: 'https://ada-nexus.io',
    logo: ''
  });

  const handleCreateToken = async () => {
    try {
      setIsCreating(true);
      
      // Inicializar servicio
      await tokenService.initialize('preprod'); // Cambiar a 'mainnet' en producción
      
      const metadata: TokenMetadata = {
        name: tokenData.name,
        description: tokenData.description,
        ticker: tokenData.ticker,
        url: tokenData.url,
        logo: tokenData.logo,
        decimals: tokenData.decimals
      };

      const result = await tokenService.createNexusToken(
        tokenData.name,
        tokenData.ticker,
        tokenData.totalSupply,
        metadata
      );

      setResult(result);
      alert(`Token creado exitosamente! TX Hash: ${result.txHash}`);
      
    } catch (error) {
      console.error('Error creating token:', error);
      alert(`Error creando token: ${error}`);
    } finally {
      setIsCreating(false);
    }
  };

  const handleInputChange = (field: string, value: string | number) => {
    setTokenData(prev => ({
      ...prev,
      [field]: value
    }));
  };

  return (
    <div style={{ 
      maxWidth: '600px', 
      margin: '0 auto',
      background: 'var(--background-light)',
      padding: '2rem',
      borderRadius: '12px'
    }}>
      <h2>Crear Token $NEXUS</h2>
      <p style={{ color: 'var(--text-secondary)', marginBottom: '2rem' }}>
        Crea tu token personalizado en la blockchain de Cardano
      </p>

      <div style={{ display: 'flex', flexDirection: 'column', gap: '1rem' }}>
        <div>
          <label style={{ display: 'block', marginBottom: '0.5rem', color: 'var(--text-primary)' }}>
            Nombre del Token *
          </label>
          <input
            type="text"
            value={tokenData.name}
            onChange={(e) => handleInputChange('name', e.target.value)}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: 'var(--background-dark)',
              border: '1px solid rgba(255, 255, 255, 0.2)',
              borderRadius: '6px',
              color: 'var(--text-primary)',
              fontSize: '1rem'
            }}
          />
        </div>

        <div>
          <label style={{ display: 'block', marginBottom: '0.5rem', color: 'var(--text-primary)' }}>
            Símbolo (Ticker) *
          </label>
          <input
            type="text"
            value={tokenData.ticker}
            onChange={(e) => handleInputChange('ticker', e.target.value.toUpperCase())}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: 'var(--background-dark)',
              border: '1px solid rgba(255, 255, 255, 0.2)',
              borderRadius: '6px',
              color: 'var(--text-primary)',
              fontSize: '1rem'
            }}
          />
        </div>

        <div>
          <label style={{ display: 'block', marginBottom: '0.5rem', color: 'var(--text-primary)' }}>
            Suministro Total *
          </label>
          <input
            type="number"
            value={tokenData.totalSupply}
            onChange={(e) => handleInputChange('totalSupply', parseInt(e.target.value) || 0)}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: 'var(--background-dark)',
              border: '1px solid rgba(255, 255, 255, 0.2)',
              borderRadius: '6px',
              color: 'var(--text-primary)',
              fontSize: '1rem'
            }}
          />
        </div>

        <div>
          <label style={{ display: 'block', marginBottom: '0.5rem', color: 'var(--text-primary)' }}>
            Decimales *
          </label>
          <select
            value={tokenData.decimals}
            onChange={(e) => handleInputChange('decimals', parseInt(e.target.value))}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: 'var(--background-dark)',
              border: '1px solid rgba(255, 255, 255, 0.2)',
              borderRadius: '6px',
              color: 'var(--text-primary)',
              fontSize: '1rem'
            }}
          >
            <option value={0}>0 (NFT-like)</option>
            <option value={6}>6 (Como ADA)</option>
            <option value={8}>8 (Como Bitcoin)</option>
            <option value={18}>18 (Como Ethereum)</option>
          </select>
        </div>

        <div>
          <label style={{ display: 'block', marginBottom: '0.5rem', color: 'var(--text-primary)' }}>
            Descripción *
          </label>
          <textarea
            value={tokenData.description}
            onChange={(e) => handleInputChange('description', e.target.value)}
            rows={3}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: 'var(--background-dark)',
              border: '1px solid rgba(255, 255, 255, 0.2)',
              borderRadius: '6px',
              color: 'var(--text-primary)',
              fontSize: '1rem',
              resize: 'vertical'
            }}
          />
        </div>

        <div>
          <label style={{ display: 'block', marginBottom: '0.5rem', color: 'var(--text-primary)' }}>
            URL del Proyecto
          </label>
          <input
            type="url"
            value={tokenData.url}
            onChange={(e) => handleInputChange('url', e.target.value)}
            style={{
              width: '100%',
              padding: '0.75rem',
              background: 'var(--background-dark)',
              border: '1px solid rgba(255, 255, 255, 0.2)',
              borderRadius: '6px',
              color: 'var(--text-primary)',
              fontSize: '1rem'
            }}
          />
        </div>

        <button
          onClick={handleCreateToken}
          disabled={isCreating}
          style={{
            width: '100%',
            padding: '1rem',
            background: isCreating ? 'var(--text-secondary)' : 'var(--success-color)',
            color: 'white',
            border: 'none',
            borderRadius: '8px',
            fontSize: '1.1rem',
            cursor: isCreating ? 'not-allowed' : 'pointer',
            marginTop: '1rem'
          }}
        >
          {isCreating ? 'Creando Token...' : '🚀 Crear Token $NEXUS'}
        </button>
      </div>

      {result && (
        <div style={{
          marginTop: '2rem',
          padding: '1rem',
          background: 'var(--background-dark)',
          borderRadius: '8px',
          border: '1px solid var(--success-color)'
        }}>
          <h3 style={{ color: 'var(--success-color)', marginBottom: '1rem' }}>
            ✅ Token Creado Exitosamente
          </h3>
          <div style={{ textAlign: 'left', fontSize: '0.9rem' }}>
            <p><strong>Transaction Hash:</strong> {result.txHash}</p>
            <p><strong>Policy ID:</strong> {result.policyId}</p>
            <p><strong>Asset Name:</strong> {result.assetName}</p>
            <p><strong>Token ID:</strong> {result.policyId + result.assetName}</p>
          </div>
        </div>
      )}

      <div style={{
        marginTop: '2rem',
        padding: '1rem',
        background: 'rgba(255, 193, 7, 0.1)',
        borderRadius: '8px',
        border: '1px solid var(--warning-color)'
      }}>
        <h4 style={{ color: 'var(--warning-color)' }}>⚠️ Importante</h4>
        <ul style={{ textAlign: 'left', color: 'var(--text-secondary)', fontSize: '0.9rem' }}>
          <li>Necesitas ADA en tu wallet para pagar los fees de transacción</li>
          <li>Recomendado: 5-10 ADA mínimo en testnet</li>
          <li>El token se creará en la red de prueba (preprod)</li>
          <li>Guarda el Policy ID para futuras operaciones</li>
        </ul>
      </div>
    </div>
  );
};