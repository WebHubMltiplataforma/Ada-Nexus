import React, { useState } from 'react'

export const SwapInterface: React.FC = () => {
  const [fromToken, setFromToken] = useState({ symbol: 'ADA', amount: '' })
  const [toToken, setToToken] = useState({ symbol: 'NEXUS', amount: '' })
  const [loading, setLoading] = useState(false)

  const handleSwap = async () => {
    setLoading(true)
    // Simular swap
    setTimeout(() => {
      alert(`Swapped ${fromToken.amount} ${fromToken.symbol} to ${toToken.amount} ${toToken.symbol}`)
      setLoading(false)
    }, 2000)
  }

  const handleFromAmountChange = (amount: string) => {
    setFromToken(prev => ({ ...prev, amount }))
    // Simular cálculo de conversión
    if (amount && !isNaN(Number(amount))) {
      const converted = (Number(amount) * 2.5).toString()
      setToToken(prev => ({ ...prev, amount: converted }))
    } else {
      setToToken(prev => ({ ...prev, amount: '' }))
    }
  }

  return (
    <div className="dex-interface">
      <h2>Token Swap</h2>
      <p style={{ 
        color: 'var(--text-secondary)', 
        marginBottom: '2rem' 
      }}>
        Trade tokens instantly on Cardano
      </p>
      
      <div style={{ maxWidth: '400px', margin: '0 auto' }}>
        <div style={{
          background: 'var(--background-dark)',
          padding: '1.5rem',
          borderRadius: '12px',
          border: '1px solid rgba(255, 255, 255, 0.1)'
        }}>
          {/* From Token */}
          <div style={{ marginBottom: '1rem' }}>
            <div style={{
              display: 'flex',
              justifyContent: 'space-between',
              marginBottom: '0.5rem',
              color: 'var(--text-secondary)',
              fontSize: '0.9rem'
            }}>
              <span>From</span>
              <span>Balance: 100 ADA</span>
            </div>
            <div style={{
              display: 'flex',
              alignItems: 'center',
              gap: '0.5rem',
              background: 'var(--background-light)',
              padding: '1rem',
              borderRadius: '8px',
              border: '1px solid rgba(255, 255, 255, 0.2)'
            }}>
              <input
                type="text"
                value={fromToken.amount}
                onChange={(e) => handleFromAmountChange(e.target.value)}
                placeholder="0.0"
                style={{
                  background: 'none',
                  border: 'none',
                  color: 'var(--text-primary)',
                  fontSize: '1.5rem',
                  width: '100%',
                  outline: 'none'
                }}
              />
              <button style={{
                background: 'var(--primary-color)',
                border: 'none',
                color: 'white',
                padding: '0.5rem 1rem',
                borderRadius: '6px',
                cursor: 'pointer',
                minWidth: '80px'
              }}>
                {fromToken.symbol} ▼
              </button>
            </div>
          </div>

          {/* Swap Arrow */}
          <div style={{
            textAlign: 'center',
            margin: '1rem 0',
            fontSize: '1.5rem',
            color: 'var(--secondary-color)'
          }}>
            ↓
          </div>

          {/* To Token */}
          <div style={{ marginBottom: '1.5rem' }}>
            <div style={{
              display: 'flex',
              justifyContent: 'space-between',
              marginBottom: '0.5rem',
              color: 'var(--text-secondary)',
              fontSize: '0.9rem'
            }}>
              <span>To</span>
              <span>Balance: 0 NEXUS</span>
            </div>
            <div style={{
              display: 'flex',
              alignItems: 'center',
              gap: '0.5rem',
              background: 'var(--background-light)',
              padding: '1rem',
              borderRadius: '8px',
              border: '1px solid rgba(255, 255, 255, 0.2)'
            }}>
              <input
                type="text"
                value={toToken.amount}
                readOnly
                placeholder="0.0"
                style={{
                  background: 'none',
                  border: 'none',
                  color: 'var(--text-primary)',
                  fontSize: '1.5rem',
                  width: '100%',
                  outline: 'none'
                }}
              />
              <button style={{
                background: 'var(--primary-color)',
                border: 'none',
                color: 'white',
                padding: '0.5rem 1rem',
                borderRadius: '6px',
                cursor: 'pointer',
                minWidth: '80px'
              }}>
                {toToken.symbol} ▼
              </button>
            </div>
          </div>

          {/* Swap Info */}
          <div style={{
            background: 'var(--background-light)',
            padding: '1rem',
            borderRadius: '8px',
            marginBottom: '1.5rem',
            fontSize: '0.9rem'
          }}>
            <div style={{
              display: 'flex',
              justifyContent: 'space-between',
              marginBottom: '0.5rem'
            }}>
              <span style={{ color: 'var(--text-secondary)' }}>Price</span>
              <span>1 ADA = 2.5 NEXUS</span>
            </div>
            <div style={{
              display: 'flex',
              justifyContent: 'space-between'
            }}>
              <span style={{ color: 'var(--text-secondary)' }}>Slippage</span>
              <span>1%</span>
            </div>
          </div>

          {/* Swap Button */}
          <button 
            onClick={handleSwap}
            disabled={!fromToken.amount || loading}
            style={{
              width: '100%',
              padding: '1rem',
              background: loading ? 'var(--text-secondary)' : 'var(--secondary-color)',
              color: 'white',
              border: 'none',
              borderRadius: '8px',
              fontSize: '1.1rem',
              cursor: loading ? 'not-allowed' : 'pointer',
              transition: 'background 0.3s ease'
            }}
          >
            {loading ? 'Swapping...' : 'Swap Tokens'}
          </button>
        </div>
      </div>
    </div>
  )
}