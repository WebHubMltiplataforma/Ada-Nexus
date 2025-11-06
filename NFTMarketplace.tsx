import React, { useState, useEffect } from 'react'
import { Loader } from '../Common/Loader'

interface NFT {
  id: string
  name: string
  image: string
  price: string
  creator: string
  royalty: number
}

export const NFTMarketplace: React.FC = () => {
  const [nfts, setNfts] = useState<NFT[]>([])
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    // Simular carga de NFTs
    setTimeout(() => {
      setNfts([
        {
          id: '1',
          name: 'Cardano Warrior',
          image: 'https://via.placeholder.com/300x200/0033ad/ffffff?text=Cardano+NFT',
          price: '100',
          creator: 'addr1...xyz',
          royalty: 5
        },
        {
          id: '2',
          name: 'ADA Diamond',
          image: 'https://via.placeholder.com/300x200/00a3ff/ffffff?text=ADA+Art',
          price: '250',
          creator: 'addr1...abc',
          royalty: 7
        },
        {
          id: '3',
          name: 'Digital Art #1',
          image: 'https://via.placeholder.com/300x200/7b61ff/ffffff?text=Digital+Art',
          price: '75',
          creator: 'addr1...def',
          royalty: 3
        }
      ])
      setLoading(false)
    }, 1500)
  }, [])

  const handleBuyNFT = (nft: NFT) => {
    alert(`Comprando NFT: ${nft.name} por ${nft.price} ADA`)
  }

  if (loading) {
    return <Loader text="Cargando NFTs..." />
  }

  return (
    <div className="marketplace">
      <div style={{ 
        display: 'flex', 
        justifyContent: 'space-between', 
        alignItems: 'center',
        marginBottom: '2rem'
      }}>
        <h2>NFT Marketplace</h2>
        <button 
          style={{
            padding: '0.75rem 1.5rem',
            background: 'var(--secondary-color)',
            color: 'white',
            border: 'none',
            borderRadius: '8px',
            cursor: 'pointer',
            fontSize: '1rem'
          }}
        >
          🎨 Create NFT
        </button>
      </div>

      <div style={{
        display: 'grid',
        gridTemplateColumns: 'repeat(auto-fit, minmax(280px, 1fr))',
        gap: '1.5rem',
        marginTop: '2rem'
      }}>
        {nfts.map(nft => (
          <div key={nft.id} style={{
            background: 'var(--background-dark)',
            borderRadius: '12px',
            overflow: 'hidden',
            border: '1px solid rgba(255, 255, 255, 0.1)',
            transition: 'transform 0.3s ease, border-color 0.3s ease'
          }}>
            <img 
              src={nft.image} 
              alt={nft.name}
              style={{
                width: '100%',
                height: '200px',
                objectFit: 'cover'
              }}
            />
            <div style={{ padding: '1.5rem' }}>
              <h3 style={{ 
                marginBottom: '0.5rem',
                color: 'var(--text-primary)'
              }}>
                {nft.name}
              </h3>
              <div style={{ 
                color: 'var(--success-color)',
                fontWeight: 'bold',
                fontSize: '1.2rem',
                marginBottom: '0.5rem'
              }}>
                {nft.price} ADA
              </div>
              <div style={{ 
                color: 'var(--text-secondary)',
                fontSize: '0.9rem',
                marginBottom: '0.5rem'
              }}>
                by {nft.creator}
              </div>
              <div style={{ 
                color: 'var(--text-secondary)',
                fontSize: '0.9rem',
                marginBottom: '1rem'
              }}>
                Royalty: {nft.royalty}%
              </div>
              <button 
                onClick={() => handleBuyNFT(nft)}
                style={{
                  width: '100%',
                  padding: '0.75rem',
                  background: 'var(--primary-color)',
                  color: 'white',
                  border: 'none',
                  borderRadius: '6px',
                  cursor: 'pointer',
                  fontSize: '1rem',
                  transition: 'background 0.3s ease'
                }}
                onMouseOver={(e) => {
                  e.currentTarget.style.background = 'var(--secondary-color)'
                }}
                onMouseOut={(e) => {
                  e.currentTarget.style.background = 'var(--primary-color)'
                }}
              >
                Buy Now
              </button>
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}