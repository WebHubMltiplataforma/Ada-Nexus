import React from 'react'

interface LoaderProps {
  size?: 'small' | 'medium' | 'large'
  text?: string
}

export const Loader: React.FC<LoaderProps> = ({ 
  size = 'medium', 
  text = 'Loading...' 
}) => {
  const sizeClass = {
    small: '20px',
    medium: '40px',
    large: '60px'
  }[size]

  return (
    <div style={{ 
      display: 'flex', 
      flexDirection: 'column', 
      alignItems: 'center', 
      gap: '1rem',
      padding: '2rem'
    }}>
      <div 
        style={{
          width: sizeClass,
          height: sizeClass,
          border: '4px solid rgba(255, 255, 255, 0.3)',
          borderTop: '4px solid var(--secondary-color)',
          borderRadius: '50%',
          animation: 'spin 1s linear infinite'
        }}
      />
      {text && <p style={{ color: 'var(--text-secondary)' }}>{text}</p>}
    </div>
  )
}