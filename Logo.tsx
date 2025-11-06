import React from 'react';

interface LogoProps {
  size?: number;
  animated?: boolean;
  className?: string;
}

export const Logo: React.FC<LogoProps> = ({ 
  size = 40, 
  animated = true,
  className = '' 
}) => {
  return (
    <div className={`logo-container ${className}`}>
      <svg
        width={size}
        height={size}
        viewBox="0 0 100 100"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
        className={`ada-nexus-logo ${animated ? 'animated' : ''}`}
      >
        {/* Círculo de fondo */}
        <circle
          cx="50"
          cy="50"
          r="45"
          fill="url(#gradient)"
          stroke="url(#borderGradient)"
          strokeWidth="2"
        />
        
        {/* Símbolo ADA */}
        <path
          d="M35 35 L65 35 L65 65 L35 65 Z"
          fill="none"
          stroke="white"
          strokeWidth="4"
          strokeLinecap="round"
          className="logo-square"
        />
        
        {/* Rayo interno */}
        <path
          d="M45 40 L55 40 L50 60 Z"
          fill="white"
          className="logo-bolt"
        />
        
        {/* Partículas alrededor */}
        <circle cx="30" cy="30" r="2" fill="white" className="particle particle-1"/>
        <circle cx="70" cy="30" r="2" fill="white" className="particle particle-2"/>
        <circle cx="30" cy="70" r="2" fill="white" className="particle particle-3"/>
        <circle cx="70" cy="70" r="2" fill="white" className="particle particle-4"/>
        
        <defs>
          <linearGradient id="gradient" x1="0%" y1="0%" x2="100%" y2="100%">
            <stop offset="0%" stopColor="#0033ad" />
            <stop offset="50%" stopColor="#0066ff" />
            <stop offset="100%" stopColor="#00a3ff" />
          </linearGradient>
          
          <linearGradient id="borderGradient" x1="0%" y1="0%" x2="100%" y2="100%">
            <stop offset="0%" stopColor="#7b61ff" />
            <stop offset="100%" stopColor="#00d395" />
          </linearGradient>
        </defs>
      </svg>
      
      {/* Texto del logo */}
      <span className="logo-text">ADA Nexus</span>
    </div>
  );
};