import React from 'react';

interface CSSLogoProps {
  size?: number;
  animated?: boolean;
}

export const CSSLogo: React.FC<CSSLogoProps> = ({ size = 40, animated = true }) => {
  return (
    <div className={`css-logo-container ${animated ? 'animated' : ''}`}>
      <div className="css-logo">
        <div className="logo-base">
          <div className="logo-cube">
            <div className="face front"></div>
            <div className="face back"></div>
            <div className="face left"></div>
            <div className="face right"></div>
            <div className="face top"></div>
            <div className="face bottom"></div>
          </div>
          <div className="logo-bolt"></div>
        </div>
      </div>
      <span className="logo-text">ADA Nexus</span>
    </div>
  );
};