import React from 'react'

export const Footer: React.FC = () => {
  return (
    <footer className="app-footer">
      <div className="footer-content">
        <div className="logo-section">
          <div className="logo-icon">⚡</div>
          <span className="logo-text">ADA Nexus</span>
        </div>
        
        <div className="footer-links">
          <a href="#" className="footer-link">Documentation</a>
          <a href="#" className="footer-link">GitHub</a>
          <a href="#" className="footer-link">Support</a>
          <a href="#" className="footer-link">Terms</a>
        </div>
        
        <div className="copyright" style={{ color: 'var(--text-secondary)' }}>
          © 2024 ADA Nexus. Built on Cardano.
        </div>
      </div>
    </footer>
  )
}