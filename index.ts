import express from 'express'
import { TransactionIndexer } from './indexers/TransactionIndexer'

const app = express()
const port = process.env.PORT || 3000

// Start the transaction indexer
const indexer = new TransactionIndexer(process.env.BLOCKFROST_PROJECT_ID!)
indexer.startIndexing()

app.get('/', (req, res) => {
  res.send('ADA Nexus Backend is running!')
})

app.listen(port, () => {
  console.log(`Backend server listening on port ${port}`)
})