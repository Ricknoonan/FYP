# Final Year Project
## Haskell DSL for smart contracts on the ethereum blockchain

This is a smart contract DSL written in haskell that will be able to produce Solidity contracts. 

When run, two options are available: 
1. Simulate the contract: This evaluates a sample contract and shows how the contract is being evaluated in the output. Provides a sandbox enviorment to test contract.
2. Generate Solidity smart contract: This takes the contract written using the Haskell DSL and produces a valid Solidity contract

## Run

### Stack
* `$ stack build `
* `$ stack exec fyp `

### Cabal 
* `$ cabal run`
