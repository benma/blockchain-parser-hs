#blockchain-parser-hs

blockchain-parser-hs is a fast parser for the Bitcoin blockchain written in Haskell. The library provides functions to easily parse the blockchain and extract relevant data. Haskell's lazy evaluation model is used to only parse, in a seamless way, only the data that is really accessed, while skipping the rest.
It has yet to be properly documented, but the parser itself (located in [src/BlockchainParser.hs](https://github.com/benma/blockchain-parser-hs/blob/master/src/BlockchainParser.hs)) should be fairly self-explanatory.

Additional resources:
* [Bitcoin: 285 bytes that changed the world](http://james.lab6.com/2012/01/12/bitcoin-285-bytes-that-changed-the-world/)
* [Bitcoin Blocks](https://en.bitcoin.it/wiki/Blocks)
* [Bitcoin Transactions](https://en.bitcoin.it/wiki/Transaction)
* [Bitcoin Script](https://en.bitcoin.it/wiki/Script)

## Running the demo app:

First, compile: `cabal install`

To calculate the total balance of a number of public addresses, run
`./blockchain-parser once < public_addresses.txt`

The program looks for the raw block data in ~/.bitcoin/blocks (as produced by bitcoind).

Run `./blockchain-parser --help` to see more options.

## Caveats

The parser assumes that the blocks are ordered, i.e. that block referenced by another block appears before that block (the blocks stored by bitcoind obey this).
Also, the parser currently looks for the longest chain, not for the heaviest chain by difficulty. This is not technically correct but yields the same result.

## Tip Jar
If you found this helpful, consider donating: 15JDdMQzH936AfZPvy6mAyjWdEnRaNtUxm
