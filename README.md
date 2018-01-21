# bitcoin-distributed-multisig-signer
## Compiling from source

This is a Haskell application and is designed to be built with the Haskell Stack tool.

1. Clone this repo.
2. Install [stack].
3. Build with `stack build`.
4. Run with `stack exec -- bitcoin-distributed-multisig-signer --help` to see usage instructions.

# The purpose of this application
It's an embarrasing fact of Bitcoin that whoever controls an adress' private keys controls the coins in it.
In particular, if an address' private keys are compromised, then you can consider the value of coins in
that address to be lost. The history of Bitcoin heists is full of exchanges getting cleaned out by an
assailant who managed to get access to a server hosting the cold wallet's sole private key.

We can mitigate this attack vector significantly by doing the following:
1. Create a multisig wallet, requiring `m-of-n` signatures.
2. Have a single server create transactions spending from this wallet; specifically, in order to create transactions
    this server needs only the adress, so it may query the blockchain for unspect transaction outputs.
3. Have several remote servers, each holding one key, such that the number of unique keys held this way is
    at least `m`; each of these servers also needs to hold the redeem script.
4. The main server should then send the transaction to at least `m` key servers, in the same order as the order
    of the public keys used to create the address.
5. The main server can then brodcast this signed transaction.

This application strives to be a minimal, yet somewhat-secure implementation of the key server in step 3.

By running multiple signing servers, on multiple hosts and multiple hosting providers, we can significantly
increase the difficulty of successfully compromising the distributed multisig cold wallet. Of course, a chain
is only as strong as its weakest link, and such a set up won't help much if, for example, all the key servers
are accessible via `ssh` without credentials from a single machine...

Note that I obviously make no claim that this application is perfectly safe to use with real bitcoins. If it fucks
something up due to a bug, it is ultimately not my fault. The user takes full responsibility for any losses
caused by using this application, even when used correctly and as intended.

# Signing a transaction
To sign a transaction with a single key, `POST` the hex transaction as plain, unquoted text to HTTP endpoint `/sign`:
    curl -H "Content-Type: text/plain;charset=utf-8" -d "0100000003ae3f2c203d92f47e207740a02cefb06550a699b4c458544bf41cf1f759107bb10100000000ffffffff92172b1165f32195f44292c07626fe99d5a5ed867cb9f5d7793862c8ffffff053c0b6af96f8bc6d09c373e649d26d9920f855f61ca315d421554d5a7def84d0100000000ffffffff02002d3101000000001976a91481f1e58aeedf48b66815f513a6aaf6a6cad875f188acf4a2fc0b0000000017a91443e5fee10ca1666bdd38c49f6931aba2f145b6258700000000" http://localhost:8080/sign

[stack]: http://docs.haskellstack.org/en/stable/install_and_upgrade.html
