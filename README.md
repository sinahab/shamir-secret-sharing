
# Shamir Secret Sharing in Haskell


##  TODO:
* Maybe create a Polynomial type, which is a list of Coeff, but also enforces that there is at most one Coeff with each deg.
* Make modifications to do stuff in finite-field. -> I.e. need GCD etc.
* Write tests.
* Allow user to input a string as a secret, and encode/decode to Int for processing.
* Create a nice UX for creating & combining shares.
* Use Crypto.Random or something for secure RNG.
* Maybe: change combineShares into an iterative combiner thing?
* Change LICENSE to MIT.

## Note to self:

To run tests:
```
stack test
```

To start REPL:
```
stack ghci
```
