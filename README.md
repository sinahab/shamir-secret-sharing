
# Shamir Secret Sharing

This project implements the [Shamir secret sharing algorithm](https://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing) in Haskell.

Specifically, [`test/ShamirSpec.hs`](https://github.com/sinahab/shamir-secret-sharing/blob/master/test/ShamirSpec.hs) demonstrates the secret recovery process by testing every possible combination of shares in a 3-out-of-6 threshold scheme.

## Architecture

The code is modular and [fully-tested](https://github.com/sinahab/shamir-secret-sharing/tree/master/test). The general structure is:

* [`src/Shamir.hs`](https://github.com/sinahab/shamir-secret-sharing/blob/master/src/Shamir.hs): provides the core API for shamir secret sharing.
  * `createShares`: divides a secret into shares, using a k-out-of-n threshold scheme with finite field arithmetic.
  * `combineShares`: combines shares to recover a secret, by using Lagrange interpolation.

* [`src/Polynomial.hs`](https://github.com/sinahab/shamir-secret-sharing/blob/master/src/Polynomial.hs): provides an interface for working with polynomials.
  * `constructPolynomial`: constructs a polynomial from a list of coefficients.
  * `evaluatePolynomial`: evaluates a polynomial at point x.

* [`src/Math.hs`](https://github.com/sinahab/shamir-secret-sharing/blob/master/src/Math.hs): implements Mathematical functions.
  * `extendedEuclid`: calculates the GCD and coeffcients of Bézout's identity for a and b s.t.: ax + by = gcd(a,b).
  * `modMultInverse`: calculates the multiplicative inverse of a s.t.: a * b = 1 mod p

* [`src/Util.hs`](https://github.com/sinahab/shamir-secret-sharing/blob/master/src/Util.hs): implements utility functions.
  * `combination`: returns all possible combinations of n elements from a list.

## Development

To run tests: `stack test`

To start the REPL: `stack ghci`

## Resources

* Shamir, Adi. [“How to Share a Secret.”](https://pdfs.semanticscholar.org/3144/5e3bd3672ed743c4a089cc0db4f23357f0f2.pdf) Commun. ACM 22 (1979): 612-613.
* Boneh, Dan, and Victor Shoup. [Principles of Modern Cryptography](https://crypto.stanford.edu/~dabo/cryptobook/draft_0_3.pdf). Vol. 0.3, 2016. 443-450.
