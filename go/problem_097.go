// Problem 97 - Large non-Mersenne prime
//
// The first known prime found to exceed one million digits was discovered in
// 1999, and is a Mersenne prime of the form 2^6972593 − 1; it contains exactly
// 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p − 1, have
// been found which contain more digits.
//
// However, in 2004 there was found a massive non-Mersenne prime which contains
// 2,357,207 digits: 28433 × 2^7830457 + 1.
//
// Find the last ten digits of this prime number.
package main

import (
	"fmt"
	"math"
	"math/big"
)

func main() {
	fmt.Println(solution())
}

func solution() int64 {
	modulus := bigI(int64(math.Pow10(10)))

	coeff, base, exp := bigI(28433), bigI(2), bigI(7830457)
	product := big.NewInt(0).Mul(coeff, bigI(0).Exp(base, exp, modulus))

	return product.Add(product, bigI(1)).Mod(product, modulus).Int64()
}

func bigI(n int64) *big.Int {
	return big.NewInt(n)
}
