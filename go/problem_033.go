// Problem 33 - Digit canceling fractions
//
// The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
// attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
// correct, is obtained by cancelling the 9s.
//
// We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
//
// There are exactly four non-trivial examples of this type of fraction, less than
// one in value, and containing two digits in the numerator and denominator.
//
// If the product of these four fractions is given in its lowest common terms,
// find the value of the denominator.
package main

import (
	"fmt"
	"math/big"
)

func main() {
	fmt.Println(solution())
}

func solution() int64 {
	product := big.NewRat(1, 1)

	for denom := 11; denom < 100; denom++ {
		for numer := 10; numer < denom; numer++ {
			if denom%10 == 0 || numer%10 == 0 {
				continue
			}

			r := big.NewRat(int64(numer), int64(denom))
			if len(r.String()) > 3 {
				continue
			}

			// By deduction, the only valid case is when the numerator's last
			// digit is the denominator's first
			if numer%10 == denom/10 && r.Cmp(big.NewRat(int64(numer/10), int64(denom%10))) == 0 {
				product.Mul(product, r)
			}
		}
	}

	return product.Denom().Int64()
}
