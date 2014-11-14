// Problem 57 - Square root convergents
//
// It is possible to show that the square root of 2 can be expressed as an
// infinite continued fraction.
//
//         √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
//
// By expanding this for the first 4 iterations, we get:
//
//         1 + 1/2 = 3/2 = 1.5
//         1 + 1/(2 + 1/2) = 7/5 = 1.4
//         1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
//         1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
//
// The next 3 expansions are 99/70, 239/169, and 577/408, but the 8th expansion,
// 1393/985, is the first example where the number of digits in the numerator
// exceeds the number of digits in the denominator.
//
// In the first one-thousand expansions, how many fractions contain a numerator
// with more digits than denominator?
package main

import (
	"fmt"
	"math/big"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	one, two := big.NewRat(1, 1), big.NewRat(2, 1)

	for idx, denom := 2, big.NewRat(1, 2); idx <= 1000; idx++ {
		denom.Add(two, denom)

		// If the numerator contains more digits than the denominator, the length
		// of the current expansion's string representation must be even
		if current := big.NewRat(1, 1).Add(one, denom.Inv(denom)); len(current.String())%2 == 0 {
			count++
		}
	}

	return
}
