// Problem 56 - Powerful digit sum
//
// A googol (10^100) is a massive number: one followed by one-hundred zeros;
// 100^100 is almost unimaginably large: one followed by two-hundred zeros.
// Despite their size, the sum of the digits in each number is only 1.
//
// Considering natural numbers of the form, a^b, where a, b < 100, what is the
// maximum digital sum?
package main

import (
	"fmt"
	"math/big"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (max int) {

	for a := 2; a < 100; a++ {
		bigA := big.NewInt(int64(a))

		for b := 2; b < 100; b++ {
			bigB := big.NewInt(int64(b))

			sum := common.SumDigits(bigB.Exp(bigA, bigB, nil).String())
			if sum > max {
				max = sum
			}
		}
	}

	return
}
