// Problem 10 - Summation of primes
//
// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
// Find the sum of all the primes below two million.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(Solution())
}

func Solution() (sum int64) {
	for _, p := range common.PrimesUpTo(2000000) {
		sum += int64(p)
	}

	return
}
