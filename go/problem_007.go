// Problem 7 - 10001st prime
//
// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
// that the 6th prime is 13.
//
// What is the 10001st prime number?

package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	// Index of the 10001st prime in a 0-indexed sequence
	var target = 10000

	var primes []int

	for max := target; ; max *= 10 {
		primes = common.PrimesUpTo(max)

		if len(primes) >= target {
			break
		}
	}

	return primes[target]
}
