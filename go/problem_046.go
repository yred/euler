// Problem 46 - Goldbach's other conjecture
//
// It was proposed by Christian Goldbach that every odd composite number can be
// written as the sum of a prime and twice a square.
//
//     9  = 7  + 2 × 1^2
//     15 = 7  + 2 × 2^2
//     21 = 3  + 2 × 3^2
//     25 = 7  + 2 × 3^2
//     27 = 19 + 2 × 2^2
//     33 = 31 + 2 × 1^2
//
// It turns out that the conjecture was false.
//
// What is the smallest odd composite that cannot be written as the sum of a prime
// and twice a square?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (a int) {
	limit := 1000000

	primes := common.PrimesUpTo(limit)

	// Build 2 initial sets/maps for easier prime and square lookups
	primeset := make(map[int]bool)
	for _, p := range primes {
		primeset[p] = true
	}

	squareset := make(map[int]bool)
	for n := 1; n*n < limit; n++ {
		squareset[n*n] = true
	}

	for n := 9; ; n += 2 {
		// Only odd composite numbers are relevant
		if _, isPrime := primeset[n]; isPrime {
			continue
		}

		for _, p := range primes {
			// If the conjecture hasn't been met for any prime less than `n`,
			// then it has been proven false
			if p > n {
				a = n
				break
			}

			// Check if the conjecture holds true for `n` with the current prime
			if _, ok := squareset[(n-p)/2]; ok {
				break
			}
		}

		if a > 0 {
			break
		}
	}

	return
}
