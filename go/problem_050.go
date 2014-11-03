// Problem 50 - Consecutive prime sum
//
// The prime 41, can be written as the sum of six consecutive primes:
//
//         41 = 2 + 3 + 5 + 7 + 11 + 13
//
// This is the longest sum of consecutive primes that adds to a prime below 100.
//
// The longest sum of consecutive primes below 1000 that adds to a prime, contains
// 21 terms, and is equal to 953.
//
// Which prime, below 1 million, can be written as the sum of the most consecutive
// primes?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	limit := 1000000

	primes := common.PrimesUpTo(limit)

	// Pre-compute a prime set/map for simpler/faster primality checks
	primeset := make(map[int]bool)
	for _, p := range primes {
		primeset[p] = true
	}

	maxLength, maxPrime, firstValid := 0, 0, 0

	sums := make([]int, len(primes))
	for ix, p := range primes {
		for i := firstValid; i <= ix; i++ {
			sums[i] += p

			// Stop tracking the consecutive sum at index `i` once it reaches
			// `limit`
			if sums[i] >= limit {
				firstValid = i + 1
				continue
			}

			// Check if the current best length has been surpassed
			if ix-i+1 > maxLength {
				if _, isPrime := primeset[sums[i]]; isPrime {
					maxLength, maxPrime = ix-i+1, sums[i]
				}
			}
		}
	}

	return maxPrime
}
