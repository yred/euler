// Problem 47 - Distinct primes factors
//
// The first two consecutive numbers to have two distinct prime factors are:
//
//         14 = 2 × 7
//         15 = 3 × 5
//
// The first three consecutive numbers to have three distinct prime factors are:
//
//         644 = 2² × 7 × 23
//         645 = 3 × 5 × 43
//         646 = 2 × 17 × 19.
//
// Find the first four consecutive integers to have four distinct prime factors.
// What is the first of these numbers?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (first int) {
	target := 4

	limit := 1000000
	primes := common.PrimesUpTo(limit)

	// Build a prime set/map for simpler primality checks
	primeset := make(map[int]bool)
	for _, p := range primes {
		primeset[p] = true
	}

	consecutive := 0
	for n := 1; ; n++ {
		if len(factors(n, primes, primeset)) != target {
			consecutive = 0
			continue
		}

		consecutive++
		if consecutive == target {
			first = n - target + 1
			break
		}
	}

	return
}

func factors(n int, primes []int, primeset map[int]bool) []int {
	fs := make([]int, 0)

	for _, p := range primes {
		if _, isPrime := primeset[n]; isPrime {
			fs = append(fs, n)
			break
		}

		if n%p == 0 {
			fs = append(fs, p)
			for n%p == 0 {
				n /= p
			}
		}
	}

	return fs
}
