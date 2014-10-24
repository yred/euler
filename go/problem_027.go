// Problem 27 - Quadratic primes
//
// Euler discovered the remarkable quadratic formula:
//
//         n² + n + 41
//
// It turns out that the formula will produce 40 primes for the consecutive values
// n = 0 to 39. However, when n = 40, 40² + 40 + 41 = 40(40 + 1) + 41 is divisible
// by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
//
// The incredible formula  n² − 79n + 1601 was discovered, which produces 80
// primes for the consecutive values n = 0 to 79. The product of the coefficients,
// −79 and 1601, is −126479.
//
// Considering quadratics of the form:
//
//     n² + an + b, where |a| < 1000 and |b| < 1000
//
//     where |n| is the modulus/absolute value of n
//     e.g. |11| = 11 and |−4| = 4
//
// Find the product of the coefficients, a and b, for the quadratic expression
// that produces the maximum number of primes for consecutive values of n,
// starting with n = 0.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	// "Set" of all primes up to 2e6, an upper bound on n² + an + b
	// for |a| < 1000 and |b| < 1000
	pset := common.PrimeSetUpTo(2000000)

	bestA, bestB, maxLen := 0, 0, 0

	for _, b := range common.PrimesUpTo(1000) {
		// for n = 1:   n² + an + b < 2 (min prime)   if a + b < 1
		for a := -b + 1; a <= 999; a++ {
			// If the current (a, b) pair doesn't improve on the current best
			// solution, move to the next pair
			if _, ok := pset[maxLen*maxLen+a*maxLen+b]; !ok {
				continue
			}

			for n := 1; ; n++ {
				if _, ok := pset[n*n+a*n+b]; !ok {
					if n > maxLen {
						bestA, bestB, maxLen = a, b, n
					}
					break
				}
			}
		}
	}

	return bestA * bestB
}
