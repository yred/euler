// Problem 87 - Prime power triples
//
// The smallest number expressible as the sum of a prime square, prime cube, and
// prime fourth power is 28. In fact, there are exactly four numbers below fifty
// that can be expressed in such a way:
//
//             28 = 2^2 + 2^3 + 2^4
//             33 = 3^2 + 2^3 + 2^4
//             49 = 5^2 + 2^3 + 2^4
//             47 = 2^2 + 3^3 + 2^4
//
// How many numbers below fifty million can be expressed as the sum of a prime
// square, prime cube, and prime fourth power?
package main

import (
	"fmt"
	"math"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	limit := 50 * 1000 * 1000

	primes := common.PrimesUpTo(int(math.Sqrt(float64(limit))))

	primes2 := powersUpTo(primes, 2, limit)
	primes3 := powersUpTo(primes, 3, limit)
	primes4 := powersUpTo(primes, 4, limit)

	sums := make(map[int]bool)

	for _, p2 := range primes2 {
		for _, p3 := range primes3 {
			s1 := p2 + p3
			if s1 >= limit {
				break
			}

			for _, p4 := range primes4 {
				sum := s1 + p4
				if sum >= limit {
					break
				}

				sums[sum] = true
			}
		}
	}

	return len(sums)
}

func powersUpTo(ns []int, exponent, limit int) []int {
	exp := float64(exponent)
	powers := make([]int, 0)

	for _, n := range ns {
		power := int(math.Pow(float64(n), exp))
		if power >= limit {
			break
		}
		powers = append(powers, power)
	}

	return powers
}
