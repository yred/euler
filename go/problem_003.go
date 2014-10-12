// Problem 3 - Largest prime factor
//
// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143?

package main

import (
	"fmt"
	"math"

	"./common"
)

func main() {
	fmt.Println(Solution())
}

func Solution() (prime int) {
	n := 600851475143

	primes := common.PrimesUpTo(int(math.Sqrt(float64(n))))

	for _, p := range primes {
		if n%p == 0 {
			for n%p == 0 {
				n /= p
			}

			if n == 1 {
				prime = p
				break
			}
		}
	}

	return
}
