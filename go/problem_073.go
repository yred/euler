// Problem 73 - Counting fractions in a range
//
// Consider the fraction, n/d, where n and d are positive integers. If n<d and
// HCF(n,d)=1, it is called a reduced proper fraction.
//
// If we list the set of reduced proper fractions for d ≤ 8 in ascending order of
// size, we get:
//
//     1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
//     5/7, 3/4, 4/5, 5/6, 6/7, 7/8
//
// It can be seen that there are 3 fractions between 1/3 and 1/2.
//
// How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper
// fractions for d ≤ 12,000?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	for d := 4; d <= 12000; d++ {
		start := int(float64(d)/3) + 1
		limit := int(float64(d)/2) + 1

		for n := start; n < limit; n++ {
			if common.GCD(n, d) == 1 {
				count++
			}
		}
	}
	return
}
