// Problem 70 - Totient permutation
//
// Euler's Totient function, φ(n) [sometimes called the phi function], is used to
// determine the number of positive numbers less than or equal to n which are
// relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than
// nine and relatively prime to nine, φ(9) = 6.
//
// The number 1 is considered to be relatively prime to every positive number, so
// φ(1) = 1.
//
// Interestingly, φ(87109) = 79180, and it can be seen that 87109 is a permutation
// of 79180.
//
// Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the
// ratio n/φ(n) produces a minimum.
package main

import (
	"fmt"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	limit := 10000000 - 1

	// Although 1/φ(1) = 1, 1 wouldn't be a suitable minimal initial value
	nmin, min := 1, float64(10)

	for n, totient := range common.Totient(2, limit, nil) {
		if isPermutation(n, totient) {
			if ratio := float64(n) / float64(totient); ratio < min {
				nmin, min = n, ratio
			}
		}
	}

	return nmin
}

func isPermutation(a, b int) bool {
	astr, bstr := strconv.Itoa(a), strconv.Itoa(b)
	return len(astr) == len(bstr) && common.SortString(astr) == common.SortString(bstr)
}
