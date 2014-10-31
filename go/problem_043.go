// Problem 43 - Sub-string divisibility
//
// The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
// each of the digits 0 to 9 in some order, but it also has a rather interesting
// sub-string divisibility property.
//
// Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
// the following:
//
//     d2d3d4  = 406 is divisible by 2
//     d3d4d5  = 063 is divisible by 3
//     d4d5d6  = 635 is divisible by 5
//     d5d6d7  = 357 is divisible by 7
//     d6d7d8  = 572 is divisible by 11
//     d7d8d9  = 728 is divisible by 13
//     d8d9d10 = 289 is divisible by 17
//
// Find the sum of all 0 to 9 pandigital numbers with this property.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int64) {
	divisors := []int{2, 3, 5, 7, 11, 13, 17}

	for _, p := range common.Permutations([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, 10) {
		// If the permutation starts with 0, the corresponding number is only 9
		// digits long
		if p[0] == 0 {
			continue
		}

		match := true
		for i, j := 0, 1; i < len(divisors); i, j = i+1, j+1 {
			if common.Integer(p[j:j+3])%divisors[i] != 0 {
				match = false
				break
			}
		}

		if match {
			sum += int64(common.Integer(p))
		}
	}

	return
}
