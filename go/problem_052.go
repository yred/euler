// Problem 52 - Permuted multiples
//
// It can be seen that the number, 125874, and its double, 251748, contain exactly
// the same digits, but in a different order.
//
// Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
// contain the same digits.
package main

import (
	"fmt"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (x int) {
	maxMulti := 6
	digits := make(map[int]string)

	for n := 1; x == 0; n++ {
		if _, exists := digits[n]; !exists {
			digits[n] = common.SortString(strconv.Itoa(n))
		}

		for i := 2; i <= maxMulti; i++ {
			if _, exists := digits[i*n]; !exists {
				digits[i*n] = common.SortString(strconv.Itoa(i * n))
			}

			if digits[n] != digits[i*n] {
				break
			} else if i == maxMulti {
				// The smallest integer `x` has been found
				x = n
			}
		}
	}

	return
}
