// Problem 30 - Digit fifth powers
//
// Surprisingly there are only three numbers that can be written as the sum of
// fourth powers of their digits:
//
//         1634 = 1^4 + 6^4 + 3^4 + 4^4
//         8208 = 8^4 + 2^4 + 0^4 + 8^4
//         9474 = 9^4 + 4^4 + 7^4 + 4^4
//
// As 1 = 1^4 is not a sum it is not included.
//
// The sum of these numbers is 1634 + 8208 + 9474 = 19316.
//
// Find the sum of all the numbers that can be written as the sum of fifth powers
// of their digits.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	maxLen := 6
	maxNum := 9 * 9 * 9 * 9 * 9 * maxLen

	powerSum := 0

	for n := 10; n <= maxNum; n++ {
		sum := 0
		for _, d := range common.Digits(n) {
			sum += d * d * d * d * d
		}
		if n == sum {
			powerSum += n
		}
	}

	return powerSum
}
