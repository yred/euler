// Problem 34 - Digit factorials
//
// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
//
// Find the sum of all numbers which are equal to the sum of the factorial of
// their digits.
//
// Note: as 1! = 1 and 2! = 2 are not sums they are not included.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	// Precompute digit factorials
	factorials := make([]int, 10)
	for i := 0; i < 10; i++ {
		factorials[i] = int(common.Factorial(i).Int64())
	}

	maxLen := 7
	maxNum := factorials[9] * maxLen

	factorialSum := 0
	for n := 10; n < maxNum; n++ {
		sum := 0
		for _, d := range common.Digits(n) {
			sum += factorials[d]
		}
		if n == sum {
			factorialSum += n
		}
	}

	return factorialSum
}
