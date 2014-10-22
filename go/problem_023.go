// Problem 23 - Non-abundant sums
//
// A perfect number is a number for which the sum of its proper divisors is
// exactly equal to the number. For example, the sum of the proper divisors of 28
// would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
//
// A number n is called deficient if the sum of its proper divisors is less than n
// and it is called abundant if this sum exceeds n.
//
// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
// number that can be written as the sum of two abundant numbers is 24.
//
// By mathematical analysis, it can be shown that all integers greater than 28123
// can be written as the sum of two abundant numbers. However, this upper limit
// cannot be reduced any further by analysis even though it is known that the
// greatest number that cannot be expressed as the sum of two abundant numbers is
// less than this limit.
//
// Find the sum of all the positive integers which cannot be written as the sum of
// two abundant numbers.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sumNonAbundant int) {
	maxNonSum := 28123

	abundant := make(map[int]struct{}, 0)

	var empty struct{}

	for n := 1; n <= maxNonSum; n++ {
		if isAbundant(n) {
			abundant[n] = empty
		}

		if !isSum(n, abundant) {
			sumNonAbundant += n
		}
	}

	return
}

func isAbundant(n int) bool {
	divs := common.Divisors(n)
	return common.Sum(divs[:len(divs)-1]) > n
}

func isSum(n int, m map[int]struct{}) bool {
	for k := range m {
		if _, ok := m[n-k]; ok {
			return true
		}
	}

	return false
}
