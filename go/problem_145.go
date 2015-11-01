// Problem 145 - How many reversible numbers are there below one-billion?
//
// Some positive integers n have the property that the sum [ n + reverse(n) ]
// consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
// 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904
// are reversible. Leading zeroes are not allowed in either n or reverse(n).
//
// There are 120 reversible numbers below one-thousand.
//
// How many reversible numbers are there below one-billion (10^9)?
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	count := 0
	for n := 1; n < 1000000000; n += 2 {
		if allOdd(n + reverse(n)) {
			count += 2
		}
	}
	return count
}

func reverse(n int) int {
	r := 0
	for n > 0 {
		r *= 10
		r += n % 10
		n /= 10
	}
	return r
}

func allOdd(n int) bool {
	if n == 0 {
		return false
	}

	for n > 0 {
		if n%2 == 0 {
			return false
		}
		n /= 10
	}

	return true
}
