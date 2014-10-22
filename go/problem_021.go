// Problem 21 - Amicable numbers
//
// Let d(n) be defined as the sum of proper divisors of n (numbers less than n
// which divide evenly into n).
//
// If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
// each of a and b are called amicable numbers.
//
// For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
// and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
// 142; so d(284) = 220.
//
// Evaluate the sum of all the amicable numbers under 10000.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sumAmicables int) {
	sumdivs := make(map[int]int)

	for a := 1; a < 10000; a++ {
		b := d(a)

		if b > 0 && b != a {
			// Avoid counting numbers twice
			if _, ok := sumdivs[b]; !ok {
				if d(b) == a {
					sumAmicables += a + b
				}
			}
		}

		sumdivs[a] = b
	}

	return
}

func d(n int) int {
	divs := common.Divisors(n)
	return common.Sum(divs[:len(divs)-1])
}
