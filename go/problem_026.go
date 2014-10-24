// Problem 26 - Reciprocal cycles
//
// A unit fraction contains 1 in the numerator. The decimal representation of the
// unit fractions with denominators 2 to 10 are given:
//
//     1/2  =   0.5
//     1/3  =   0.(3)
//     1/4  =   0.25
//     1/5  =   0.2
//     1/6  =   0.1(6)
//     1/7  =   0.(142857)
//     1/8  =   0.125
//     1/9  =   0.(1)
//     1/10 =   0.1
//
// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
// seen that 1/7 has a 6-digit recurring cycle.
//
// Find the value of d < 1000 for which 1/d contains the longest recurring cycle
// in its decimal fraction part.
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	d, maxLen := 1, 0

	for i := 2; i < 1000; i++ {
		if l := invCycLen(i); l > maxLen {
			d, maxLen = i, l
		}
	}

	return d
}

// Returns the length of the cycle of `n`'s decimal inverse, or 0 if no such
// cycle exists
func invCycLen(n int) (length int) {
	ds := make(map[int]int)

	dividend := 1

	for l := 0; ; l++ {
		if index, ok := ds[dividend]; ok {
			length = l - index
			break
		} else {
			ds[dividend] = l
		}

		for dividend < n {
			dividend *= 10
		}

		// The inverse is "finite"
		if dividend%n == 0 {
			break
		}

		dividend = dividend % n
	}

	return
}
