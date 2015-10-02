// Problem 113 - Non-bouncy numbers
//
// Working from left-to-right if no digit is exceeded by the digit to its left
// it is called an increasing number; for example, 134468.
//
// Similarly if no digit is exceeded by the digit to its right it is called a
// decreasing number; for example, 66420.
//
// We shall call a positive integer that is neither increasing nor decreasing a
// "bouncy" number; for example, 155349.
//
// As n increases, the proportion of bouncy numbers below n increases such that
// there are only 12951 numbers below one-million that are not bouncy and only
// 277032 non-bouncy numbers below 10^10.
//
// How many numbers below a googol (10^100) are not bouncy?
package main

import (
	"fmt"
)

type input struct {
	digits, length int
}

var memo = make(map[input]int64)

func main() {
	fmt.Println(solution())
}

func solution() (count int64) {
	maxLen := 100

	// Increasing numbers
	for length := 1; length <= maxLen; length++ {
		count += monotonic(9, length)
	}

	// Decreasing numbers
	for length := 1; length <= maxLen; length++ {
		// Subtract 1 for the `length`-digit 0
		count += monotonic(10, length) - 1
	}

	// "Constant" numbers are subtracted, since they've been double counted:
	// first with the increasing numbers, and again with the decreasing ones
	count -= 9 * int64(maxLen)

	return
}

// Return the number of integers of length `numLength`, with up to `digitCount`
// digits, all following a certain order (i.e., increasing or decreasing)
func monotonic(digitCount, numLength int) (count int64) {
	if res, ok := memo[input{digitCount, numLength}]; ok {
		return res
	}

	if numLength <= 0 {
		return 0
	} else if numLength == 1 {
		return int64(digitCount)
	} else if digitCount == 1 {
		return 1
	}

	// Account for the `numLength`-digit (sub-)number only containing the least
	// or greatest digit (the one that will be "skipped" in the pseudo-recursive
	// calls)
	count += 1

	for length := 1; length <= numLength; length++ {
		count += monotonic(digitCount-1, length)
	}

	memo[input{digitCount, numLength}] = count
	return
}
