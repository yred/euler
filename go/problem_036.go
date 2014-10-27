// Problem 36 - Double-base palindromes
//
// The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
//
// Find the sum of all numbers, less than one million, which are palindromic in
// base 10 and base 2.
//
// (Please note that the palindromic number, in either base, may not include
// leading zeros.)
package main

import (
	"fmt"
	"math"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	for _, n := range palindromesUpToLength(6) {
		if common.IsPalindrome(fmt.Sprintf("%b", n)) {
			sum += n
		}
	}

	return
}

func palindromesUpToLength(maxLength int) []int {
	count := int(math.Pow10((maxLength+1)/2)) - 1
	if maxLength%2 == 0 {
		count *= 2
	} else {
		count += count / 10
	}

	palindromes := make([]int, count)
	for ix, length := 0, 1; length <= maxLength; length++ {
		start := int(math.Pow10((length - 1) / 2))
		limit := start * 10

		for n := start; n < limit; n, ix = n+1, ix+1 {
			if length%2 == 1 {
				palindromes[ix] = n*start + reverse(n/10)
			} else {
				palindromes[ix] = n*limit + reverse(n)
			}
		}
	}

	return palindromes
}

func reverse(n int) int {
	reversed, _ := strconv.Atoi(common.ReverseString(strconv.Itoa(n)))
	return reversed
}
