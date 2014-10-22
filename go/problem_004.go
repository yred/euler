// Problem 4 - Largest palindrome product
//
// A palindromic number reads the same both ways. The largest palindrome made
// from the product of two 2-digit numbers is 9009 = 91 × 99.
//
// Find the largest palindrome made from the product of two 3-digit numbers.

package main

import (
	"fmt"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (max int) {
	for i := 100; i < 1000; i++ {
		for j := 100; j < 1000; j++ {
			if max < i*j && common.IsPalindrome(strconv.Itoa(i*j)) {
				max = i * j
			}
		}
	}
	return
}
