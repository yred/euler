// Problem 32 - Pandigital products
//
// We shall say that an n-digit number is pandigital if it makes use of all the
// digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
// through 5 pandigital.
//
// The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
// multiplicand, multiplier, and product is 1 through 9 pandigital.
//
// Find the sum of all products whose multiplicand/multiplier/product identity
// can be written as a 1 through 9 pandigital.
//
// HINT: Some products can be obtained in more than one way so be sure to only
// include it once in your sum.
package main

import (
	"fmt"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	products := make(map[int]bool)

	// Pre-convert all 1 through 4 digit numbers to strings
	str := make(map[int]string)
	for i := 1; i < 10000; i++ {
		str[i] = strconv.Itoa(i)
	}

	// Uses the fact that the products must be 4-digits long
	startPairs := map[int]int{1: 1000, 10: 100, 100: 100}

	for left, right := range startPairs {
		for a := left; a < 10*left; a++ {
			for b := right; b < 10*right; b++ {
				if a*b >= 10000 {
					continue
				}

				if common.SortString(str[a]+str[b]+str[a*b]) == "123456789" {
					if _, ok := products[a*b]; !ok {
						products[a*b] = true
						sum += a * b
					}
				}
			}
		}
	}

	return
}
