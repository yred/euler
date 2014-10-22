// Problem 24 - Lexicographic permutations
//
// A permutation is an ordered arrangement of objects. For example, 3124 is one
// possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
// are listed numerically or alphabetically, we call it lexicographic order. The
// lexicographic permutations of 0, 1 and 2 are:
//
//     012   021   102   120   201   210
//
// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,
// 6, 7, 8 and 9?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (permutation string) {
	// Index of the millionth element in a 0-indexed sequence
	target := 999999

	sortedElems := "0123456789"

	base := make([]int, len(sortedElems)-1)
	for i, count := 0, len(sortedElems)-1; count > 0; i, count = i+1, count-1 {
		base[i] = int(common.Factorial(count).Int64())
	}

	for _, b := range base {
		quo, rem := target/b, target%b

		permutation += string(sortedElems[quo])

		if quo < len(sortedElems)-1 {
			sortedElems = sortedElems[:quo] + sortedElems[quo+1:]
		} else {
			sortedElems = sortedElems[:quo]
		}

		if rem == 0 {
			permutation += sortedElems
			return
		}

		target = rem
	}

	return
}
