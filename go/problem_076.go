// Problem 76 - Counting summations
//
// It is possible to write five as a sum in exactly six different ways:
//
//         4 + 1
//         3 + 2
//         3 + 1 + 1
//         2 + 2 + 1
//         2 + 1 + 1 + 1
//         1 + 1 + 1 + 1 + 1
//
// How many different ways can one hundred be written as a sum of at least two
// positive integers?
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	target := 100

	sums := map[int][]int{1: []int{0, 1}}

	for a := 2; a <= target; a++ {
		sums[a] = []int{0}

		for largest := 1; largest < a; largest++ {
			delta := a - largest

			if largest <= delta {
				sums[a] = append(sums[a], sums[a][largest-1]+sums[delta][largest])
			} else {
				sums[a] = append(sums[a], sums[a][largest-1]+sums[delta][delta])
			}

		}

		sums[a] = append(sums[a], sums[a][a-1]+1)
	}

	return sums[target][target] - 1
}
