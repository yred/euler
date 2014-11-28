// Problem 92 - Square digit chains
//
// A number chain is created by continuously adding the square of the digits in a
// number to form a new number until it has been seen before.
//
// For example,
//
//         44 → 32 → 13 → 10 → 1 → 1
//         85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
//
// Therefore any chain that arrives at 1 or 89 will become stuck in an endless
// loop. What is most amazing is that EVERY starting number will eventually arrive
// at 1 or 89.
//
// How many starting numbers below ten million will arrive at 89?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	limit := 10000000

	chainTo1 := map[int]bool{1: true}
	chainTo89 := map[int]bool{89: true}

	for n := 1; n < limit; n++ {
		current := n

		chain := make([]int, 0)

		for {
			chain = append(chain, current)

			if _, ok := chainTo89[current]; ok {
				addToSet(chain, chainTo89)
				count++
				break
			}

			if _, ok := chainTo1[current]; ok {
				addToSet(chain, chainTo1)
				break
			}

			current = sumDigitSquares(current)
		}
	}

	return
}

func sumDigitSquares(n int) (sum int) {
	for _, d := range common.Digits(n) {
		sum += d * d
	}

	return
}

func addToSet(ns []int, set map[int]bool) {
	for _, n := range ns {
		set[n] = true
	}
}
