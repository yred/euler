// Problem 51 - Prime digit replacements
//
// By replacing the 1st digit of the 2-digit number *3, it turns out that six of
// the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
//
// By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit
// number is the first example having seven primes among the ten generated
// numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and
// 56993. Consequently 56003, being the first member of this family, is the
// smallest prime with this property.
//
// Find the smallest prime which, by replacing part of the number (not necessarily
// adjacent digits) with the same digit, is part of an eight prime value
// family.
package main

import (
	"fmt"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (min int) {
	target := 8
	limit := 1000000

	primes := common.PrimesUpTo(limit)

	var primeIndex map[string][]int

	for start, end := 10, 100; min == 0 && end <= limit; start, end = end, end*10 {
		// Every "prime family" contains numbers with the same digit length, and
		// so a new/cleared map is required for every iteration
		primeIndex = make(map[string][]int)

		// `primes` is sorted, and so it's possible to remove no-longer valid
		// elements with a "drop while"
		primes = common.DropWhile(primes, lessThan(start))

		for _, p := range common.TakeWhile(primes, lessThan(end)) {
			pstr := strconv.Itoa(p)

			for _, positions := range digitPositions(p) {
				// Create multiple indexes on the current prime and current
				// digit (the discarded variable above, `_`)
				for count := 1; count <= len(positions); count++ {
					for _, pos := range common.Combinations(positions, count) {
						key := common.ReplacePositions(pstr, pos, '*')

						primeIndex[key] = append(primeIndex[key], p)
					}
				}
			}
		}

		for _, ps := range primeIndex {
			if len(ps) == target && (min == 0 || ps[0] < min) {
				min = ps[0]
			}
		}
	}

	return
}

// Returns a function that accepts an int and returns `true` for all inputs less
// than `n`
func lessThan(n int) func(int) bool {
	return func(a int) bool {
		return a < n
	}
}

// Returns a mapping between digits and their positions in the integer n, where
// 0 is the position of the leftmost digit
func digitPositions(n int) map[int][]int {
	positions := make(map[int][]int)

	for ix, d := range common.Digits(n) {
		positions[d] = append(positions[d], ix)
	}

	return positions
}
