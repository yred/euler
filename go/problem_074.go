// Problem 74 - Digit factorial chains
//
// The number 145 is well known for the property that the sum of the factorial of
// its digits is equal to 145:
//
//         1! + 4! + 5! = 1 + 24 + 120 = 145
//
// Perhaps less well known is 169, in that it produces the longest chain of
// numbers that link back to 169; it turns out that there are only three such
// loops that exist:
//
//         169 → 363601 → 1454 → 169
//         871 → 45361 → 871
//         872 → 45362 → 872
//
// It is not difficult to prove that EVERY starting number will eventually get
// stuck in a loop. For example,
//
//         69 → 363600 → 1454 → 169 → 363601 (→ 1454)
//         78 → 45360 → 871 → 45361 (→ 871)
//         540 → 145 (→ 145)
//
// Starting with 69 produces a chain of five non-repeating terms, but the longest
// non-repeating chain with a starting number below one million is sixty terms.
//
// How many chains, with a starting number below one million, contain exactly
// sixty non-repeating terms?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	chains := make(map[int]int)

	for n := 1; n <= 1000000; n++ {
		if _, ok := chains[n]; ok {
			continue
		}

		path := make([]int, 0)
		var cur int
		for cur = n; indexOf(cur, path) == -1; cur = sumDigitFactorials(cur) {
			path = append(path, cur)
		}

		length := len(path)
		start := indexOf(cur, path)

		for ix, val := range path {
			if _, ok := chains[val]; ok {
				continue
			}

			var chainLen int
			if ix <= start {
				chainLen = length - ix
			} else {
				chainLen = length - start
			}

			if chainLen == 60 {
				count++
			}

			chains[val] = chainLen
		}
	}

	return
}

func indexOf(n int, ns []int) int {
	for ix, value := range ns {
		if n == value {
			return ix
		}
	}
	return -1
}

func factorial(n int) int {
	if n == 0 || n == 1 {
		return 1
	}
	return n * factorial(n-1)
}

func sumDigitFactorials(n int) (sum int) {
	for _, d := range common.Digits(n) {
		sum += factorial(d)
	}
	return
}
