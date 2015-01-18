// Problem 95 - Amicable chains
//
// The proper divisors of a number are all the divisors excluding the number
// itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the
// sum of these divisors is equal to 28, we call it a perfect number.
//
// Interestingly the sum of the proper divisors of 220 is 284 and the sum of the
// proper divisors of 284 is 220, forming a chain of two numbers. For this reason,
// 220 and 284 are called an amicable pair.
//
// Perhaps less well known are longer chains. For example, starting with 12496, we
// form a chain of five numbers:
//
//             12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
//
// Since this chain returns to its starting point, it is called an amicable chain.
//
// Find the smallest member of the longest amicable chain with no element
// exceeding one million.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	limit := 1000000
	sums := make(map[int]int)

	amicables := make(map[int][]int)

	for n := 1; n <= limit; n++ {
		if _, exists := sums[n]; exists {
			continue
		}

		sd := sumDivs(n)
		sums[n] = sd

		if n < sd {
			chain := []int{n}
			for last := 1; n < sd && sd < limit; last++ {
				chain = append(chain, sd)

				sd = sumDivs(chain[last])
				sums[chain[last]] = sd

				if ix := indexOf(chain, sd); ix >= 0 {
					chain = chain[ix:]
					amicables[common.Min(chain...)] = chain
				}
			}

			if n == sd {
				amicables[n] = chain
			}
		}
	}

	maxLen, maxEl := 0, 0
	for key, chain := range amicables {
		if len(chain) > maxLen {
			maxLen, maxEl = len(chain), key
		}
	}

	return maxEl
}

func sumDivs(n int) (sum int) {
	divs := common.Divisors(n)
	for _, d := range divs[:len(divs)-1] {
		sum += d
	}
	return
}

func indexOf(ns []int, n int) int {
	for ix, el := range ns {
		if n == el {
			return ix
		}
	}
	return -1
}
