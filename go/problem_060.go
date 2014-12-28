// Problem 60 - Prime pair sets
//
// The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
// and concatenating them in any order the result will always be prime. For
// example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four
// primes, 792, represents the lowest sum for a set of four primes with this
// property.
//
// Find the lowest sum for a set of five primes for which any two primes
// concatenate to produce another prime.
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
	target := 5

	sets := make([][]int, 0)

	for start, limit := 3, 1000; sum == 0; start, limit = limit, limit*2 {
		primes := common.PrimesUpTo(limit)

		maxPrime := limit * limit
		primeset := common.PrimeSetUpTo(maxPrime)

		startIx := len(primes) - 1
		for ix, p := range primes {
			if p >= start {
				startIx = ix
				break
			}
		}

		for _, p := range primes[startIx:] {
			newSets := [][]int{[]int{p}}

			for _, set := range sets {
				if tryCombining(set, p, primeset, maxPrime) {
					newSet := append(append([]int{}, set...), p)

					if len(newSet) == target {
						sum = common.Sum(newSet)
						break
					}

					newSets = append(newSets, newSet)
				}
			}

			if sum != 0 {
				break
			} else {
				sets = append(sets, newSets...)
			}
		}
	}

	return
}

func tryCombining(primes []int, prime int, primeset map[int]bool, maxPrimeset int) bool {
	pstrs, pstr := common.Strings(primes), strconv.Itoa(prime)

	for _, p := range pstrs {
		for _, n := range []int{Int(p + pstr), Int(pstr + p)} {
			if n < maxPrimeset {
				if _, isPrime := primeset[n]; !isPrime {
					return false
				}
			} else {
				if !common.IsPrime(n) {
					return false
				}
			}
		}
	}

	return true
}

func Int(s string) int {
	n, _ := strconv.Atoi(s)
	return n
}
