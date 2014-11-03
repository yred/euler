// Problem 49 - Prime permutations
//
// The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
// by 3330, is unusual in two ways:
//
//     (i) each of the three terms are prime, and,
//     (ii) each of the 4-digit numbers are permutations of one another.
//
// There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
// exhibiting this property, but there is one other 4-digit increasing sequence.
//
// What 12-digit number do you form by concatenating the three terms in this
// sequence?
package main

import (
	"fmt"
	"strconv"
	"strings"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int64 {
	primes := common.PrimesUpTo(10000)

	pmap := make(map[string][]int)
	for _, p := range primes {
		if p < 1000 {
			continue
		}

		key := common.SortString(strconv.Itoa(p))
		pmap[key] = append(pmap[key], p)
	}

	var pseq []int
	for _, ps := range pmap {
		if len(ps) >= 3 && ps[0] != 1487 {
			for _, c := range common.Combinations(ps, 3) {
				if c[2]-c[1] == c[1]-c[0] {
					pseq = c
					break
				}
			}
		}
		if pseq != nil {
			break
		}
	}

	n, _ := strconv.ParseInt(strings.Join(common.Strings(pseq), ""), 10, 64)
	return n
}
