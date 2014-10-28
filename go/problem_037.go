// Problem 37 - Truncatable primes
//
// The number 3797 has an interesting property. Being prime itself, it is possible
// to continuously remove digits from left to right, and remain prime at each
// stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
// 379, 37, and 3.
//
// Find the sum of the only eleven primes that are both truncatable from left to
// right and right to left.
//
// NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
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

func solution() (sum int) {
	primes := common.PrimeSetUpTo(1000000)

	for p := range primes {
		if p < 10 {
			continue
		}

		pstr := strconv.Itoa(p)
		if strings.Contains("14689", pstr[:1]) || strings.ContainsAny(pstr[1:], "024568") {
			continue
		}

		isPrime := func(n int) bool { _, ok := primes[n]; return ok }

		if !common.AllInt(dropFromLeft(p), isPrime) {
			continue
		}

		if !common.AllInt(dropFromRight(p), isPrime) {
			continue
		}

		sum += p
	}

	return
}

func dropFromLeft(p int) []int {
	pstr := strconv.Itoa(p)

	partials := make([]int, len(pstr)-1)

	for ix := 0; ix < len(pstr)-1; ix++ {
		partials[ix], _ = strconv.Atoi(pstr[ix+1:])
	}

	return partials
}

func dropFromRight(p int) []int {
	partials := make([]int, 0)

	for p >= 10 {
		p /= 10
		partials = append(partials, p)
	}

	return partials
}
