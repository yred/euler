// Problem 35 - Circular primes
//
// The number, 197, is called a circular prime because all rotations of the
// digits: 197, 971, and 719, are themselves prime.
//
// There are thirteen such primes below 100:
//
//     2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
//
// How many circular primes are there below one million?
package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	primes := common.PrimeSetUpTo(int(math.Pow10(6)))

	circulars := make(map[int]bool)
	for p, check := range primes {
		if !check {
			continue
		}

		if p < 10 {
			circulars[p] = true
			continue
		}

		if strings.ContainsAny(strconv.Itoa(p), "245680") {
			continue
		}

		rs := rotations(p)
		circular := common.AllInt(rs, func(r int) bool { _, ok := primes[r]; return ok })

		for _, r := range rs {
			if circular {
				circulars[r] = true
			}
			primes[r] = false
		}
	}

	return len(circulars)
}

func rotations(p int) []int {
	length := int(math.Floor(math.Log10(float64(p)))) + 1

	powerOf10 := int(math.Pow10(length - 1))

	elements := make([]int, length)
	for i := 0; i < length; i++ {
		lastDigit := p % 10
		p = lastDigit*powerOf10 + p/10
		elements[i] = p
	}

	return elements
}
