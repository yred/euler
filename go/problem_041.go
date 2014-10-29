// Problem 41 - Pandigital prime
//
// We shall say that an n-digit number is pandigital if it makes use of all the
// digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
// also prime.
//
// What is the largest n-digit pandigital prime that exists?
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

func solution() (pandigital int) {
	// The largest valid pandigital primes are at most 7 digits long (8 and 9-digit
	// pandigitals are divisible by 3)
	primes := common.PrimesUpTo(10000000)

	for i := len(primes) - 1; i >= 0; i-- {
		if strings.HasPrefix("1234567", common.SortString(strconv.Itoa(primes[i]))) {
			pandigital = primes[i]
			break
		}
	}

	return
}
