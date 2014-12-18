// Problem 119 - Digit power sum
//
// The number 512 is interesting because it is equal to the sum of its digits
// raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a number
// with this property is 614656 = 28^4.
//
// We shall define a(n) to be the nth term of this sequence and insist that a
// number must contain at least two digits to have a sum.
//
// You are given that a(2) = 512 and a(10) = 614656.
//
// Find a(30).
package main

import (
	"fmt"
	"math/big"
	"sort"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (number string) {
	target := 30
	maxlen := target

	// Maximum sum of the digits of `maxlen`-digit numbers
	maxbase := maxlen * 9

	// Mapping between number lengths and valid digit power sum numbers
	dpSums := make(map[int][]string)

	for base := 2; base <= maxbase; base++ {
		bigbase := big.NewInt(int64(base))

		for exp := 2; ; exp++ {
			powStr := big.NewInt(0).Exp(bigbase, big.NewInt(int64(exp)), nil).String()

			if len(powStr) > maxlen {
				break
			}

			if common.SumDigits(powStr) == base {
				if _, ok := dpSums[len(powStr)]; ok {
					dpSums[len(powStr)] = append(dpSums[len(powStr)], powStr)
				} else {
					dpSums[len(powStr)] = []string{powStr}
				}
			}
		}
	}

	for length := 2; length <= maxlen; length++ {
		if numstrs, ok := dpSums[length]; ok {
			if len(numstrs) >= target {
				sort.Strings(numstrs)
				number = numstrs[target-1]
				break
			} else {
				target -= len(numstrs)
			}
		}
	}

	return
}
