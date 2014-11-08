// Problem 63 - Powerful digit counts
//
// The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit
// number, 134217728=8^9, is a ninth power.
//
// How many n-digit positive integers exist which are also an nth power?
package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	var newPowers int

	for exponent := float64(1); ; exponent++ {
		max := math.Pow(10, exponent)

		for base := float64(1); ; base++ {
			power := math.Pow(base, exponent)

			if power >= max {
				break
			}

			if power >= max/10 {
				newPowers++
			}
		}

		if newPowers == 0 {
			break
		} else {
			count += newPowers
			newPowers = 0
		}
	}

	return
}
