// Problem 31 - Coin sums
//
// In England the currency is made up of pound, £, and pence, p, and there are
// eight coins in general circulation:
//
//     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
//
// It is possible to make £2 in the following way:
//
//     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
//
// How many different ways can £2 be made using any number of coins?
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	return combinations(200, []int{200, 100, 50, 20, 10, 5, 2, 1})
}

// Returns the number of ways in which `total` amount can be produced using
// coins in `rscoins` (reverse-sorted coins -- i.e., sorted in decreasing order)
func combinations(total int, rscoins []int) (count int) {

	// All but the last coin
	for ix, largest := range rscoins[:len(rscoins)-1] {
		amount := total
		for largest <= amount {
			amount -= largest
			count += combinations(amount, rscoins[ix+1:])
		}
	}

	// Last coin and 0-totals
	if total%rscoins[len(rscoins)-1] == 0 {
		count++
	}

	return
}
