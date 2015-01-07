// Problem 100 - Arranged probability
//
// If a box contains twenty-one coloured discs, composed of fifteen blue discs and
// six red discs, and two discs were taken at random, it can be seen that the
// probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
//
// The next such arrangement, for which there is exactly 50% chance of taking two
// blue discs at random, is a box containing eighty-five blue discs and
// thirty-five red discs.
//
// By finding the first arrangement to contain over 10^12 = 1,000,000,000,000
// discs in total, determine the number of blue discs that the box would contain.
package main

import (
	"fmt"
	"math/big"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (blue int64) {
	// Threshold of 1,000,000,000,000
	threshold := bigI(0).Exp(bigI(10), bigI(12), nil)

	// Note: Exactly the same as the Python solution
	//
	// The probability equation is:
	//
	//              (blue/combined) * ((blue - 1)/(combined - 1)) = 1/2
	//
	// By replacing blue by x and combined by y, the above equation can be
	// rewritten as:
	//
	//               2*x^2 - y^2 - 2*x + y = 0
	//
	// which is a binary quadratic Diophatine equation. By using Lagrange's
	// method for solving the general case, and by simplifying, we obtain the
	// following Diophantine equation:
	//
	//               X^2 - 8*Y^2 = -4
	//
	// where X = 4*y - 2 and Y = 2*x - 1
	//
	for start, limit := 0, 10; blue == 0; start, limit = limit, 2*limit {
		for _, sol := range common.DiophantineSolutions(8, -4, limit)[start:] {
			bigX := sol.Fst
			if bigI(0).Div(bigX.Add(bigX, bigI(2)), bigI(4)).Cmp(threshold) > 0 {
				bigY := sol.Snd
				blue = bigI(0).Div(bigY.Add(bigY, bigI(1)), bigI(2)).Int64()
				break
			}
		}
	}

	return
}

func bigI(n int) *big.Int {
	return big.NewInt(int64(n))
}
