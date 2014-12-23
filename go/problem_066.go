// Problem 66 - Diophantine equation
//
// Consider quadratic Diophantine equations of the form:
//
//         x^2 – D*y^2 = 1
//
// For example, when D = 13, the minimal solution in x is 649^2 – 13×180^2 = 1.
//
// It can be assumed that there are no solutions in positive integers when D is
// square.
//
// By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the
// following:
//
//         3^2 – 2×2^2 = 1
//         2^2 – 3×1^2 = 1
//         9^2 – 5×4^2 = 1
//         5^2 – 6×2^2 = 1
//         8^2 – 7×3^2 = 1
//
// Hence, by considering minimal solutions in x for D ≤ 7, the largest x is
// obtained when D = 5.
//
// Find the value of D ≤ 1000 in minimal solutions of x for which the largest
// value of x is obtained.
package main

import (
	"fmt"
	"math"
	"math/big"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	limit := 1000
	limRoot := int(math.Sqrt(float64(limit)))

	maxX, maxD := big.NewInt(9), 5

	for i, j := 0, 1; j <= limRoot; i, j = i+1, j+1 {
		for D := i*i + 1; D < j*j && D <= limit; D++ {
			x := common.DiophantineSolutions(D, 1, 1)[0].Fst

			if x.Cmp(maxX) > 0 {
				maxX = x
				maxD = D
			}
		}
	}

	return maxD
}
