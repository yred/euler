// Problem 80 - Square root digital expansion
//
// It is well known that if the square root of a natural number is not an integer,
// then it is irrational. The decimal expansion of such square roots is infinite
// without any repeating pattern at all.
//
// The square root of two is 1.41421356237309504880..., and the digital sum of the
// first one hundred decimal digits is 475.
//
// For the first one hundred natural numbers, find the total of the digital sums
// of the first one hundred decimal digits for all the irrational square roots.
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

func solution() (sum int) {
	lbound := 1
	ubound := 100
	digits := 100

	for n := lbound; n <= ubound; n++ {
		sum += common.Sum(squareRootDigits(n, digits))
	}

	return
}

func square(n int) int {
	return n * n
}

func isSquare(n int) bool {
	return square(int(math.Sqrt(float64(n)))) == n
}

func bigSquare(n *big.Int) *big.Int {
	return big.NewInt(0).Exp(n, big.NewInt(2), nil)
}

func bigAppend(a *big.Int, b int) *big.Int {
	n := big.NewInt(0).Mul(a, big.NewInt(10))
	return n.Add(n, big.NewInt(int64(b)))
}

func median(a, b int) int {
	return (a + b) / 2
}

func squareRootDigits(n, decimals int) []int {
	isqrt := int(math.Sqrt(float64(n)))
	digits := common.Digits(isqrt)

	bigN := big.NewInt(int64(n))
	current := big.NewInt(int64(isqrt))

	for ix := 1; ix <= decimals; ix++ {
		// Account for 2 extra decimal digits from the product
		bigN = bigN.Mul(bigN, big.NewInt(100))

		lo, hi := 0, 9
		if bigSquare(bigAppend(current, hi)).Cmp(bigN) < 0 {
			digits = append(digits, hi)
			current.Set(bigAppend(current, hi))
		} else {
			for {
				mid := median(lo, hi)

				if bigSquare(bigAppend(current, mid)).Cmp(bigN) < 0 {
					lo = mid
				} else {
					hi = mid
				}

				if median(lo, hi) == mid {
					digits = append(digits, mid)
					current.Set(bigAppend(current, mid))
					break
				}
			}
		}
	}

	return digits
}
