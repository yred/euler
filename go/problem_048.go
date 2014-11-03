// Problem 48 - Self powers
//
// The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
//
// Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
package main

import (
	"fmt"
	"math"
	"math/big"
)

func main() {
	fmt.Println(solution())
}

func solution() int64 {
	sum := big.NewInt(0)
	mod := big.NewInt(int64(math.Pow10(10)))

	for n := 1; n <= 1000; n++ {
		bigN := big.NewInt(int64(n))
		sum.Add(sum, bigN.Exp(bigN, bigN, mod))
	}

	return sum.Mod(sum, mod).Int64()
}
