// Problem 16 - Power digit sum
//
// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
//
// What is the sum of the digits of the number 2^1000?
package main

import (
	"fmt"
	"math/big"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	return common.SumDigits(big.NewInt(0).Exp(big.NewInt(2), big.NewInt(1000), nil).String())
}
