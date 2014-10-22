// Problem 5 - Smallest multiple
//
// 2520 is the smallest number that can be divided by each of the numbers from 1
// to 10 without any remainder.
//
// What is the smallest positive number that is evenly divisible by all of the
// numbers from 1 to 20?

package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	lcm := 1
	for i := 2; i <= 20; i++ {
		lcm = (i * lcm) / common.GCD(lcm, i)
	}
	return lcm
}
