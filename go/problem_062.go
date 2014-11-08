// Problem 62 - Cubic permutations
//
// The cube, 41063625 (345^3), can be permuted to produce two other cubes:
// 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is the smallest cube
// which has exactly three permutations of its digits which are also cube.
//
// Find the smallest cube for which exactly five permutations of its digits are
// cube.
package main

import (
	"fmt"
	"strconv"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (cube int64) {

	cubes := make(map[string][]int64)

	for n := int64(1); ; n++ {
		ncubed := n * n * n
		sorted := common.SortString(strconv.FormatInt(ncubed, 10))

		cubes[sorted] = append(cubes[sorted], ncubed)

		if len(cubes[sorted]) == 5 {
			cube = cubes[sorted][0]
			break
		}
	}

	return
}
