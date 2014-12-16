// Problem 117 - Red, green, and blue tiles
//
// Using a combination of black square tiles and oblong tiles chosen from: red
// tiles measuring two units, green tiles measuring three units, and blue tiles
// measuring four units, it is possible to tile a row measuring five units in
// length in exactly fifteen different ways.
//
//         (check combinations at https://projecteuler.net/problem=117)
//
// How many ways can a row measuring fifty units in length be tiled?
//
// NOTE: This is related to Problem 116.
package main

import (
	"fmt"
)

type input struct {
	row, t1, t2, t3 int64
}

var memo = make(map[input]int64)

func main() {
	fmt.Println(solution())
}

func solution() int64 {
	// Tile lengths
	red, green, blue := int64(2), int64(3), int64(4)

	// Row length
	row := int64(50)

	return combinations(row, red, green, blue)
}

func combinations(rowLen, t1, t2, t3 int64) int64 {
	if res, ok := memo[input{rowLen, t1, t2, t3}]; ok {
		return res
	}

	if rowLen < 0 {
		return 0
	}

	count := int64(1)
	for _, tile := range []int64{t1, t2, t3} {
		for length := tile; length <= rowLen; length++ {
			count += combinations(rowLen-length, t1, t2, t3)
		}
	}

	memo[input{rowLen, t1, t2, t3}] = count

	return count
}
