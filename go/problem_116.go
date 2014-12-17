// Problem 116 - Red, green or blue tiles
//
// A row of five black square tiles is to have a number of its tiles replaced with
// coloured oblong tiles chosen from red (length two), green (length three), or
// blue (length four).
//
// If red tiles are chosen there are exactly seven ways this can be done. If green
// tiles are chosen there are three ways. And if blue tiles are chosen there are
// two ways.
//
//         (check combinations at https://projecteuler.net/problem=116)
//
// Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
// replacing the black tiles in a row measuring five units in length.
//
// How many different ways can the black tiles in a row measuring fifty units in
// length be replaced if colours cannot be mixed and at least one coloured tile
// must be used?
//
// NOTE: This is related to Problem 117.
package main

import (
	"fmt"
)

type input struct {
	row, tile int64
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

	return combinations(row, red) + combinations(row, green) + combinations(row, blue)
}

func combinations(rowLen, tileLen int64) int64 {
	if res, ok := memo[input{rowLen, tileLen}]; ok {
		return res
	}

	if rowLen < tileLen {
		return 0
	}

	// First compute combinations containing a single tile
	count := rowLen - (tileLen - 1)

	for length := tileLen; length < rowLen; length++ {
		count += combinations(rowLen-length, tileLen)
	}

	memo[input{rowLen, tileLen}] = count

	return count
}
