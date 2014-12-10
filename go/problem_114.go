// Problem 114 - Counting block combinations I
//
// A row measuring seven units in length has red blocks with a minimum length of
// three units placed on it, such that any two red blocks (which are allowed to be
// different lengths) are separated by at least one black square. There are
// exactly seventeen ways of doing this.
//
//         (check combinations at: https://projecteuler.net/problem=114)
//
// How many ways can a row measuring fifty units in length be filled?
//
// NOTE: Although the example above does not lend itself to the possibility, in
// general it is permitted to mix block sizes. For example, on a row measuring
// eight units in length you could use red (3), black (1), and red (4).
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int64 {
	return combinations(50, 3)
}

func combinations(length, min int) int64 {
	count := int64(1)

	for ix, l := 1, min; l <= length; ix, l = ix+1, l+1 {
		count += int64(ix) * combinations(length-l-1, min)
	}

	return count
}
