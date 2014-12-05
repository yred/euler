// Problem 86 - Cuboid route
//
// A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a
// fly, F, sits in the opposite corner. By travelling on the surfaces of the room
// the shortest "straight line" distance from S to F is 10 and the path is shown
// on the diagram (https://projecteuler.net/project/images/p086.gif).
//
// However, there are up to three "shortest" path candidates for any given cuboid
// and the shortest route doesn't always have integer length.
//
// It can be shown that there are exactly 2060 distinct cuboids, ignoring
// rotations, with integer dimensions, up to a maximum size of M by M by M, for
// which the shortest route has integer length when M = 100. This is the least
// value of M for which the number of solutions first exceeds two thousand; the
// number of solutions when M = 99 is 1975.
//
// Find the least value of M such that the number of solutions first exceeds one
// million.
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() (M int) {
	target := 1000000

	start := 1
	limit := 1001

	squares := make(map[int]bool)

	for cuboids := 0; M == 0; start, limit = limit, 2*limit-start {
		squares = updateSquaresSet(start, 5*limit*limit, squares)

		for a := start; a < limit; a++ {
			for b := 1; b <= a; b++ {
				for c := 1; c <= b; c++ {
					shortest := a*a + (b+c)*(b+c)

					if _, isSquare := squares[shortest]; isSquare {
						cuboids++
					}
				}
			}

			if cuboids >= target {
				M = a
				break
			}
		}
	}

	return
}

func updateSquaresSet(start, maxSquare int, set map[int]bool) map[int]bool {
	if len(set) == 0 {
		set = make(map[int]bool)
	}

	for n := start; n*n < maxSquare; n++ {
		set[n*n] = true
	}

	return set
}
