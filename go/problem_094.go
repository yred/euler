// Problem 94 - Almost equilateral triangles
//
// It is easily proved that no equilateral triangle exists with integral length
// sides and integral area. However, the almost equilateral triangle 5-5-6 has an
// area of 12 square units.
//
// We shall define an almost equilateral triangle to be a triangle for which two
// sides are equal and the third differs by no more than one unit.
//
// Find the sum of the perimeters of all almost equilateral triangles with
// integral side lengths and area and whose perimeters do not exceed one billion
// (1,000,000,000).
package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	start := 2
	limit := 1000000000 / 6

	deltas := []int{1, -1}

	for n := start; n < limit; n++ {
		for _, delta := range deltas {
			odd := 2*n + delta

			if isSquare(odd*odd - n*n) {
				sum += 2 * (odd + n)
			}
		}
	}

	return
}

func square(n int) int {
	return n * n
}

func isSquare(n int) bool {
	return square(int(math.Sqrt(float64(n)))) == n
}
