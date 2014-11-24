// Problem 85 - Counting rectangles
//
// By counting carefully it can be seen that a rectangular grid measuring 3 by 2
// contains eighteen rectangles.
//
// Although there exists no rectangular grid that contains exactly two million
// rectangles, find the area of the grid with the nearest solution.
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	target := 2000000

	var maxdim int
	for n := 1; ; n++ {
		if rectangles(n, 1) >= target {
			maxdim = n
			break
		}
	}

	candidates := make(map[int]int)

	for a := 1; ; a++ {
		lower := 1
		upper := maxdim

		for upper-lower > 1 {
			b := (upper + lower) / 2

			if rectangles(a, b) > target {
				upper = b
			} else {
				lower = b
			}
		}

		candidates[a*lower] = target - rectangles(a, lower)
		candidates[a*upper] = rectangles(a, upper) - target

		if lower <= a {
			break
		}
	}

	var minDelta, minArea int
	for area, delta := range candidates {
		if minDelta == 0 || delta < minDelta {
			minDelta = delta
			minArea = area
		}
	}

	return minArea
}

// Returns the number of rectangles contained within a rectangular grid measuring
// `width` by `length`
func rectangles(width, length int) (count int) {
	for w := 0; w < width; w++ {
		for l := 0; l < length; l++ {
			count += (width - w) * (length - l)
		}
	}

	return
}
