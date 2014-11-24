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

type rectangle struct {
	Width  int
	Length int
}

func (r rectangle) Area() int {
	return r.Width * r.Length
}

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

	candidates := make(map[rectangle]int)

	for w := 1; ; w++ {
		lower := 1
		upper := maxdim

		for upper-lower > 1 {
			l := (upper + lower) / 2

			if rectangles(w, l) > target {
				upper = l
			} else {
				lower = l
			}
		}

		candidates[rectangle{w, lower}] = target - rectangles(w, lower)
		candidates[rectangle{w, upper}] = rectangles(w, upper) - target

		if lower <= w {
			break
		}
	}

	var minArea, minDiff int
	for rect, diff := range candidates {
		if minDiff == 0 || diff < minDiff {
			minDiff = diff
			minArea = rect.Area()
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
