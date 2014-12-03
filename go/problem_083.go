// Problem 83 - Path sum: four ways
//
// NOTE: This problem is a significantly more challenging version of Problem 81.
//
// In the 5 by 5 matrix below, the minimal path sum from the top left to the
// bottom right, by moving left, right, up, and down, is indicated using
// parentheses and is equal to 2297.
//
//                    (131)   673   (234)  (103) ( 18)
//                    (201)  ( 96)  (342)   965  (150)
//                     630    803   (746)  (422) (111)
//                     537    699    497   (121)  956
//                     805    732    524   ( 37) (331)
//
// Find the minimal path sum, in "../resources/p083_matrix.txt", a 31K text file
// containing a 80 by 80 matrix, from the top left to the bottom right by moving
// left, right, up, and down.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"./common"
)

type point struct {
	Row, Col int
}

type cell struct {
	Cost, Best int
}

// Returns `true` if a move from `other` to the current cell results in a more
// optimal path
func (c *cell) Update(other *cell) bool {
	if newCost := other.Best + c.Cost; newCost < c.Best {
		c.Best = newCost
		return true
	}
	return false
}

func main() {
	fmt.Println(solution())
}

func solution() int {
	bytes, _ := ioutil.ReadFile("../resources/p083_matrix.txt")
	contents := strings.TrimSpace(string(bytes))

	matrix := make([][]int, 0)

	for _, line := range strings.Split(contents, "\n") {
		matrix = append(matrix, common.Ints(strings.Split(line, ",")))
	}

	var maxPath int
	for _, rowValues := range matrix {
		maxPath += common.Sum(rowValues)
	}

	maxRow, maxCol := len(matrix)-1, len(matrix[0])-1

	cells := make(map[point]*cell)

	for row := 0; row <= maxRow; row++ {
		for col := 0; col <= maxCol; col++ {
			cells[point{row, col}] = &cell{Cost: matrix[row][col], Best: maxPath}
		}
	}

	cells[point{0, 0}].Best = matrix[0][0]

	// Compute a first estimate on the best path by by limiting the possible
	// moves to `Down` and `Right`
	paths := []point{point{0, 0}}

	for len(paths) > 0 {
		newPaths := make([]point, 0)

		for _, pt := range paths {
			cell := cells[pt]
			row, col := pt.Row, pt.Col

			if row < maxRow && cells[point{row + 1, col}].Update(cell) {
				newPaths = append(newPaths, point{row + 1, col})
			}

			if col < maxCol && cells[point{row, col + 1}].Update(cell) {
				newPaths = append(newPaths, point{row, col + 1})
			}
		}

		paths = newPaths
	}

	// Compute the real best path to each cell using continuous updates, until
	// the solution converges for all cells
	for {
		updated := false

		for pt, c := range cells {
			row, col := pt.Row, pt.Col

			neighbors := make([]*cell, 0)
			if row > 0 {
				neighbors = append(neighbors, cells[point{row - 1, col}])
			}
			if row < maxRow {
				neighbors = append(neighbors, cells[point{row + 1, col}])
			}
			if col > 0 {
				neighbors = append(neighbors, cells[point{row, col - 1}])
			}
			if col < maxCol {
				neighbors = append(neighbors, cells[point{row, col + 1}])
			}

			for _, neighbor := range neighbors {
				updated = updated || neighbor.Update(c)
			}
		}

		if !updated {
			break
		}
	}

	return cells[point{maxRow, maxCol}].Best
}
