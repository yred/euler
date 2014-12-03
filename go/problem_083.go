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

type Point struct {
	Row, Col int
}

type Cell struct {
	Cost, Best int
}

// Returns `true` if a move from `other` to the current cell results in a more
// optimal path
func (c *Cell) Update(other Cell) bool {
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

	cells := make(map[Point]*Cell)

	for row := 0; row <= maxRow; row++ {
		for col := 0; col <= maxCol; col++ {
			cells[Point{row, col}] = &Cell{Cost: matrix[row][col], Best: maxPath}
		}
	}

	cells[Point{0, 0}].Best = matrix[0][0]

	// Compute a first estimate on the best path by by limiting the possible
	// moves to `Down` and `Right`
	paths := []Point{Point{0, 0}}

	for len(paths) > 0 {
		newPaths := make([]Point, 0)

		for _, point := range paths {
			cell := cells[point]
			row, col := point.Row, point.Col

			if row < maxRow && cells[Point{row + 1, col}].Update(*cell) {
				newPaths = append(newPaths, Point{row + 1, col})
			}

			if col < maxCol && cells[Point{row, col + 1}].Update(*cell) {
				newPaths = append(newPaths, Point{row, col + 1})
			}
		}

		paths = newPaths
	}

	// Compute the actual optimal path for every cell using continuous updates,
	// until the solution converges for all cells
	for {
		updated := false

		for point, cell := range cells {
			row, col := point.Row, point.Col

			neighbors := make([]*Cell, 0)
			if row > 0 {
				neighbors = append(neighbors, cells[Point{row - 1, col}])
			}
			if row < maxRow {
				neighbors = append(neighbors, cells[Point{row + 1, col}])
			}
			if col > 0 {
				neighbors = append(neighbors, cells[Point{row, col - 1}])
			}
			if col < maxCol {
				neighbors = append(neighbors, cells[Point{row, col + 1}])
			}

			for _, neighbor := range neighbors {
				updated = neighbor.Update(*cell) || updated
			}
		}

		if !updated {
			break
		}
	}

	return cells[Point{maxRow, maxCol}].Best
}
