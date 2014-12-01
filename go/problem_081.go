// Problem 81 - Path sum: two ways
//
// In the 5 by 5 matrix below, the minimal path sum from the top left to the
// bottom right, by only moving to the right and down, is indicated using
// parentheses and is equal to 2427.
//
//                    (131)   673    234    103     18
//                    (201)  ( 96)  (342)   965    150
//                     630    803   (746)  (422)   111
//                     537    699    497   (121)   956
//                     805    732    524   ( 37)  (331)
//
// Find the minimal path sum, in "../resources/p081_matrix.txt", a 31K text file
// containing an 80 by 80 matrix, from the top left to the bottom right by only
// moving right and down.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"./common"
)

type point struct {
	row, col int
}

func (p point) Right() point {
	return point{row: p.row, col: p.col + 1}
}

func (p point) Down() point {
	return point{row: p.row + 1, col: p.col}
}

func (p point) Cost(m [][]int) int {
	return m[p.row][p.col]
}

func main() {
	fmt.Println(solution())
}

func solution() (best int) {
	bytes, _ := ioutil.ReadFile("../resources/p081_matrix.txt")
	contents := strings.TrimSpace(string(bytes))

	matrix := make([][]int, 0)

	for _, line := range strings.Split(contents, "\n") {
		matrix = append(matrix, common.Ints(strings.Split(line, ",")))
	}

	paths := map[point]int{point{0, 0}: matrix[0][0]}

	maxRow, maxCol := len(matrix)-1, len(matrix[0])-1

	for {
		newPaths := make(map[point]int)

		for pt, cost := range paths {
			if pt.row < maxRow {
				down := pt.Down()
				if cur, ok := newPaths[down]; !ok || cost+down.Cost(matrix) < cur {
					newPaths[down] = cost + down.Cost(matrix)
				}
			}

			if pt.col < maxCol {
				right := pt.Right()
				if cur, ok := newPaths[right]; !ok || cost+right.Cost(matrix) < cur {
					newPaths[right] = cost + right.Cost(matrix)
				}
			}
		}

		paths = newPaths
		if len(paths) == 1 {
			best = paths[point{maxRow, maxCol}]
			break
		}
	}

	return
}
