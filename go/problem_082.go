// Problem 82 - Path sum: three ways
//
// NOTE: This problem is a more challenging version of Problem 81.
//
// The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the
// left column and finishing in any cell in the right column, and only moving up,
// down, and right, is indicated using parentheses and is equal to 994.
//
//              131   673  (234) (103) ( 18)
//             (201) ( 96) (342)  965   180
//              630   803   746   422   111
//              537   699   497   121   956
//              805   732   524    37   331
//
// Find the minimal path sum, in "../resources/p082_matrix.txt", a 31K text file
// containing an 80 by 80 matrix, from the left column to the right column.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	bytes, _ := ioutil.ReadFile("../resources/p082_matrix.txt")
	contents := strings.TrimSpace(string(bytes))

	matrix := make([][]int, 0)

	for _, line := range strings.Split(contents, "\n") {
		matrix = append(matrix, common.Ints(strings.Split(line, ",")))
	}

	numRows, maxRow, maxCol := len(matrix), len(matrix)-1, len(matrix[0])-1

	paths := make([]int, numRows)
	for row := 0; row <= maxRow; row++ {
		paths[row] = matrix[row][0]
	}

	for col := 1; col <= maxCol; col++ {
		column := getColumn(col, matrix)
		newPaths := make([]int, numRows)

		for row := 0; row <= maxRow; row++ {
			candidates := append([]int{}, paths...)

			for ix := range candidates {
				if ix < row {
					candidates[ix] += common.Sum(column[ix : row+1])
				} else {
					candidates[ix] += common.Sum(column[row : ix+1])
				}
			}

			newPaths[row] = min(candidates)
		}

		paths = newPaths
	}

	return min(paths)
}

func getColumn(index int, matrix [][]int) []int {
	column := make([]int, len(matrix))
	for ix := range column {
		column[ix] = matrix[ix][index]
	}
	return column
}

// Returns the minimal value in `ns`. Assumes `ns` is not empty
func min(ns []int) int {
	minVal := ns[0]
	for _, val := range ns[1:] {
		if val < minVal {
			minVal = val
		}
	}
	return minVal
}
