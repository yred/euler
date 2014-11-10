// Problem 67 - Maximum path sum II
//
// By starting at the top of the triangle below and moving to adjacent numbers on
// the row below, the maximum total from top to bottom is 23.
//
// 3
// 7 4
// 2 4 6
// 8 5 9 3
//
// That is, 3 + 7 + 4 + 9 = 23.
//
// Find the maximum total from top to bottom in "../resources/p067_triangle.txt",
// a 15K text file containing a triangle with one-hundred rows.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"./common"
)

type Triangle [][]int

func main() {
	fmt.Println(solution())
}

func solution() int {
	input, _ := ioutil.ReadFile("../resources/p067_triangle.txt")

	rows := strings.Split(strings.TrimSpace(string(input)), "\n")

	triangle := Triangle(make([][]int, len(rows)))
	for ix, row := range rows {
		triangle[ix] = common.Ints(strings.Split(strings.TrimSpace(row), " "))
	}

	return bestPath(triangle)
}

func bestPath(t Triangle) int {
	paths := make([]int, len(t))
	for ix := range paths {
		paths[ix] = t[len(t)-1][ix]
	}

	for i := len(t) - 2; i >= 0; i-- {
		newPaths := make([]int, i+1)
		for j := 0; j <= i; j++ {
			if paths[j] >= paths[j+1] {
				newPaths[j] = t[i][j] + paths[j]
			} else {
				newPaths[j] = t[i][j] + paths[j+1]
			}
		}
		paths = newPaths
	}

	return paths[0]
}
