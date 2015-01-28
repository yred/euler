// Problem 96 - Su Doku
//
// Su Doku (Japanese meaning number place) is the name given to a popular puzzle
// concept. Its origin is unclear, but credit must be attributed to Leonhard Euler
// who invented a similar, and much more difficult, puzzle idea called Latin
// Squares. The objective of Su Doku puzzles, however, is to replace the blanks
// (or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box
// contains each of the digits 1 to 9. Below is an example of a typical starting
// puzzle grid and its solution grid.
//
//             0 0 3 | 0 2 0 | 6 0 0            4 8 3 | 9 2 1 | 6 5 7
//             9 0 0 | 3 0 5 | 0 0 1            9 6 7 | 3 4 5 | 8 2 1
//             0 0 1 | 8 0 6 | 4 0 0            2 5 1 | 8 7 6 | 4 9 3
//             ---------------------            ---------------------
//             0 0 8 | 1 0 2 | 9 0 0            5 4 8 | 1 3 2 | 9 7 6
//             7 0 0 | 0 0 0 | 0 0 8            7 2 9 | 5 6 4 | 1 3 8
//             0 0 6 | 7 0 8 | 2 0 0            1 3 6 | 7 9 8 | 2 4 5
//             ---------------------            ---------------------
//             0 0 2 | 6 0 9 | 5 0 0            3 7 2 | 6 8 9 | 5 1 4
//             8 0 0 | 2 0 3 | 0 0 9            8 1 4 | 2 5 3 | 7 6 9
//             0 0 5 | 0 1 0 | 3 0 0            6 9 5 | 4 1 7 | 3 8 2
//
// A well constructed Su Doku puzzle has a unique solution and can be solved by
// logic, although it may be necessary to employ "guess and test" methods in order
// to eliminate options (there is much contested opinion over this). The
// complexity of the search determines the difficulty of the puzzle; the example
// above is considered easy because it can be solved by straight forward direct
// deduction.
//
// The 6K text file, "../resources/p096_sudoku.txt", contains fifty different
// Su Doku puzzles ranging in difficulty, but all with unique solutions (the first
// puzzle in the file is the example above).
//
// By solving all fifty puzzles find the sum of the 3-digit numbers found in the
// top left corner of each solution grid; for example, 483 is the 3-digit number
// found in the top left corner of the solution grid above.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"

	"./common"
)

type puzzle struct {
	rows [][]int
}

func (p *puzzle) Row(n int) []int {
	return append([]int(nil), p.rows[n]...)
}

func (p *puzzle) Column(n int) []int {
	col := make([]int, 9)
	for ix, row := range p.rows {
		col[ix] = row[n]
	}
	return col
}

func (p *puzzle) Block(n int) []int {
	block := make([]int, 9)

	bFirstRow := 3 * (n / 3)
	bFirstCol := 3 * (n % 3)

	for ix := 0; ix < 3; ix++ {
		for jx := 0; jx < 3; jx++ {
			block[3*ix+jx] = p.rows[bFirstRow+ix][bFirstCol+jx]
		}
	}

	return block
}

func (p *puzzle) ToRow(n index) int {
	return n / 9
}

func (p *puzzle) ToColumn(n index) int {
	return n % 9
}

func (p *puzzle) ToBlock(n index) int {
	return (p.ToRow(n)/3)*3 + (p.ToColumn(n) / 3)
}

func (p *puzzle) Solve() (success bool) {
	unresolved := make(map[int][]int)

	for {
		updated := false

		for ix, val := range p.rows {
			// TODO
		}
	}
}

func main() {
	fmt.Println(solution())
}

func solution() int {
	bytes, _ := ioutil.ReadFile("../resources/p096_sudoku.txt")
	contents := strings.TrimSpace(string(bytes))

	puzzles := make([]*puzzle, 0)
	current := make([][]int, 9)

	for ix, line := range strings.Split(contents, "\n") {
		ix %= 10

		if ix == 0 {
			continue
		}

		current[ix-1] = common.Ints(strings.Split(line, ""))
		if ix == 9 {
			puzzles = append(puzzles, &puzzle{current})
			current = make([][]int, 9)
		}
	}

	return len(puzzles)
}
