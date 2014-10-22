// Problem 11 - Largest product in a grid
//
// In the 20x20 grid below, four numbers along a diagonal line have been marked in
// red (between brackets).
//
//     08 02 22 97 38 15 00 40   00    75    04    05   07 78 52 12 50 77 91 08
//     49 49 99 40 17 81 18 57   60    87    17    40   98 43 69 48 04 56 62 00
//     81 49 31 73 55 79 14 29   93    71    40    67   53 88 30 03 49 13 36 65
//     52 70 95 23 04 60 11 42   69    24    68    56   01 32 56 71 37 02 36 91
//     22 31 16 71 51 67 63 89   41    92    36    54   22 40 40 28 66 33 13 80
//     24 47 32 60 99 03 45 02   44    75    33    53   78 36 84 20 35 17 12 50
//     32 98 81 28 64 23 67 10  [26]   38    40    67   59 54 70 66 18 38 64 70
//     67 26 20 68 02 62 12 20   95   [63]   94    39   63 08 40 91 66 49 94 21
//     24 55 58 05 66 73 99 26   97    17   [78]   78   96 83 14 88 34 89 63 72
//     21 36 23 09 75 00 76 44   20    45    35   [14]  00 61 33 97 34 31 33 95
//     78 17 53 28 22 75 31 67   15    94    03    80   04 62 16 14 09 53 56 92
//     16 39 05 42 96 35 31 47   55    58    88    24   00 17 54 24 36 29 85 57
//     86 56 00 48 35 71 89 07   05    44    44    37   44 60 21 58 51 54 17 58
//     19 80 81 68 05 94 47 69   28    73    92    13   86 52 17 77 04 89 55 40
//     04 52 08 83 97 35 99 16   07    97    57    32   16 26 26 79 33 27 98 66
//     88 36 68 87 57 62 20 72   03    46    33    67   46 55 12 32 63 93 53 69
//     04 42 16 73 38 25 39 11   24    94    72    18   08 46 29 32 40 62 76 36
//     20 69 36 41 72 30 23 88   34    62    99    69   82 67 59 85 74 04 36 16
//     20 73 35 29 78 31 90 01   74    31    49    71   48 86 81 16 23 57 05 54
//     01 70 54 71 83 51 54 69   16    92    33    48   61 43 52 01 89 19 67 48
//
// The product of these numbers is 26 x 63 x 78 x 14 = 1788696.
//
// What is the greatest product of four adjacent numbers in the same direction
// (up, down, left, right, or diagonally) in the 20x20 grid?
package main

import (
	"fmt"
	"strconv"
	"strings"

	"./common"
)

func main() {
	fmt.Println(Solution())
}

func Solution() int {
	numStrings := strings.Split(strings.TrimSpace(`
        08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
        49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
        81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
        52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
        22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
        24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
        32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
        67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
        24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
        21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
        78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
        16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
        86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
        19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
        04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
        88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
        04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
        20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
        20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
        01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
    `), "\n")

	m := make([][]int, len(numStrings))

	for i, numstr := range numStrings {
		ns := strings.Split(strings.TrimSpace(numstr), " ")

		m[i] = make([]int, len(ns))
		for j, nstr := range ns {
			m[i][j], _ = strconv.Atoi(nstr)
		}
	}

	return maxProduct(m, 4)
}

func maxProduct(m [][]int, n int) (max int) {
	sequences := make([][]int, 0)

	sequences = append(sequences, m...)
	sequences = append(sequences, transpose(m)...)
	sequences = append(sequences, rightDiagonals(m)...)
	sequences = append(sequences, leftDiagonals(m)...)

	for _, seq := range sequences {
		for i := 0; i <= len(seq)-n; i++ {
			p := common.Product(seq[i : i+n])
			if p > max {
				max = p
			}
		}
	}

	return
}

func transpose(m [][]int) [][]int {
	t := make([][]int, len(m[0]))

	for i := range t {
		t[i] = make([]int, len(m))
		for j := range t[i] {
			t[i][j] = m[j][i]
		}
	}

	return t
}

func rightDiagonals(m [][]int) [][]int {
	rows, cols := len(m), len(m[0])

	diagonals := make([][]int, rows*2-1)

	// diagonals starting at column 0
	for ix, row := 0, rows-1; row > 0; ix, row = ix+1, row-1 {
		diagonals[ix] = make([]int, rows-row)
		for d := 0; d < rows-row; d++ {
			diagonals[ix][d] = m[row+d][d]
		}
	}

	// diagonals starting at row 0
	for ix, col := rows-1, 0; col < cols; ix, col = ix+1, col+1 {
		diagonals[ix] = make([]int, cols-col)
		for d := 0; d < cols-col; d++ {
			diagonals[ix][d] = m[d][col+d]
		}
	}

	return diagonals
}

func leftDiagonals(m [][]int) [][]int {
	rows, cols := len(m), len(m[0])

	diagonals := make([][]int, rows*2-1)

	// diagonals starting at column `cols` - 1
	for ix, row := 0, rows-1; row > 0; ix, row = ix+1, row-1 {
		diagonals[ix] = make([]int, rows-row)
		for d := 0; d < rows-row; d++ {
			diagonals[ix][d] = m[row+d][cols-1-d]
		}
	}

	// diagonals starting at row 0
	for ix, col := rows-1, cols-1; col >= 0; ix, col = ix+1, col-1 {
		diagonals[ix] = make([]int, col+1)
		for d := 0; d < col+1; d++ {
			diagonals[ix][d] = m[d][col-d]
		}
	}

	return diagonals
}
