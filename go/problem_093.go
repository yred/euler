// Problem 93 - Arithmetic expressions
//
// By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and
// making use of the four arithmetic operations (+, −, *, /) and
// brackets/parentheses, it is possible to form different positive integer
// targets.
//
// For example,
//
//             8 = (4 * (1 + 3)) / 2
//             14 = 4 * (3 + 1 / 2)
//             19 = 4 * (2 + 3) − 1
//             36 = 3 * 4 * (2 + 1)
//
// Note that concatenations of the digits, like 12 + 34, are not allowed.
//
// Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different
// target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can
// be obtained before encountering the first non-expressible number.
//
// Find the set of four distinct digits, a < b < c < d, for which the longest set
// of consecutive positive integers, 1 to n, can be obtained, giving your answer
// as a string: abcd.
package main

import (
	"fmt"
	"strings"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() string {
	digits := common.Range(0, 10)
	ops := []func(float64, float64) float64{add, sub, mul, div}

	opSets := make([][]func(float64, float64) float64, 0)
	for _, index := range indexOrderings(len(ops), 3) {
		set := make([]func(float64, float64) float64, len(index))
		for ix, pos := range index {
			set[ix] = ops[pos]
		}
		opSets = append(opSets, set)
	}

	maxDigits, maxConsecutive := "", 0

	for _, combo := range common.Combinations(digits, 4) {
		generated := make(map[int]bool)

		for _, perm := range common.Permutations(combo, 4) {
			a := float64(perm[0])
			b := float64(perm[1])
			c := float64(perm[2])
			d := float64(perm[3])

			for _, set := range opSets {
				x := set[0]
				y := set[1]
				z := set[2]

				r1 := x(y(z(a, b), c), d)
				if isInteger(r1) {
					generated[int(r1)] = true
				}

				r2 := x(y(a, b), z(c, d))
				if isInteger(r2) {
					generated[int(r2)] = true
				}
			}
		}

		for n := 1; ; n++ {
			if _, exists := generated[n]; !exists {
				if n > maxConsecutive {
					maxConsecutive = n
					maxDigits = strings.Join(common.Strings(combo), "")
				}
				break
			}
		}
	}

	return maxDigits
}

func add(a, b float64) float64 {
	return a + b
}

func sub(a, b float64) float64 {
	return a - b
}

func mul(a, b float64) float64 {
	return a * b
}

func div(a, b float64) float64 {
	if b == 0 {
		// Obviously false result that wouldn't affect the result of the problem
		return 1000000
	} else {
		return a / b
	}
}

// Returns the set of all possible permutations of the combinations (with
// replacement) of the indexes of an array of size `setSize` chosen from
// `numElems` elements
func indexOrderings(numElems, setSize int) [][]int {
	keySep := "-"
	all := make(map[string]bool)
	elems := common.Range(0, numElems)
	for _, c := range common.CombinationsWithReplacement(elems, setSize) {
		for _, p := range common.Permutations(c, setSize) {
			key := strings.Join(common.Strings(p), keySep)
			all[key] = true
		}
	}

	ords := make([][]int, 0)
	for key := range all {
		slice := common.Ints(strings.Split(key, keySep))
		ords = append(ords, slice)
	}

	return ords
}

func isInteger(x float64) bool {
	return float64(int(x)) == x
}
