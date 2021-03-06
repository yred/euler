// Problem 61 - Cyclical figurate numbers
//
// Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are
// all figurate (polygonal) numbers and are generated by the following formulae:
//
//     Triangle        P3(n) = n(n+1)/2        1, 3, 6, 10, 15, ...
//     Square          P4(n) = n^2             1, 4, 9, 16, 25, ...
//     Pentagonal      P5(n) = n(3n−1)/2       1, 5, 12, 22, 35, ...
//     Hexagonal       P6(n) = n(2n−1)         1, 6, 15, 28, 45, ...
//     Heptagonal      P7(n) = n(5n−3)/2       1, 7, 18, 34, 55, ...
//     Octagonal       P8(n) = n(3n−2)         1, 8, 21, 40, 65, ...
//
// The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
// interesting properties.
//
//     -   The set is cyclic, in that the last two digits of each number is the
//         first two digits of the next number (including the last number with the
//         first).
//
//     -   Each polygonal type: triangle (P3(127)=8128), square (P4(91)=8281), and
//         pentagonal (P5(44)=2882), is represented by a different number in the
//         set.
//
//     -   This is the only set of 4-digit numbers with this property.
//
// Find the sum of the only ordered set of six cyclic 4-digit numbers for which
// each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and
// octagonal, is represented by a different number in the set.
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	numbers := map[int][]int{
		3: fourDigitNumbers(func(n int) int { return n * (n + 1) / 2 }),
		4: fourDigitNumbers(func(n int) int { return n * n }),
		5: fourDigitNumbers(func(n int) int { return n * (3*n - 1) / 2 }),
		6: fourDigitNumbers(func(n int) int { return n * (2*n - 1) }),
		7: fourDigitNumbers(func(n int) int { return n * (5*n - 3) / 2 }),
		8: fourDigitNumbers(func(n int) int { return n * (3*n - 2) }),
	}

	fst2 := make(map[int]map[int][]int)
	lst2 := make(map[int]map[int][]int)

	for n := 3; n < 8; n++ {
		for _, k := range numbers[n] {
			// First 2 digits
			if _, ok := fst2[k/100]; !ok {
				fst2[k/100] = make(map[int][]int)
			}

			if _, ok := fst2[k/100][n]; ok {
				fst2[k/100][n] = append(append([]int{}, fst2[k/100][n]...), k)
			} else {
				fst2[k/100][n] = []int{k}
			}

			// Last 2 digits
			if _, ok := lst2[k%100]; !ok {
				lst2[k%100] = make(map[int][]int)
			}

			if _, ok := lst2[k%100][n]; ok {
				lst2[k%100][n] = append(append([]int{}, lst2[k%100][n]...), k)
			} else {
				lst2[k%100][n] = []int{k}
			}
		}
	}

	for _, octa := range numbers[8] {
		for _, p := range common.Permutations([]int{3, 4, 5, 6, 7}, 5) {
			head := [][]int{[]int{octa}}
			tail := [][]int{[]int{octa}}

			var h, t map[int]bool

			for len(p) > 0 {
				h = make(map[int]bool)
				t = make(map[int]bool)

				for _, elem := range head[len(head)-1] {
					for _, n := range lst2[elem/100][p[len(p)-1]] {
						h[n] = true
					}
				}

				for _, elem := range tail[len(tail)-1] {
					for _, n := range fst2[elem%100][p[0]] {
						t[n] = true
					}
				}

				if len(h) == 0 || len(t) == 0 {
					break
				}

				head = append(head, keys(h))
				tail = append(tail, keys(t))

				if len(p) > 1 {
					p = p[1 : len(p)-1]
				} else {
					p = p[1:]
				}
			}

			shared := intersection(h, t)
			if len(shared) > 0 {
				cycle := []int{shared[0]}

				for ix := len(head) - 2; ix >= 0; ix-- {
					for _, n := range head[ix] {
						if n/100 == cycle[len(cycle)-1]%100 {
							cycle = append(cycle, n)
							break
						}
					}
				}

				for ix := 1; ix < len(tail)-1; ix++ {
					for _, n := range tail[ix] {
						if n/100 == cycle[len(cycle)-1]%100 {
							cycle = append(cycle, n)
							break
						}
					}
				}

				sum = common.Sum(cycle)
			}

			if sum > 0 {
				break
			}
		}

		if sum > 0 {
			break
		}
	}

	return
}

func fourDigitNumbers(fn func(int) int) []int {
	numbers := make([]int, 0)
	for n := 1; ; n++ {
		value := fn(n)
		if value >= 10000 {
			break
		} else if value < 1000 {
			continue
		}
		numbers = append(numbers, value)
	}
	return numbers
}

func keys(m map[int]bool) []int {
	ks := make([]int, 0, len(m))
	for k := range m {
		ks = append(ks, k)
	}
	return ks
}

func intersection(a, b map[int]bool) []int {
	shared := make([]int, 0)
	for ka := range a {
		if _, ok := b[ka]; ok {
			shared = append(shared, ka)
		}
	}
	return shared
}
