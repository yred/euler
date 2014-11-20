// Problem 75 - Singular integer right triangles
//
// It turns out that 12 cm is the smallest length of wire that can be bent to form
// an integer sided right angle triangle in exactly one way, but there are many
// more examples.
//
//         12 cm: (3,4,5)
//         24 cm: (6,8,10)
//         30 cm: (5,12,13)
//         36 cm: (9,12,15)
//         40 cm: (8,15,17)
//         48 cm: (12,16,20)
//
// In contrast, some lengths of wire, like 20 cm, cannot be bent to form an
// integer sided right angle triangle, and other lengths allow more than one
// solution to be found; for example, using 120 cm it is possible to form exactly
// three different integer sided right angle triangles.
//
// 120 cm: (30,40,50), (20,48,52), (24,45,51)
//
// Given that L is the length of the wire, for how many values of L ≤ 1,500,000
// can exactly one integer sided right angle triangle be formed?
package main

import (
	"fmt"
	"math"
	"strings"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	limit := 1500000

	pythagoreans := make(map[int]map[string]bool)

	for m := 2; m <= int(math.Sqrt(float64(limit/2))); m++ {
		var start int
		if m%2 == 0 {
			start = 1
		} else {
			start = 2
		}

		for n := start; n < m; n += 2 {
			if common.GCD(m, n) == 1 {
				sides := rightTriangle(m, n)
				a, b, c := sides[0], sides[1], sides[2]

				perimeter := a + b + c

				for k := 1; k*perimeter <= limit; k++ {
					if _, ok := pythagoreans[k*perimeter]; !ok {
						pythagoreans[k*perimeter] = make(map[string]bool)
					}
					pythagoreans[k*perimeter][strings.Join(common.Strings([]int{k * a, k * b, k * c}), "")] = true
				}
			}
		}
	}

	for perimeter := range pythagoreans {
		if len(pythagoreans[perimeter]) == 1 {
			count++
		}
	}

	return
}

// Returns the sides of the right triangle defined by the tuple (m, n), as
// described by Euclid's formula. For more information, visit:
//
//      http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
func rightTriangle(m, n int) [3]int {
	return [3]int{m*m - n*n, 2 * m * n, m*m + n*n}
}
