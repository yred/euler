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

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	limit := 1500000

	pythagoreans := make(map[int]int)

	for m := 2; m <= int(math.Sqrt(float64(limit/2))); m++ {
		var start int
		if m%2 == 0 {
			start = 1
		} else {
			start = 2
		}

		for n := start; n < m; n += 2 {
			if common.GCD(m, n) == 1 {
				perimeter := rightTrianglePerimeter(m, n)

				for p := perimeter; p <= limit; p += perimeter {
					pythagoreans[p]++

					if pythagoreans[p] == 1 {
						count++
					} else if pythagoreans[p] == 2 {
						count--
					}
				}
			}
		}
	}

	return
}

// Returns the perimeter of the right triangle defined by the tuple (m, n), as
// described by Euclid's formula. For more information, visit:
//
//      http://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
func rightTrianglePerimeter(m, n int) int {
	return 2 * (m*m + m*n)
}
