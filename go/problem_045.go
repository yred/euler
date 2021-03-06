// Problem 45 - Triangular, pentagonal, and hexagonal
//
// Triangle, pentagonal, and hexagonal numbers are generated by the following
// formulae:
//
// Triangle        T(n) = n(n+1)/2     1, 3, 6, 10, 15, ...
// Pentagonal      P(n) = n(3n−1)/2    1, 5, 12, 22, 35, ...
// Hexagonal       H(n) = n(2n−1)      1, 6, 15, 28, 45, ...
//
// It can be verified that T(285) = P(165) = H(143) = 40755.
//
// Find the next triangle number that is also pentagonal and hexagonal.
package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(solution())
}

func solution() (t int) {
	last := 285

	for n := last + 1; ; n++ {
		t = triangle(n)
		if is_pentagonal(t) && is_hexagonal(t) {
			break
		}
	}

	return
}

func triangle(n int) int {
	return n * (n + 1) / 2
}

func pentagonal(n int) int {
	return n * (3*n - 1) / 2
}

func is_pentagonal(n int) bool {
	return pentagonal(int(math.Floor(math.Sqrt((2.0/3.0)*float64(n))))+1) == n
}

func hexagonal(n int) int {
	return n * (2*n - 1)
}

func is_hexagonal(n int) bool {
	return hexagonal(int(math.Floor(math.Sqrt(float64(n)/2)))+1) == n
}
