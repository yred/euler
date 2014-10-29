// Problem 39 - Integer right triangles
//
// If p is the perimeter of a right angle triangle with integral length sides,
// {a,b,c}, there are exactly three solutions for p = 120:
//
//         {20, 48, 52}, {24, 45, 51}, {30, 40, 50}
//
// For which value of p ≤ 1000, is the number of solutions maximised?
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	perimeters := make(map[int]int)

	maxPerimeter := 1000
	for a := 1; a < maxPerimeter/3; a++ {
		for b := a + 1; b < (maxPerimeter-a)/2; b++ {
			for c := b + 1; a+b+c <= maxPerimeter; c++ {
				if a*a+b*b == c*c {
					perimeters[a+b+c] += 1
				}
			}
		}
	}

	max, maxP := 0, 0
	for p, count := range perimeters {
		if count > max {
			max, maxP = count, p
		}
	}

	return maxP
}
