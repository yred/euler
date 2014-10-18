// Problem 9 - Special Pythagorean triplet
//
// A Pythagorean triplet is a set of three natural numbers, a < b < c, for
// which, a² + b² = c²
//
// For example, 3² + 4² = 9 + 16 = 25 = 5².
//
// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
// Find the product abc.
package main

import (
	"fmt"
)

func main() {
	fmt.Println(Solution())
}

func Solution() (product int) {
	for a := 1; a < 1000/3; a++ {
		for b := a + 1; b < (1000-a)/2; b++ {
			c := 1000 - a - b

			if a*a+b*b == c*c {
				product = a * b * c
				goto End
			}
		}
	}

End:
	return
}
