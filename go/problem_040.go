// Problem 40 - Champernowne's constant
//
// An irrational decimal fraction is created by concatenating the positive
// integers:
//
//         0.12345678910[1]112131415161718192021...
//
// It can be seen that the 12th digit of the fractional part is 1.
//
// If d(n) represents the n-th digit of the fractional part, find the value of the
// following expression.
//
//     d(1) × d(10) × d(100) × d(1000) × d(10000) × d(100000) × d(1000000)
package main

import (
	"fmt"
	"strconv"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	// Target indexes
	indexes := []int{1, 10, 100, 1000, 10000, 100000, 1000000}

	current := indexes[0]
	indexes = indexes[1:]

	product := 1

	// `dskipped`: number of skipped digits
	// `count`: number of numbers with `length` digits
	dskipped, first, length, count := 0, 1, 1, 9

	for {
		if current-dskipped <= count*length {
			// `nskip`: numbers to skip
			nskip := (current - 1 - dskipped) / length

			// String with the `current`-th digit of the fraction
			numstr := strconv.Itoa(first + nskip)

			product *= int(numstr[current-1-dskipped-nskip*length]) - int('0')

			if len(indexes) > 0 {
				current, indexes = indexes[0], indexes[1:]
			} else {
				break
			}

		} else {
			dskipped += count * length
			first += count
			length += 1
			count *= 10
		}
	}

	return product
}
