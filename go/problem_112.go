// Problem 112 - Bouncy numbers
//
// Working from left-to-right if no digit is exceeded by the digit to its left it
// is called an increasing number; for example, 134468.
//
// Similarly if no digit is exceeded by the digit to its right it is called a
// decreasing number; for example, 66420.
//
// We shall call a positive integer that is neither increasing nor decreasing a
// "bouncy" number; for example, 155349.
//
// Clearly there cannot be any bouncy numbers below one-hundred, but just over
// half of the numbers below one-thousand (525) are bouncy. In fact, the least
// number for which the proportion of bouncy numbers first reaches 50% is 538.
//
// Surprisingly, bouncy numbers become more and more common and by the time we
// reach 21780 the proportion of bouncy numbers is equal to 90%.
//
// Find the least number for which the proportion of bouncy numbers is exactly
// 99%.
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	for bouncy, n := 0, 100; ; n += 1 {
		if isBouncy(n) {
			bouncy += 1
		}

		if float64(bouncy)/float64(n) >= 0.99 {
			return n
		}
	}
}

func isBouncy(n int) bool {
	n, current := n/10, n%10

	for previous, direction := current, 0; n > 0; n, previous = n/10, current {
		current = n % 10

		if direction == 0 {
			if previous > current {
				direction = -1
			} else if previous < current {
				direction = 1
			}
		} else if (current-previous)*direction < 0 {
			return true
		}
	}

	return false
}
