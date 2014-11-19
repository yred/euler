// Problem 77 - Prime summations
//
// It is possible to write ten as the sum of primes in exactly five different
// ways:
//
//         7 + 3
//         5 + 5
//         5 + 3 + 2
//         3 + 3 + 2 + 2
//         2 + 2 + 2 + 2 + 2
//
// What is the first value which can be written as the sum of primes in over five
// thousand different ways?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (a int) {
	target := 5000

	plimit := 1000
	primes := common.PrimesUpTo(plimit)

	for n := 2; ; n++ {
		if n > plimit {
			plimit *= 10
			primes = common.PrimesUpTo(plimit)
		}

		plist := append([]int{}, primes...)
		for i := range plist {
			if plist[i] > n {
				plist = reverse(plist[:i])
				break
			}
		}

		if combinations(n, plist) > target {
			a = n
			break
		}
	}

	return
}

// Returns the number of ways in which `total` can be produced as a sum of
// elements from the reverse sorted array `list`
func combinations(total int, list []int) (count int) {

	for ix, largest := range list[:len(list)-1] {
		remaining := total
		for largest <= remaining {
			remaining -= largest
			count += combinations(remaining, list[ix+1:])
		}
	}

	if total%list[len(list)-1] == 0 {
		count++
	}

	return
}

func reverse(list []int) []int {
	for i, j := 0, len(list)-1; i < j; i, j = i+1, j-1 {
		list[i], list[j] = list[j], list[i]
	}
	return list
}
