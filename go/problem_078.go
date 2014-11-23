// Problem 78 - Coin partitions
//
// Let p(n) represent the number of different ways in which n coins can be
// separated into piles. For example, five coins can separated into piles in
// exactly seven different ways, so p(5) = 7.
//
//         OOOOO
//         OOOO   O
//         OOO   OO
//         OOO   O   O
//         OO   OO   O
//         OO   O   O   O
//         O   O   O   O   O
//
// Find the least value of n for which p(n) is divisible by one million.
package main

import (
	"fmt"
)

func main() {
	fmt.Println(solution())
}

func solution() (index int) {
	modulus := int64(1000000)

	partitions := make([]int64, 0)

	// Add p(n) for n in [0, 1]
	partitions = append(partitions, 1, 1)

	// The number of partitions of n, p(n), is computed using the pentagonal
	// number recursion formula:
	//
	//  p(n) = Σ(-1)^k+1 * [p(n - k(3k-1)/2) + p(n - k(3k+1)/2)]   (k=1..n)
	for n := 2; ; n++ {
		pN := int64(0)

		for k, coeff := 1, 1; k <= n; k, coeff = k+1, coeff*-1 {
			fst := k * (3*k - 1) / 2
			snd := k * (3*k + 1) / 2

			if fst > n {
				break
			}

			pFst := partitions[n-fst]

			var pSnd int64
			if snd <= n {
				pSnd = partitions[n-snd]
			} else {
				pSnd = 0
			}

			pN += int64(coeff) * (pFst + pSnd)
		}

		if pN%modulus == 0 {
			index = n
			break
		} else {
			partitions = append(partitions, pN%modulus)
		}
	}

	return
}
