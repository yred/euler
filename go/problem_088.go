// Problem 88 - Product-sum numbers
//
// A natural number, N, that can be written as the sum and product of a given set
// of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum
// number:
//
//             N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.
//
// For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.
//
// For a given set of size, k, we shall call the smallest N with this property a
// minimal product-sum number. The minimal product-sum numbers for sets of size,
// k = 2, 3, 4, 5, and 6 are as follows.
//
//             k=2: 4 = 2 × 2 = 2 + 2
//             k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
//             k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
//             k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
//             k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6
//
// Hence for 2 ≤ k ≤ 6, the sum of all the minimal product-sum numbers is
//
//             4 + 6 + 8 + 12 = 30
//
// Note that 8 is only counted once in the sum.
//
// In fact, as the complete set of minimal product-sum numbers for 2 ≤ k ≤ 12 is
// {4, 6, 8, 12, 15, 16}, the sum is 61.
//
// What is the sum of all the minimal product-sum numbers for 2 ≤ k ≤ 12000?
package main

import (
	"fmt"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	max := 12000
	maxSumProduct := int(1.1 * float64(max))

	primes := common.PrimeSetUpTo(maxSumProduct)

	productSums := make(map[int][][]int)
	lenProdSums := make(map[int][]int)

	for n := 2; n < maxSumProduct; n++ {
		if _, isPrime := primes[n]; isPrime {
			continue
		}

		prodsums := make([][]int, 0)
		sumlengths := make(map[int]int)

		divisors := common.Divisors(n)
		for _, d := range divisors[1 : len(divisors)-1] {
			if d > n/d {
				break
			}

			prodsums = append(prodsums, []int{d, n / d})

			for _, list := range productSums[n/d] {
				newList := append([]int{d}, list...)
				sumList := common.Sum(newList)

				if l, found := sumlengths[sumList]; !found || l != len(newList) {
					prodsums = append(prodsums, newList)
					sumlengths[sumList] = len(newList)
				}
			}
		}

		for _, list := range prodsums {
			length := len(list) + (n - common.Sum(list))
			lenProdSums[length] = append(lenProdSums[length], n)
		}

		productSums[n] = prodsums
	}

	minProdSums := make(map[int]bool)
	for length, prodsums := range lenProdSums {
		if length <= max {
			min := common.Min(prodsums...)
			if _, found := minProdSums[min]; !found {
				sum += min
				minProdSums[min] = true
			}
		}
	}

	return
}
