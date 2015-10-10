// Problem 104 - Pandigital Fibonacci ends
//
// The Fibonacci sequence is defined by the recurrence relation:
//
//     F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
//
// It turns out that F(541), which contains 113 digits, is the first Fibonacci
// number for which the last nine digits are 1-9 pandigital (contain all the
// digits 1 to 9, but not necessarily in order). And F(2749), which contains 575
// digits, is the first Fibonacci number for which the first nine digits are 1-9
// pandigital.
//
// Given that F(k) is the first Fibonacci number for which the first nine digits
// AND the last nine digits are 1-9 pandigital, find k.
package main

import (
	"fmt"
	"math/big"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	a := big.NewInt(1)
	b := big.NewInt(1)

	// Using the problem statement, it should be safe to skip over Fibonacci
	// numbers up to F(2749)
	for k := 3; k < 2749; k++ {
		a, b = nextFib(a, b)
	}

	// Only keeping the leading and trailing digits of the last 2 Fibonacci
	// numbers should be sufficient for checking for pandigitals at both ends
	fA, lA, fB, lB := downsize(a, b, 20)

	for k := 2749; ; k++ {
		fA, fB = nextFib(fA, fB)
		lA, lB = nextFib(lA, lB)

		fBstr, lBstr := fB.String(), lB.String()
		first9, last9 := fBstr[:9], lBstr[len(lBstr)-9:]

		if isPandigital(first9) && isPandigital(last9) {
			return k
		}

		if len(fBstr) >= 100 {
			fA, _, fB, _ = downsize(fA, fB, 20)
			_, lA, _, lB = downsize(lA, lB, 20)
		}
	}
}

func isPandigital(s string) bool {
	return common.SortString(s) == "123456789"
}

func ends(n *big.Int, first, last int) (*big.Int, *big.Int) {
	nstr := n.String()

	prefix, _ := big.NewInt(0).SetString(nstr[:first], 10)
	suffix, _ := big.NewInt(0).SetString(nstr[len(nstr)-last:], 10)

	return prefix, suffix
}

func downsize(smaller, larger *big.Int, length int) (*big.Int, *big.Int, *big.Int, *big.Int) {
	delta := len(larger.String()) - len(smaller.String())

	prefSmall, suffSmall := ends(smaller, length, length)
	prefLarge, suffLarge := ends(larger, length+delta, length)
	return prefSmall, suffSmall, prefLarge, suffLarge
}

func nextFib(a, b *big.Int) (*big.Int, *big.Int) {
	newA := b
	newB := big.NewInt(0).Add(a, b)

	return newA, newB
}
