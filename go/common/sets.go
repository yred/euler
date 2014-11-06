package common

import (
	"math/big"
	"sort"
)

func Factorial(n int) *big.Int {
	return big.NewInt(0).MulRange(1, int64(n))
}

// Returns the number of permutations (ordered selections) of r elements from
// an n-element set
func P(n int, r int) *big.Int {
	return big.NewInt(0).Div(Factorial(n), Factorial(n-r))
}

// Returns all r-length permutations of elements in `set`. Permutations are
// produced in sorted order
func Permutations(set []int, r int) [][]int {
	if len(set) < r {
		return nil
	}

	// Permutations
	ps := make([][]int, int(P(len(set), r).Int64()))

	sorted := append([]int(nil), set...)
	sort.Ints(sorted)

	// Each element of `counters` and `thresholds` corresponds to the element
	// with the same position in a permutation (i.e., counters[i] is for the
	// i-th element in a permutation)
	counters, thresholds := make([]int, r), make([]int, r)
	for ix := range counters {
		counters[ix], thresholds[ix] = 0, len(set)-1-ix
	}

	current := append([]int(nil), sorted...)
	for ix := range ps {
		ps[ix] = append([]int(nil), current[:r]...)

		// "Increment" the counters
		for i := r - 1; i >= 0; i-- {
			if counters[i] < thresholds[i] {
				counters[i] += 1
				break
			} else {
				counters[i] = 0
			}
		}

		sortedCopy := append([]int(nil), sorted...)
		for i, c := range counters {
			current[i] = sortedCopy[c]
			sortedCopy = append(sortedCopy[:c], sortedCopy[c+1:]...)
		}
	}

	return ps
}

// Returns the number of combinations of r elements from an n-element set
func C(n int, r int) *big.Int {
	return big.NewInt(0).Binomial(int64(n), int64(r))
}

// Returns all r-length combinations of elements in `set`. Combinations are
// produced in sorted order
func Combinations(set []int, r int) [][]int {
	if len(set) < r || r <= 0 {
		return nil
	}

	// Combinations
	cs := make([][]int, int(C(len(set), r).Int64()))

	sorted := append([]int(nil), set...)
	sort.Ints(sorted)

	if r == 1 {
		for ix := range cs {
			cs[ix] = []int{sorted[ix]}
		}
	} else {
		for i, ix := 0, 0; i <= len(set)-r; i++ {
			for _, c := range Combinations(sorted[i+1:], r-1) {
				cs[ix] = append([]int{sorted[i]}, c...)
				ix++
			}
		}
	}

	return cs
}

func Sum(nums []int) int {
	sum := 0
	for _, n := range nums {
		sum += n
	}
	return sum
}

func Product(nums []int) int {
	product := 1
	for _, n := range nums {
		product *= n
	}
	return product
}

func AllInt(elems []int, fn func(int) bool) bool {
	for _, e := range elems {
		if !fn(e) {
			return false
		}
	}

	return true
}

func AnyInt(elems []int, fn func(int) bool) bool {
	for _, e := range elems {
		if fn(e) {
			return true
		}
	}

	return false
}

func TakeWhile(elems []int, fn func(int) bool) []int {
	// The return value defaults to the entire input slice
	index := len(elems)

	for ix, e := range elems {
		if !fn(e) {
			index = ix
			break
		}
	}

	return elems[:index]
}

func DropWhile(elems []int, fn func(int) bool) []int {
	// The return value defaults to an empty slice
	index := len(elems)

	for ix, e := range elems {
		if !fn(e) {
			index = ix
			break
		}
	}

	return elems[index:]
}
