package common

import (
	"math"
	"strconv"
)

func Digits(n int) []int {
	if n < 0 {
		n = -n
	}

	digits := make([]int, int64(math.Log10(float64(n)))+1)

	for ix, d := range strconv.Itoa(n) {
		digits[ix] = int(byte(d) - '0')
	}

	return digits
}

func Integer(digits []int) (number int) {
	for _, d := range digits {
		number *= 10
		number += d
	}
	return
}

func Strings(ns []int) []string {
	strs := make([]string, len(ns))
	for ix, n := range ns {
		strs[ix] = strconv.Itoa(n)
	}
	return strs
}

func Ints(strs []string) []int {
	ns := make([]int, len(strs))
	for ix, s := range strs {
		ns[ix], _ = strconv.Atoi(s)
	}
	return ns
}

func SumDigits(str string) (sum int) {
	for _, b := range []byte(str) {
		sum += int(b - '0')
	}
	return
}

func Power(a, b int) int {
	return int(math.Pow(float64(a), float64(b)))
}

func Range(start, limit int) []int {
	numbers := make([]int, limit-start)
	for ix, value := 0, start; value < limit; ix, value = ix+1, value+1 {
		numbers[ix] = value
	}
	return numbers
}
