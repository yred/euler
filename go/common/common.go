package common

import (
	"math"
	"strconv"
)

// Originally from:
//   http://stackoverflow.com/questions/1752414/how-to-reverse-a-string-in-go
func ReverseString(str string) string {
	runes := []rune(str)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func IsPalindrome(str string) bool {
	runes := []rune(str)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		if runes[i] != runes[j] {
			return false
		}
	}
	return true
}

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

func SumDigits(str string) (sum int) {
	for _, b := range []byte(str) {
		sum += int(b - '0')
	}
	return
}
