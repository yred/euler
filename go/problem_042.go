// Problem 42 - Coded triangle numbers
//
// The n-th term of the sequence of triangle numbers is given by, t(n) = ½n(n+1);
// so the first ten triangle numbers are:
//
//         1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
//
// By converting each letter in a word to a number corresponding to its
// alphabetical position and adding these values we form a word value. For
// example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value
// is a triangle number then we shall call the word a triangle word.
//
// Using "../resources/p042_words.txt", a 16K text file containing nearly
// two-thousand common English words, how many are triangle words?
package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strings"
)

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	bytes, _ := ioutil.ReadFile("../resources/p042_words.txt")

	words := strings.Split(strings.Replace(string(bytes), "\"", "", -1), ",")

	for _, word := range words {
		value := 0
		for _, c := range []byte(word) {
			value += int(c) - int('A') + 1
		}

		if is_triangle(value) {
			count++
		}
	}

	return
}

func is_triangle(n int) bool {
	a := int(math.Floor(math.Sqrt(float64(2 * n))))
	return a*(a+1) == 2*n
}
