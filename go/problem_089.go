// Problem 89 - Roman numerals
//
// For a number written in Roman numerals to be considered valid there are basic
// rules which must be followed. Even though the rules allow some numbers to be
// expressed in more than one way there is always a "best" way of writing a
// particular number.
//
// For example, it would appear that there are at least six ways of writing the
// number sixteen:
//
//             IIIIIIIIIIIIIIII
//             VIIIIIIIIIII
//             VVIIIIII
//             XIIIIII
//             VVVI
//             XVI
//
// However, according to the rules only XIIIIII and XVI are valid, and the last
// example is considered to be the most efficient, as it uses the least number of
// numerals.
//
// The 11K text file, "../resources/p089_roman.txt", contains one thousand numbers
// written in valid, but not necessarily minimal, Roman numerals; see About Roman
// Numerals (https://projecteuler.net/about=roman_numerals) for the definitive
// rules for this problem.
//
// Find the number of characters saved by writing each of these in their minimal
// form.
//
// Note: You can assume that all the Roman numerals in the file contain no more
// than four consecutive identical units.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

var VALUES = map[byte]int{
	'M': 1000,
	'D': 500,
	'C': 100,
	'L': 50,
	'X': 10,
	'V': 5,
	'I': 1,
}

var LETTERS = map[int]string{
	1000: "M",
	500:  "D",
	100:  "C",
	50:   "L",
	10:   "X",
	5:    "V",
	1:    "I",
}

var SIMPLIFICATIONS = map[string]string{
	"CM": strings.Repeat("C", 9),
	"CD": strings.Repeat("C", 4),
	"XC": strings.Repeat("X", 9),
	"XL": strings.Repeat("X", 4),
	"IX": strings.Repeat("I", 9),
	"IV": strings.Repeat("I", 4),
}

func main() {
	fmt.Println(solution())
}

func solution() (saved int) {
	bytes, _ := ioutil.ReadFile("../resources/p089_roman.txt")
	numerals := strings.Split(strings.TrimSpace(string(bytes)), "\n")

	for _, num := range numerals {
		saved += len(num) - len(roman(value(num)))
	}

	return
}

func value(roman string) (v int) {
	for oldS, newS := range SIMPLIFICATIONS {
		roman = strings.Replace(roman, oldS, newS, 1)
	}

	for _, c := range roman {
		v += VALUES[byte(c)]
	}

	return
}

func roman(n int) (r string) {
	var q int

	for _, d := range []int{1000, 100, 10, 1} {
		q, n = n/d, n%d

		switch {
		case d == 1000:
			r += strings.Repeat(LETTERS[d], q)
		case q == 4 || q == 9:
			r += LETTERS[d] + LETTERS[d*(q+1)]
		case q >= 5:
			r += LETTERS[d*5] + strings.Repeat(LETTERS[d], q-5)
		case q > 0:
			r += strings.Repeat(LETTERS[d], q)
		}
	}

	return
}
