// Problem 17 - Number letter counts
//
// If the numbers 1 to 5 are written out in words: one, two, three, four, five,
// then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
//
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
// words, how many letters would be used?
//
// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
// forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
// letters. The use of "and" when writing out numbers is in compliance with
// British usage.
package main

import (
	"fmt"
)

var dict = map[int]string{
	1:    "one",
	2:    "two",
	3:    "three",
	4:    "four",
	5:    "five",
	6:    "six",
	7:    "seven",
	8:    "eight",
	9:    "nine",
	10:   "ten",
	11:   "eleven",
	12:   "twelve",
	13:   "thirteen",
	14:   "fourteen",
	15:   "fifteen",
	16:   "sixteen",
	17:   "seventeen",
	18:   "eighteen",
	19:   "nineteen",
	20:   "twenty",
	30:   "thirty",
	40:   "forty",
	50:   "fifty",
	60:   "sixty",
	70:   "seventy",
	80:   "eighty",
	90:   "ninety",
	100:  "hundred",
	1000: "thousand",
}

func main() {
	fmt.Println(solution())
}

func solution() int {
	letterCount := 0
	for i := 1; i <= 1000; i++ {
		for _, c := range toWords(i) {
			if 'a' <= c && c <= 'z' {
				letterCount++
			}
		}
	}
	return letterCount
}

func toWords(n int) string {
	switch {
	case n < 100:
		if value, ok := dict[n]; ok {
			return value
		} else {
			// n%10 != 0 since it's not in dict
			return dict[(n/10)*10] + "-" + dict[n%10]
		}
	case n < 1000:
		hundreds := dict[n/100] + " " + dict[100]

		if n%100 == 0 {
			return hundreds
		} else {
			return hundreds + " and " + toWords(n%100)
		}
	default:
		thousands := dict[n/1000] + " " + dict[1000]

		if n%1000 == 0 {
			return thousands
		} else if n%1000 < 100 {
			return thousands + " and " + toWords(n%1000)
		} else {
			return thousands + " " + toWords(n%1000)
		}
	}
}
