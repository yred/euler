// Problem 99 - Largest exponential
//
// Comparing two numbers written in index form like 2^11 and 3^7 is not difficult,
// as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
//
// However, confirming that 632382^518061 > 519432^525806 would be much more
// difficult, as both numbers contain over three million digits.
//
// Using "../resources/p099_base_exp.txt", a 22K text file containing one thousand
// lines with a base/exponent pair on each line, determine which line number has
// the greatest numerical value.
//
// NOTE: The first two lines in the file represent the numbers in the example
// given above.
package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"strconv"
	"strings"
)

func main() {
	fmt.Println(solution())
}

func solution() int {
	bytes, _ := ioutil.ReadFile("../resources/p099_base_exp.txt")

	maxLine, maxVal := 0, float64(0)

	for ix, line := range strings.Split(string(bytes), "\n") {
		numbers := strings.Split(line, ",")

		base, _ := strconv.ParseFloat(numbers[0], 64)
		exponent, _ := strconv.ParseFloat(numbers[1], 64)

		// Replacing a^b by b*log(a) preserves order
		value := exponent * math.Log(base)
		if value > maxVal {
			maxLine, maxVal = ix+1, value
		}
	}

	return maxLine
}
