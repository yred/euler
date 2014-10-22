// Problem 22 - Names scores
//
// Using "../resources/p022_names.txt", a 46K text file containing over
// five-thousand first names, begin by sorting it into alphabetical order.
// Then working out the alphabetical value for each name, multiply this value by
// its alphabetical position in the list to obtain a name score.
//
// For example, when the list is sorted into alphabetical order, COLIN, which is
// worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
// obtain a score of 938 × 53 = 49714.
//
// What is the total of all the name scores in the file?
package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strings"
)

func main() {
	fmt.Println(solution())
}

func solution() (sumScores int) {
	bytes, _ := ioutil.ReadFile("../resources/p022_names.txt")

	names := strings.Split(strings.Replace(string(bytes), "\"", "", -1), ",")
	sort.Strings(names)

	for ix, name := range names {
		for _, letter := range name {
			// All the names are upper-case
			sumScores += (ix + 1) * (int(letter) - int('A') + 1)
		}
	}

	return
}
