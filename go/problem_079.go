// Problem 79 - Passcode derivation
//
// A common security method used for online banking is to ask the user for three
// random characters from a passcode. For example, if the passcode was 531278,
// they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be:
// 317.
//
// The text file, "../resources/p079_keylog.txt", contains fifty successful login
// attempts.
//
// Given that the three characters are always asked for in order, analyse the file
// so as to determine the shortest possible secret passcode of unknown length.
package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	fmt.Println(solution())
}

func solution() (passcode string) {
	bytes, _ := ioutil.ReadFile("../resources/p079_keylog.txt")
	contents := strings.TrimSpace(string(bytes))
	partials := strings.Split(contents, "\n")

	for {
		first := make(map[rune]bool)
		other := make(map[rune]bool)

		for _, partial := range partials {
			first[rune(partial[0])] = true
			for _, c := range partial[1:] {
				other[c] = true
			}
		}

		nextchar := string(difference(first, other)[0])
		passcode += nextchar

		newPartials := make([]string, 0)
		for _, partial := range partials {
			newP := strings.TrimPrefix(partial, nextchar)
			if len(newP) > 1 {
				newPartials = append(newPartials, newP)
			}
		}

		partials = newPartials
		if len(partials) == 0 {
			// Only one digit/character should remain at this point
			for remaining := range other {
				passcode += string(remaining)
			}
			break
		}
	}

	return
}

func difference(a, b map[rune]bool) []rune {
	delta := make([]rune, 0)
	for key := range a {
		if _, exists := b[key]; !exists {
			delta = append(delta, key)
		}
	}
	return delta
}
