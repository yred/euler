// Problem 98 - Anagramic squares
//
// By replacing each of the letters in the word CARE with 1, 2, 9, and 6
// respectively, we form a square number: 1296 = 36^2. What is remarkable is that,
// by using the same digital substitutions, the anagram, RACE, also forms a square
// number: 9216 = 96^2. We shall call CARE (and RACE) a square anagram word pair
// and specify further that leading zeroes are not permitted, neither may a
// different letter have the same digital value as another letter.
//
// Using "../resources/p098_words.txt", a 16K text file containing nearly
// two-thousand common English words, find all the square anagram word pairs (a
// palindromic word is NOT considered to be an anagram of itself).
//
// What is the largest square number formed by any member of such a pair?
//
// NOTE: All anagrams formed must be contained in the given text file.
package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"sort"
	"strconv"
	"strings"

	"./common"
)

type strLists [][]string

func (sl strLists) Len() int {
	return len(sl)
}

func (sl strLists) Less(i, j int) bool {
	return len(sl[i][0]) < len(sl[j][0])
}

func (sl strLists) Swap(i, j int) {
	sl[i], sl[j] = sl[j], sl[i]
}

func main() {
	fmt.Println(solution())
}

func solution() int {
	bytes, _ := ioutil.ReadFile("../resources/p098_words.txt")
	contents := string(bytes)

	index := make(map[string][]string)
	for _, word := range strings.Split(contents, ",") {
		// Remove the double-quote marks
		word = word[1 : len(word)-1]

		key := common.SortString(word)
		if anagrams, found := index[key]; found {
			index[key] = append(anagrams, word)
		} else {
			index[key] = []string{word}
		}
	}

	// Filter out words without any anagrams in the file
	for key, anagrams := range index {
		if len(anagrams) < 2 {
			delete(index, key)
		}
	}

	// Sort the anagram sequences by decreasing length
	anagList := make([][]string, 0)
	for _, anagrams := range index {
		anagList = append(anagList, anagrams)
	}
	sort.Sort(sort.Reverse(strLists(anagList)))

	// Index all square anagrams up to the length of the longest anagram in the
	// file
	maxSquare := int64(math.Pow10(len(anagList[0][0]) + 1))
	squareIdx := make(map[common.Tuple]map[string][]string)

	for n := int64(1); n*n < maxSquare; n++ {
		square := strconv.FormatInt(n*n, 10)

		// Skip palindromic squares
		if square != common.ReverseString(square) {
			// Squares will be indexed by their length and the number of
			// distinct digits they contain
			l := len(square)
			d := distinctCount(square)

			// TODO
		}
	}

	// TODO

	return 0
}

func distinctCount(s string) int {
	index := make(map[rune]bool)
	for _, r := range []rune(s) {
		if _, exists := index[r]; !exists {
			index[r] = true
		}
	}
	return len(index)
}
