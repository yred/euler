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

type intLists [][]int

func (il intLists) Len() int {
	return len(il)
}

func (il intLists) Less(i, j int) bool {
	return il[i][0] < il[j][0]
}

func (il intLists) Swap(i, j int) {
	il[i], il[j] = il[j], il[i]
}

func main() {
	fmt.Println(solution())
}

func solution() string {
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
		if !common.IsPalindrome(square) {
			// Squares will be indexed by their length and the number of
			// distinct digits they contain
			l := len(square)
			d := distinctCount(square)

			primaryKey := common.Tuple{l, d}
			secondaryKey := common.SortString(square)

			var newSqAnagrams []string
			if _, exists := squareIdx[primaryKey]; exists {
				if sqAnagrams, ok := squareIdx[primaryKey][secondaryKey]; ok {
					newSqAnagrams = append(sqAnagrams, square)
				} else {
					newSqAnagrams = []string{square}
				}
			} else {
				squareIdx[primaryKey] = make(map[string][]string)
				newSqAnagrams = []string{square}
			}
			squareIdx[primaryKey][secondaryKey] = newSqAnagrams
		}
	}

	// Find the maximal anagramic square corresponding to the file's anagrams
	for ix := 0; ix < len(anagList); {
		curLen := len(anagList[ix][0])
		results := make([]string, 0)

		for ; ix < len(anagList) && len(anagList[ix][0]) == curLen; ix++ {
			anagrams := anagList[ix]
			distinct := distinctCount(anagrams[0])
			relevant := squareIdx[common.Tuple{curLen, distinct}]

			indices := common.Range(0, len(anagrams))
			for _, ixPair := range common.Combinations(indices, 2) {
				wa, wb := anagrams[ixPair[0]], anagrams[ixPair[1]]
				permutation := getPermutation(wa, wb)

				for _, anags := range relevant {
					for _, square := range anags {
						psquare := permutation(square)
						for _, sq := range anags {
							if psquare == sq {
								largest := common.MaxStrings(square, psquare)
								results = append(results, largest)
								break
							}
						}
					}
				}
			}
		}

		if len(results) > 0 {
			return common.MaxStrings(results...)
		}
	}

	return ""
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

// Returns a map linking each distinct character in `word` to the indices of
// its occurences
func positions(word string) map[rune][]int {
	index := make(map[rune][]int)
	for p, r := range []rune(word) {
		if pos, exists := index[r]; exists {
			index[r] = append(pos, p)
		} else {
			index[r] = []int{p}
		}
	}
	return index
}

// Returns a function that accepts a string and returns one of its anagrams,
// using the provided input/output pair as an example of the required
// permutation
func getPermutation(input, output string) func(string) string {
	index := positions(input)

	order := values(index)
	sort.Sort(intLists(order))

	permutation := make([]int, 0)
	for _, r := range []rune(output) {
		permutation = append(permutation, index[r][0])
		index[r] = index[r][1:]
	}

	return func(s string) string {
		sindex := positions(s)

		sorder := values(sindex)
		sort.Sort(intLists(sorder))

		// Only return an anagram if the indices of `s` and `input` match
		// (i.e., character/digit duplicates are contained in the same
		// positions)
		result := ""
		if equal(sorder, order) {
			for _, ix := range permutation {
				result += string(s[ix])
			}
		}

		return result
	}
}

func values(m map[rune][]int) [][]int {
	vals := make([][]int, 0)
	for _, val := range m {
		vals = append(vals, val)
	}
	return vals
}

func equal(a, b [][]int) bool {
	if len(a) != len(b) {
		return false
	}

	for ix := range a {
		if len(a[ix]) != len(b[ix]) {
			return false
		}

		for jx := range a[ix] {
			if a[ix][jx] != b[ix][jx] {
				return false
			}
		}
	}

	return true
}
