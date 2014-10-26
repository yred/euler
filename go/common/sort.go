package common

import (
	"sort"
)

// Solution originally from:
//      http://stackoverflow.com/questions/22688651/golang-how-to-sort-string-or-byte
type runes []rune

func (rs runes) Len() int {
	return len(rs)
}

func (rs runes) Less(i, j int) bool {
	return rs[i] < rs[j]
}

func (rs runes) Swap(i, j int) {
	rs[i], rs[j] = rs[j], rs[i]
}

func SortString(s string) string {
	rs := []rune(s)
	sort.Sort(runes(rs))
	return string(rs)
}
