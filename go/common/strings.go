package common

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

func ReplacePositions(str string, pos []int, r rune) string {
	runes := []rune(str)
	for _, ix := range pos {
		runes[ix] = r
	}
	return string(runes)
}

func MaxStrings(strs ...string) string {
	max := strs[0]

	for _, s := range strs[1:] {
		if s > max {
			max = s
		}
	}

	return max
}

func MinStrings(strs ...string) string {
	min := strs[0]

	for _, s := range strs[1:] {
		if s < min {
			min = s
		}
	}

	return min
}
