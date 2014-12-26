// Problem 59 - XOR decryption
//
// Each character on a computer is assigned a unique code and the preferred
// standard is ASCII (American Standard Code for Information Interchange). For
// example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
//
// A modern encryption method is to take a text file, convert the bytes to ASCII,
// then XOR each byte with a given value, taken from a secret key. The advantage
// with the XOR function is that using the same encryption key on the cipher text,
// restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
//
// For unbreakable encryption, the key is the same length as the plain text
// message, and the key is made up of random bytes. The user would keep the
// encrypted message and the encryption key in different locations, and without
// both "halves", it is impossible to decrypt the message.
//
// Unfortunately, this method is impractical for most users, so the modified
// method is to use a password as a key. If the password is shorter than the
// message, which is likely, the key is repeated cyclically throughout the
// message. The balance for this method is using a sufficiently long password key
// for security, but short enough to be memorable.
//
// Your task has been made easy, as the encryption key consists of three lower
// case characters. Using "../resources/p059_cipher.txt", a file containing the
// encrypted ASCII codes, and the knowledge that the plain text must contain
// common English words, decrypt the message and find the sum of the ASCII values
// in the original text.
package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strings"

	"./common"
)

func main() {
	fmt.Println(solution())
}

func solution() (sum int) {
	bytes, _ := ioutil.ReadFile("../resources/p059_cipher.txt")
	encBytes := common.Ints(strings.Split(strings.TrimSpace(string(bytes)), ","))

	// Create a map/set of common English words
	ws, _ := ioutil.ReadFile("../resources/english_common_10000.txt")
	words := strings.Split(strings.TrimSpace(string(ws)), "\n")
	dictionary := make(map[string]bool)
	for _, word := range words {
		dictionary[word] = true
	}

	lowercase := make([]int, 0)
	for char := 'a'; char <= 'z'; char++ {
		lowercase = append(lowercase, int(char))
	}

	for _, key := range common.Permutations(lowercase, 3) {
		ptext := decrypt(encBytes, key)

		if isReadable(ptext) && isHuman(ptext, dictionary) {
			sum = common.Sum(ptext)
			break
		}
	}

	return
}

func decrypt(ctext []int, key []int) []int {
	decrypted := make([]int, len(ctext))
	keyLength := len(key)

	for ix, c := range ctext {
		decrypted[ix] = c ^ key[ix%keyLength]
	}

	return decrypted
}

func toString(text []int) string {
	bytes := make([]byte, len(text))
	for ix, c := range text {
		bytes[ix] = byte(c)
	}
	return string(bytes)
}

func isReadable(text []int) bool {
	for _, c := range text {
		if c < 32 || c >= 127 {
			return false
		}
	}

	return true
}

func isHuman(text []int, dict map[string]bool) bool {
	noise := regexp.MustCompile("[^a-zA-Z ]")
	human := noise.ReplaceAllString(strings.ToLower(toString(text)), "")

	words := strings.Split(human, " ")
	found := 0

	for _, word := range words {
		if _, exists := dict[word]; exists {
			found++
		}
	}

	return float64(found)/float64(len(words)) >= 0.8
}
