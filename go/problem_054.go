// Problem 54 - Poker hands
//
// In the card game poker, a hand consists of five cards and are ranked, from
// lowest to highest, in the following way:
//
//         High Card:          Highest value card.
//         One Pair:           Two cards of the same value.
//         Two Pairs:          Two different pairs.
//         Three of a Kind:    Three cards of the same value.
//         Straight:           All cards are consecutive values.
//         Flush:              All cards of the same suit.
//         Full House:         Three of a kind and a pair.
//         Four of a Kind:     Four cards of the same value.
//         Straight Flush:     All cards are consecutive values of same suit.
//         Royal Flush:        Ten, Jack, Queen, King, Ace, in same suit.
//
// The cards are valued in the order:
//
//         2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
//
// If two players have the same ranked hands then the rank made up of the highest
// value wins; for example, a pair of eights beats a pair of fives (see example 1
// below). But if two ranks tie, for example, both players have a pair of
// queens, then highest cards in each hand are compared (see example 4 below); if
// the highest cards tie then the next highest cards are compared, and so on.
//
// Consider the following five hands dealt to two players:
//
//     Hand    Player 1            Player 2            Winner
//
//     1       5H 5C 6S 7S KD      2C 3S 8S 8D TD      Player 2
//             Pair of Fives       Pair of Eights
//
//     2       5D 8C 9S JS AC      2C 5C 7D 8S QH      Player 1
//             Highest card Ace    Highest card Queen
//
//     3       2D 9C AS AH AC      3D 6D 7D TD QD      Player 2
//             Three Aces          Flush with Diamonds
//
//     4       4D 6S 9H QH QC      3D 6D 7H QD QS      Player 1
//             Pair of Queens      Pair of Queens
//             Highest card Nine   Highest card Seven
//
//     5       2H 2D 4C 4D 4S      3C 3D 3S 9S 9D      Player 1
//             Full House          Full House
//             With Three Fours    With Three Threes
//
// The file, "../resources/p054_poker.txt", contains one-thousand random hands
// dealt to two players.
//
// Each line of the file contains ten cards (separated by a single space): the
// first five are Player 1's cards and the last five are Player 2's cards.
//
// You can assume that all hands are valid (no invalid characters or repeated
// cards), each player's hand is in no specific order, and in each hand there is a
// clear winner.
//
// How many hands does Player 1 win?
package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strings"

	"./common"
)

const (
	HI = iota
	PAIR
	DPAIR
	THREE
	STRAIGHT
	FLUSH
	FULL
	FOUR
	STRAIGHT_FLUSH
	ROYAL
)

var VALUES = map[rune]int{
	'2': 2,
	'3': 3,
	'4': 4,
	'5': 5,
	'6': 6,
	'7': 7,
	'8': 8,
	'9': 9,
	'T': 10,
	'J': 11,
	'Q': 12,
	'K': 13,
	'A': 14,
}

var SUITS = map[rune]int{
	'H': 1,
	'D': 2,
	'C': 3,
	'S': 4,
}

type card struct {
	value, suit int
}

type cardSeq []*card

func newCard(cStr string) *card {
	return &card{VALUES[rune(cStr[0])], SUITS[rune(cStr[1])]}
}

func (c *card) Compare(other *card) int {
	if c.value > other.value {
		return 1
	} else if c.value < other.value {
		return -1
	}
	return 0
}

func (cs cardSeq) Len() int {
	return len(cs)
}

func (cs cardSeq) Less(i, j int) bool {
	return cs[i].Compare(cs[j]) == -1
}

func (cs cardSeq) Swap(i, j int) {
	cs[i], cs[j] = cs[j], cs[i]
}

func sortCards(cards []*card) []*card {
	cSeq := cardSeq(cards)
	sort.Sort(cSeq)
	return []*card(cSeq)
}

func main() {
	fmt.Println(solution())
}

func solution() (count int) {
	bytes, _ := ioutil.ReadFile("../resources/p054_poker.txt")
	contents := strings.TrimSpace(string(bytes))

	for _, line := range strings.Split(contents, "\n") {
		cards := make([]*card, 0)

		for _, cardStr := range strings.Split(line, " ") {
			cards = append(cards, newCard(cardStr))
		}

		if winner(cards[:5], cards[5:]) == 1 {
			count++
		}
	}

	return
}

// Returns 1 if the hand `hand1` wins, 2 if `hand2` wins, 0 otherwise
func winner(hand1, hand2 []*card) int {
	r1, v1 := rank(hand1)
	r2, v2 := rank(hand2)

	switch {
	case r1 > r2:
		return 1
	case r2 > r1:
		return 2
	case v1 > v2:
		return 1
	case v2 > v1:
		return 2
	}

	hand1 = sortCards(hand1)
	hand2 = sortCards(hand2)

	for ix := len(hand1) - 1; ix >= 0; ix-- {
		if hand1[ix].value > hand2[ix].value {
			return 1
		} else if hand2[ix].value > hand1[ix].value {
			return 2
		}
	}

	return 0
}

// Note: assumes the hand contains exactly 5 cards
func rank(hand []*card) (rank int, value int) {
	hand = sortCards(hand)

	vCounter := reverse(counter(hand, func(c *card) int { return c.value }))
	sCounter := reverse(counter(hand, func(c *card) int { return c.suit }))

	// Check if all cards are of the same suit
	if _, ok := sCounter[5]; ok {
		if hand[0].value == 10 {
			// In the case of a royal flush, an associated value has no real
			// meaning
			return ROYAL, 0

		} else if hand[4].value-hand[0].value == 4 {
			return STRAIGHT_FLUSH, hand[4].value
		}
		return FLUSH, hand[4].value
	}

	if values, ok := vCounter[4]; ok {
		return FOUR, values[0]
	}

	if values, ok := vCounter[3]; ok {
		if _, ok := vCounter[2]; ok {
			return FULL, values[0]
		}
		return THREE, values[0]
	}

	if values, ok := vCounter[2]; ok {
		if len(values) == 2 {
			return DPAIR, common.Max(values...)
		}
		return PAIR, values[0]
	}

	// Since there aren't any pairs, it's safe to assume that all card values
	// are distinct
	if hand[4].value-hand[0].value == 4 {
		return STRAIGHT, hand[4].value
	}

	return HI, hand[4].value
}

func counter(hand []*card, getter func(*card) int) map[int]int {
	ctr := make(map[int]int)

	for _, c := range hand {
		val := getter(c)

		if count, ok := ctr[val]; ok {
			ctr[val] = count + 1
		} else {
			ctr[val] = 1
		}
	}

	return ctr
}

func reverse(m map[int]int) map[int][]int {
	revM := make(map[int][]int)

	for k, v := range m {
		if _, ok := revM[v]; ok {
			revM[v] = append(revM[v], k)
		} else {
			revM[v] = []int{k}
		}
	}

	return revM
}
