// Problem 84 - Monopoly odds
//
// In the game, Monopoly, the standard board is set up in the following way:
//
//              GO  A1 CC1  A2  T1  R1  B1 CH1  B2  B3  JAIL
//              H2                                      C1
//              T2                                      U1
//              H1                                      C2
//             CH3                                      C3
//              R4                                      R2
//              G3                                      D1
//             CC3                                      CC2
//              G2                                      D2
//              G1                                      D3
//             G2J  F3  U2  F2  F1  R3  E3  E2 CH2  E1  FP
//
// A player starts on the GO square and adds the scores on two 6-sided dice to
// determine the number of squares they advance in a clockwise direction. Without
// any further rules we would expect to visit each square with equal probability:
// 2.5%. However, landing on G2J (Go To Jail), CC (community chest), and CH
// (chance) changes this distribution.
//
// In addition to G2J, and one card from each of CC and CH, that orders the player
// to go directly to jail, if a player rolls three consecutive doubles, they do
// not advance the result of their 3rd roll. Instead they proceed directly to
// jail.
//
// At the beginning of the game, the CC and CH cards are shuffled. When a player
// lands on CC or CH they take a card from the top of the respective pile and,
// after following the instructions, it is returned to the bottom of the pile.
// There are sixteen cards in each pile, but for the purpose of this problem we
// are only concerned with cards that order a movement; any instruction not
// concerned with movement will be ignored and the player will remain on the CC/CH
// square.
//
//     Community Chest (2/16 cards):
//         Advance to GO
//         Go to JAIL
//     Chance (10/16 cards):
//         Advance to GO
//         Go to JAIL
//         Go to C1
//         Go to E3
//         Go to H2
//         Go to R1
//         Go to next R (railway company)
//         Go to next R
//         Go to next U (utility company)
//         Go back 3 squares.
//
// The heart of this problem concerns the likelihood of visiting a particular
// square. That is, the probability of finishing at that square after a roll. For
// this reason it should be clear that, with the exception of G2J for which the
// probability of finishing on it is zero, the CH squares will have the lowest
// probabilities, as 5/8 request a movement to another square, and it is the final
// square that the player finishes at on each roll that we are interested in. We
// shall make no distinction between "Just Visiting" and being sent to JAIL, and
// we shall also ignore the rule about requiring a double to "get out of jail",
// assuming that they pay to get out on their next turn.
//
// By starting at GO and numbering the squares sequentially from 00 to 39 we can
// concatenate these two-digit numbers to produce strings that correspond with
// sets of squares.
//
// Statistically it can be shown that the three most popular squares, in order,
// are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and
// GO (3.09%) = Square 00. So these three most popular squares can be listed with
// the six-digit modal string: 102400.
//
// If, instead of using two 6-sided dice, two 4-sided dice are used, find the
// six-digit modal string.
package main

import (
	"fmt"
	"math/rand"
	"sort"
	"strings"
)

const (
	CC = iota
	CH
	RR
	UT
)

var LABELS = []string{
	"GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL", "C1",
	"U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2",
	"E3", "R3", "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4",
	"CH3", "H1", "T2", "H2",
}

const NUM_CARDS = 16

var CCHEST = []func(*board, *player, *square){
	visitIndex(0),
	visitIndex(10),
}

var CHANCE = []func(*board, *player, *square){
	visitIndex(0),
	visitIndex(5),
	visitIndex(10),
	visitIndex(11),
	visitIndex(24),
	visitIndex(39),
	visitNext(RR),
	visitNext(UT),
	visitSteps(-3),
}

type square struct {
	index  int
	label  string
	typ    int
	visits int
}

type board struct {
	squares  []*square
	cchest   []func(*board, *player, *square)
	cchestIx int
	chance   []func(*board, *player, *square)
	chanceIx int
}

type dice []int

type player struct {
	game     *board
	position int
}

type squares []*square

func (sqs squares) Len() int {
	return len(sqs)
}

func (sqs squares) Less(i, j int) bool {
	return sqs[i].visits < sqs[j].visits
}

func (sqs squares) Swap(i, j int) {
	sqs[i], sqs[j] = sqs[j], sqs[i]
}

func newSquare(index int, label string) *square {
	typ := -1
	switch {
	case strings.HasPrefix(label, "CC"):
		typ = CC
	case strings.HasPrefix(label, "CH"):
		typ = CH
	case strings.HasPrefix(label, "R"):
		typ = RR
	case strings.HasPrefix(label, "U"):
		typ = UT
	}
	return &square{index, label, typ, 0}
}

func (sq *square) Host(p *player) {
	p.position = sq.index
	sq.visits++
}

func (sq *square) String() string {
	return fmt.Sprintf("Square<%v:%v>", sq.label, sq.index)
}

func newBoard() *board {
	squares := make([]*square, len(LABELS))
	for ix, label := range LABELS {
		squares[ix] = newSquare(ix, label)
	}

	ccCards := completeAndShuffle(CCHEST)
	chCards := completeAndShuffle(CHANCE)

	return &board{squares, ccCards, 0, chCards, 0}
}

func (b *board) At(index int) *square {
	return b.squares[index%len(b.squares)]
}

func (b *board) Next(typ, start int) *square {
	var ix int
	for ix = start; ; ix = (ix + 1) % len(b.squares) {
		if b.squares[ix].typ == typ {
			break
		}
	}
	return b.squares[ix]
}

func (b *board) Play(p *player, d dice, turns int) {
	doubles := 0

	for ix := 0; ix < turns; ix++ {
		steps, isDouble := d.Roll()

		if isDouble {
			doubles++
		} else {
			doubles = 0
		}

		if doubles == 3 {
			p.Visit(b.At(10))
			doubles = 0
		} else {
			p.Move(steps)
		}
	}
}

func (b *board) Handle(p *player, sq *square) {
	switch sq.typ {
	case CC:
		b.cchest[b.cchestIx](b, p, sq)
		b.cchestIx = (b.cchestIx + 1) % NUM_CARDS
	case CH:
		b.chance[b.chanceIx](b, p, sq)
		b.chanceIx = (b.chanceIx + 1) % NUM_CARDS
	default:
		// Go to Jail
		if sq.label == "G2J" {
			p.Visit(b.At(10))
		} else {
			sq.Host(p)
		}
	}
}

func (b *board) ModalString() string {
	mostFrequent := squares(append([]*square(nil), b.squares...))
	sort.Sort(sort.Reverse(mostFrequent))

	modal := ""
	for ix := 0; ix < 3; ix++ {
		modal += fmt.Sprintf("%02d", mostFrequent[ix].index)
	}
	return modal
}

func (d dice) Roll() (int, bool) {
	value, first, allSame := 0, -1, true

	for _, max := range []int(d) {
		throw := 1 + rand.Intn(max)
		value += throw

		if first >= 0 {
			allSame = allSame && first == throw
		} else {
			first = throw
		}
	}

	return value, allSame
}

func (p *player) Move(steps int) {
	p.Visit(p.game.At(p.position + steps))
}

func (p *player) Visit(sq *square) {
	p.game.Handle(p, sq)
}

func main() {
	fmt.Println(solution())
}

func solution() string {
	monopoly := newBoard()
	monopoly.Play(&player{monopoly, 0}, dice([]int{4, 4}), 1000000)
	return monopoly.ModalString()
}

func visitIndex(index int) func(*board, *player, *square) {
	return func(b *board, p *player, sq *square) {
		p.Visit(b.At(index))
	}
}

func visitNext(typ int) func(*board, *player, *square) {
	return func(b *board, p *player, sq *square) {
		p.Visit(b.Next(typ, sq.index))
	}
}

func visitSteps(steps int) func(*board, *player, *square) {
	return func(b *board, p *player, sq *square) {
		p.Visit(b.At(sq.index + steps))
	}
}

func host(b *board, p *player, sq *square) {
	sq.Host(p)
}

// Complete the set of (chance or community chest) cards with the "filler"
// `host` card/function, and return the full shuffled set
func completeAndShuffle(cs []func(*board, *player, *square)) []func(*board, *player, *square) {
	full := append(([]func(*board, *player, *square))(nil), cs...)
	for ix := len(full); ix < NUM_CARDS; ix++ {
		full = append(full, host)
	}

	shuffled := make([]func(*board, *player, *square), 16)
	for ix, jx := range rand.Perm(NUM_CARDS) {
		shuffled[ix] = full[jx]
	}

	return shuffled
}
