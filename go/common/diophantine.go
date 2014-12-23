package common

import (
	"math"
	"math/big"
)

type Tuple struct {
	Fst, Snd int
}

type BigTuple struct {
	Fst, Snd *big.Int
}

func simplify(a, b int) (int, int) {
	gcd := GCD(a, b)
	return a / gcd, b / gcd
}

func BigSquare(n *big.Int) *big.Int {
	return big.NewInt(0).Mul(n, n)
}

func ContinuedFraction(n int) (int, []int) {
	isqrtn := math.Sqrt(float64(n))
	processed := make(map[Tuple]bool)

	currentA := int(isqrtn)
	aSequence := []int{currentA}

	current := Tuple{1, currentA}

	for {
		if _, exists := processed[current]; exists {
			break
		} else {
			processed[current] = true
		}

		numer, denom := simplify(current.Fst, n-current.Snd*current.Snd)

		currentA := int(float64(numer) / float64(denom) * (isqrtn + float64(current.Snd)))
		aSequence = append(aSequence, currentA)

		current = Tuple{denom, denom*currentA - current.Snd}
	}

	return aSequence[0], aSequence[1:]
}

// Returns the first k terms in the sequence of convergents for √n
func SqrtConvergents(n, k int) []*big.Rat {
	first, cycle := ContinuedFraction(n)

	cycleLen := len(cycle)
	ratCycle := make([]*big.Rat, cycleLen)
	for ix, num := range cycle {
		ratCycle[ix] = big.NewRat(int64(num), 1)
	}

	convergents := make([]*big.Rat, k)
	convergents[0] = big.NewRat(int64(first), 1)

	for ix := 1; ix < k; ix++ {
		frac := big.NewRat(0, 1)

		for jx := ix - 1; jx >= 0; jx-- {
			frac.Inv(frac.Add(frac, ratCycle[jx%cycleLen]))
		}

		convergents[ix] = frac.Add(frac, convergents[0])
	}

	return convergents
}

// Returns the first k solutions for the Diophantine equation: x^2 - n*y^2 = m
func DiophantineSolutions(n, m, k int) []BigTuple {
	solutions := make([]BigTuple, 0)

	bigN := big.NewInt(int64(n))
	bigM := big.NewInt(int64(m))

	for start, limit := 0, 2*k; len(solutions) < k; start, limit = limit, 2*limit {
		for _, c := range SqrtConvergents(n, limit)[start:] {
			x, y := c.Num(), c.Denom()
			x2, y2 := BigSquare(x), BigSquare(y)

			if bigM.Cmp(x2.Sub(x2, y2.Mul(y2, bigN))) == 0 {
				solutions = append(solutions, BigTuple{x, y})
				if len(solutions) >= k {
					break
				}
			}
		}
	}

	return solutions
}
