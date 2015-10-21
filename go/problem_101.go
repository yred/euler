// Problem 101 - Optimum polynomial
//
// If we are presented with the first k terms of a sequence it is impossible to
// say with certainty the value of the next term, as there are infinitely many
// polynomial functions that can model the sequence.
//
// As an example, let us consider the sequence of cube numbers. This is defined by
// the generating function,
//
//             u(n) = n^3: 1, 8, 27, 64, 125, 216, ...
//
// Suppose we were only given the first two terms of this sequence. Working on the
// principle that "simple is best" we should assume a linear relationship and
// predict the next term to be 15 (common difference 7). Even if we were presented
// with the first three terms, by the same principle of simplicity, a quadratic
// relationship should be assumed.
//
// We shall define OP(k, n) to be the nth term of the optimum polynomial
// generating function for the first k terms of a sequence. It should be clear
// that OP(k, n) will accurately generate the terms of the sequence for n ≤ k, and
// potentially the first incorrect term (FIT) will be OP(k, k+1); in which case we
// shall call it a bad OP (BOP).
//
// As a basis, if we were only given the first term of sequence, it would be most
// sensible to assume constancy; that is, for n ≥ 2, OP(1, n) = u(1).
//
// Hence we obtain the following OPs for the cubic sequence:
//
//             OP(1, n) = 1                    1, (1), 1, 1, ...
//             OP(2, n) = 7*n − 6              1, 8, (15), ...
//             OP(3, n) = 6*n^2 − 11*n + 6     1, 8, 27, (58), ...
//             OP(4, n) = n^3                  1, 8, 27, 64, 125, ...
//
// Clearly no BOPs exist for k ≥ 4.
//
// By considering the sum of FITs generated by the BOPs (in parentheses above), we
// obtain 1 + 15 + 58 = 74.
//
// Consider the following tenth degree polynomial generating function:
//
//     u(n) = 1 − n + n^2 − n^3 + n^4 − n^5 + n^6 − n^7 + n^8 − n^9 + n^10
//
// Find the sum of FITs for the BOPs.
package main

import (
	"fmt"
	"math"
)

type polynomial struct {
	coeffs []float64
}

type equation struct {
	left  *polynomial
	right float64
}

func (p *polynomial) Degree() int {
	return len(p.coeffs) - 1
}

func (p *polynomial) Get(idx int) float64 {
	if idx < len(p.coeffs) {
		return p.coeffs[idx]
	}
	return 0
}

func (p *polynomial) Set(idx int, coeff float64) {
	if idx < len(p.coeffs) {
		p.coeffs[idx] = coeff
	} else {
		newCoeffs := make([]float64, idx-len(p.coeffs)+1)
		newCoeffs[len(newCoeffs)-1] = coeff

		p.coeffs = append(p.coeffs, newCoeffs...)
	}
}

func (p *polynomial) Eval(x float64) (result float64) {
	for idx, coeff := range p.coeffs {
		result += coeff * math.Pow(x, float64(idx))
	}
	return
}

func (p *polynomial) Copy() *polynomial {
	return &polynomial{coeffs: append([]float64{}, p.coeffs...)}
}

func (eq *equation) Add(other *equation) *equation {
	newEq := &equation{left: eq.left.Copy(), right: eq.right + other.right}

	if newEq.left.Degree() < other.left.Degree() {
		// "Extend" the equation's left side polynomial
		newEq.left.Set(other.left.Degree(), 0)
	}

	for ix := 0; ix <= newEq.left.Degree(); ix++ {
		newEq.left.Set(ix, newEq.left.Get(ix)+other.left.Get(ix))
	}

	return newEq
}

func (eq *equation) Mul(scalar float64) *equation {
	newEq := &equation{left: eq.left.Copy(), right: scalar * eq.right}
	for ix := 0; ix <= newEq.left.Degree(); ix++ {
		newEq.left.Set(ix, scalar*newEq.left.Get(ix))
	}
	return newEq
}

func (eq *equation) Substitution(idx int) *equation {
	coeff := eq.left.Get(idx)
	if coeff == 0 {
		panic("The substitution term must not equal 0!")
	}

	return eq.Mul(-1.0 / coeff)
}

func main() {
	fmt.Println(solution())
}

func solution() (sum int64) {
	P := &polynomial{coeffs: make([]float64, 11)}
	for ix := range P.coeffs {
		P.coeffs[ix] = math.Pow(-1, float64(ix))
	}

	seq := make([]float64, P.Degree())
	for ix := range seq {
		seq[ix] = P.Eval(float64(ix + 1))
	}

	for ix := range seq {
		sum += int64(optimum(seq[:ix+1]).Eval(float64(ix + 2)))
	}

	return
}

func optimum(seq []float64) *polynomial {
	equations := make([]*equation, len(seq))
	for ix := range equations {
		equations[ix] = &equation{left: &polynomial{}, right: seq[ix]}
		equations[ix].left.coeffs = make([]float64, len(seq))
		for iy := range equations[ix].left.coeffs {
			equations[ix].left.coeffs[iy] = math.Pow(float64(ix+1), float64(iy))
		}
	}

	// Using substitution, transform the above system of equations into a
	// triangular one
	for ix := 0; ix < len(seq)-1; ix++ {
		sub := equations[ix].Substitution(ix)

		for iy := ix + 1; iy < len(seq); iy++ {
			equations[iy] = equations[iy].Add(sub.Mul(equations[iy].left.Get(ix)))
		}
	}

	op := &polynomial{coeffs: make([]float64, len(seq))}

	// Moving from the last equation backwards, solve for the only remaining
	// variable and "plug" its value into the preceding equations
	for ix := len(seq) - 1; ix >= 0; ix-- {
		solution := equations[ix].right / equations[ix].left.Get(ix)

		for iy := 0; iy < ix; iy++ {
			equations[iy].right -= solution * equations[iy].left.Get(ix)
			equations[iy].left.Set(ix, 0)
		}

		op.coeffs[ix] = solution
	}

	return op
}
