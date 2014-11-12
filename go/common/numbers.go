package common

import (
	"math"
)

func PrimesUpTo(limit int) (primes []int) {
	primes = append(primes, 2)

	sieve := NewBitSet((limit - 1) / 2)
	sieve.SetAll(true)

	// Set non-primes to false, using the sieve of Sundaram
	for i := 1; i < int(math.Sqrt(float64(limit/2)))+1; i++ {
		for j := 1; j < (limit-i)/(1+2*i)+1; j++ {
			sieve.Set(i+j+(2*i*j)-1, false)
		}
	}

	sieve.Iterate(func(ix int, val bool) {
		if val {
			// The sieve is 0-indexed, which is why the returned index `ix` must
			// first be augmented by 1
			primes = append(primes, 2*(ix+1)+1)
		}
	})

	return
}

func PrimeSetUpTo(limit int) map[int]bool {
	primes := make(map[int]bool)

	primes[2] = true

	sieve := NewBitSet((limit - 1) / 2)
	sieve.SetAll(true)

	// Set non-primes to false, using the sieve of Sundaram
	for i := 1; i < int(math.Sqrt(float64(limit/2)))+1; i++ {
		for j := 1; j < (limit-i)/(1+2*i)+1; j++ {
			sieve.Set(i+j+(2*i*j)-1, false)
		}
	}

	sieve.Iterate(func(ix int, val bool) {
		if val {
			primes[2*(ix+1)+1] = true
		}
	})

	return primes
}

func IsPrime(n int) bool {
	for i := 2; i <= int(math.Sqrt(float64(n))); i++ {
		if n%i == 0 {
			return false
		}
	}
	return true
}

func GCD(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// Returns the sorted list of the divisors of n
func Divisors(n int) []int {
	maxIterations := int(math.Sqrt(float64(n)))

	maxDivs := 2 * maxIterations
	divisors := make([]int, maxDivs)

	// Forward and back indexes
	fIndex, bIndex := 0, 0

	for d := 1; d <= maxIterations; d++ {
		if n%d == 0 {
			divisors[fIndex] = d
			fIndex++

			if d != n/d {
				divisors[maxDivs-bIndex-1] = n / d
				bIndex++
			}
		}
	}

	return append(divisors[:fIndex], divisors[maxDivs-bIndex:]...)
}

func Factors(n int, primes []int) map[int]int {
	if len(primes) == 0 {
		primes = PrimesUpTo(n)
	}

	ncopy := n

	factors := make(map[int]int)
	for _, p := range primes {
		if p*p > n {
			if ncopy > 1 {
				factors[ncopy] = 1
			}
			break
		}

		if ncopy%p == 0 {
			for ncopy%p == 0 {
				factors[p]++
				ncopy /= p
			}
		}

		if ncopy == 1 {
			break
		}
	}

	return factors
}

// Returns the value of Euler's totient function for integers in [2, limit]
func Totient(limit int, primes []int) map[int]int {
	if len(primes) == 0 {
		primes = PrimesUpTo(limit)
	}

	totients := make(map[int]int)

	// Compute the totient of prime powers
	for _, p := range primes {
		power := p
		for power <= limit {
			totients[power] = power - (power / p)
			power *= p
		}
	}

	for n := 2; n <= limit; n++ {
		if _, ok := totients[n]; ok {
			continue
		}

		totients[n] = 1

		factors := Factors(n, primes)
		for prime, power := range factors {
			totients[n] *= totients[Power(prime, power)]
		}
	}

	return totients
}
