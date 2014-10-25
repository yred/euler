package common

import (
	"math"
	"math/big"
	"strconv"
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

func PrimeSetUpTo(limit int) map[int]struct{} {
	primes := make(map[int]struct{})

	var empty struct{}

	primes[2] = empty

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
			primes[2*(ix+1)+1] = empty
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

func Sum(nums []int) int {
	sum := 0
	for _, n := range nums {
		sum += n
	}
	return sum
}

func Product(nums []int) int {
	product := 1
	for _, n := range nums {
		product *= n
	}
	return product
}

func Factorial(n int) *big.Int {
	return big.NewInt(0).MulRange(1, int64(n))
}

func Permutations(whole int, part int) *big.Int {
	return big.NewInt(0).Div(Factorial(whole), Factorial(part))
}

func Digits(n int) []int {
	if n < 0 {
		n = -n
	}

	digits := make([]int, int64(math.Log10(float64(n)))+1)

	for ix, d := range strconv.Itoa(n) {
		digits[ix] = int(byte(d) - '0')
	}

	return digits
}

func SumDigits(str string) (sum int) {
	for i := 0; i < len(str); i++ {
		sum += int(byte(str[i]) - '0')
	}
	return
}
