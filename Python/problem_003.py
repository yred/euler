"""
Problem 3 - Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143?
"""
import common


def factorize(n):
    factors = []

    if n > 1:
        for p in common.primes():
            while n % p == 0:
                factors.append(p)
                n = n / p

            if n == 1:
                break

    return factors


def solution():
    return max(factorize(600851475143))


if __name__ == '__main__':
    print(solution())
