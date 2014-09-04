"""
Problem 3 - Largest prime factor

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143?
"""
from itertools import count
from math import sqrt


def is_prime(n):
    if n <= 1:
        return False

    for i in range(2, int(sqrt(n)) + 1):
        if n % i == 0:
            return False

    return True


def primes():
    yield 2

    for n in count(3, 2):
        if is_prime(n):
            yield n


def largest_prime_factor(n):
    factors = set()

    for p in primes():
        if n % p == 0:
            factors.add(p)

            while n % p == 0:
                n = n / p

        if n == 1:
            break

    return max(factors)


def solution():
    return largest_prime_factor(600851475143)


if __name__ == '__main__':
    print(solution())
