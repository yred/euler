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


def factorize(n):
    factors = []

    if n > 1:
        for p in primes():
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
