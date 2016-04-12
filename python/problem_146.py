# -*- coding: utf-8 -*-
"""
Problem 146 - Investigating a Prime Pattern

The smallest positive integer n for which the numbers n²+1, n²+3, n²+7, n²+9,
n²+13, and n²+27 are consecutive primes is 10. The sum of all such integers n
below one-million is 1242490.

What is the sum of all such integers n below 150 million?
"""
from random import randint


def probably_prime(n, iterations=5):
    """
    Returns `True` if `n` is probably a prime number, using Fermat's little
    theorem
    """
    for _ in range(iterations):
        a = randint(2, n-1)

        if pow(a, n-1, n) != 1:
            return False

    return True


def solution():
    limit = 15*10**7
    numbers = []

    # Filter out candidates using `n modulo p`, where `p` is a small prime
    sieve = (
        (7, [False, False, False, True, True, False, False]),
        (11, [True, True, False, False, True, True, True, True, False, False, True]),
        (13, [False, True, False, True, True, False, False, False, False, True, True, False, True]),
    )

    for n in xrange(10, limit, 10):
        if n % 3 == 0:
            continue

        if not all(rems[n % p] for p, rems in sieve):
            continue

        for delta in 1, 3, 7, 9, 13, 27:
            if not probably_prime(n*n + delta):
                break
        else:
            # 21 is the only delta value that may interrupt the sequence
            if not probably_prime(n*n + 21, iterations=10):
                numbers.append(n)

    return sum(numbers)


if __name__ == '__main__':
    print(solution())
