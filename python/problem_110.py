# -*- coding: utf-8 -*-
"""
Problem 110 - Diophantine reciprocals II

In the following equation x, y, and n are positive integers:

            1/x + 1/y = 1/n

It can be verified that when n = 1260 there are 113 distinct solutions and this
is the least value of n for which the total number of distinct solutions
exceeds one hundred.

What is the least value of n for which the number of distinct solutions exceeds
four million?

NOTE: This problem is a much more difficult version of Problem 108 and as it is
well beyond the limitations of a brute force approach it requires a clever
implementation.
"""
from itertools import count, islice, takewhile
from math import log

from common import primes, product


def distinct(factors):
    """
    Returns the number of distinct solutions to the equation:

                1/x + 1/y = 1/n

    where n is the integer whose prime factorization corresponds to the
    `factors` dict (a mapping between primes and their multiplicities)
    """
    # Uses the fact that the equation 1/x + 1/y = 1/n is equivalent to:
    #
    #           (x - n)*(y - n) = n^2
    #
    # and that the prime factorization of n^2 is the same as that of n,
    # except that each prime's multiplicity is doubled.
    return (product(2*exp + 1 for exp in factors.values()) + 1) / 2


def solution():
    # Note: same method as problem 108
    threshold = 4*10**6

    # Get the maximum number of primes that would be required to find the least
    # value of `n` (using the same reasoning as that of the distinct()
    # function)
    factors = list(islice(primes(), 0, log(threshold, 2) + 1))

    # Order the prime factors, and add terms with multiplicities greater than
    # 1 that are smaller than the largest retrieved prime
    factors = sorted([(p**i, p, i)
                      for p in factors
                      for i in takewhile(lambda n: p**n <= max(factors),
                                         count(1))])

    # Prime factorization mapping for potential results, producing strictly
    # increasing values (before minimization)
    least = {}

    results = []

    for idx, (_, p, i) in enumerate(factors):
        # Increase the multiplicity of p
        least[p] = i

        if distinct(least) > threshold:
            # Make a new copy of the factorization dictionary before the
            # minimization process
            result = dict(least.items())

            # Keep track of prime factors whose multiplicity has been finalized
            checked = set()

            for _, prime, _ in reversed(factors[:idx]):
                if prime in checked:
                    continue

                # Attempt reducing the multiplicity of `prime`...
                result[prime] -= 1

                # ... and backtrack if necessary
                if distinct(result) <= threshold:
                    result[prime] += 1

                    # Finalize `prime`'s multiplicity
                    checked.add(prime)

            results.append(product(b**e for b, e in result.items()))

    return min(results)


if __name__ == '__main__':
    print(solution())
