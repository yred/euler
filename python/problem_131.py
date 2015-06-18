# -*- coding: utf-8 -*-
"""
Problem 131 - Prime cube partnership

There are some prime values, p, for which there exists a positive integer, n,
such that the expression n^3 + n²p is a perfect cube.

For example, when p = 19, 8^3 + 8²×19 = 12^3.

What is perhaps most surprising is that for each prime with this property the
value of n is unique, and there are only four such primes below one-hundred.

How many primes below one million have this remarkable property?
"""
from itertools import count

from common import primes_up_to


def solution():
    maxprime = 10**6
    primeset = set(primes_up_to(maxprime))

    primes = []
    for k in count(1):
        # Uses the fact that the first 4 primes with the described property
        # exhibit the following pattern:
        #   n = k^3
        #   m = k^3 + k² where m^3 = n^3 + n²*p
        #
        # Consequently:
        #   p = (m^3 - n^3)/n² = 3*k² + 3*k + 1
        #
        potential_prime = 3*k*k + 3*k + 1

        if potential_prime >= maxprime:
            break

        if potential_prime in primeset:
            primes.append(potential_prime)

    return len(primes)


if __name__ == '__main__':
    print(solution())
