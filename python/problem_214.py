# -*- coding: utf-8 -*-
"""
Problem 214 - Totient Chains

Let φ be Euler's totient function, i.e. for a natural number n, φ(n) is the number
of k, 1 ≤ k ≤ n, for which gcd(k,n) = 1.

By iterating φ, each positive integer generates a decreasing chain of numbers
ending in 1.

E.g. if we start with 5 the sequence 5,4,2,1 is generated.

Here is a listing of all chains with length 4:

        5,4,2,1
        7,6,2,1
        8,4,2,1
        9,6,2,1
        10,4,2,1
        12,4,2,1
        14,6,2,1
        18,6,2,1

Only two of these chains start with a prime, their sum is 12.

What is the sum of all primes less than 40000000 which generate a chain of length 25?
"""
from collections import defaultdict

from common import primes_up_to, OrderedSet


def phi(factors):
    result = 1

    for p, m in factors.iteritems():
        result *= (p-1) * (p**(m-1))

    return result


def decompose(n, primes):
    for p in primes:
        if n == 1:
            return

        if n in primes:
            yield n
            break

        while n % p == 0:
            yield p
            n /= p


def solution():
    target = 25

    limit = 40 * 10**6
    primes = OrderedSet(primes_up_to(limit))

    totient, chainlen = {}, {}

    for p in primes:

        to = totient[p] = p - 1
        chain = [p, to]

        length = 2
        while to > 1:
            if to in chainlen:
                length += chainlen[to] - 1
                break

            if to not in totient:
                factors = defaultdict(int)
                for f in decompose(to, primes):
                    factors[f] += 1

                totient[to] = phi(factors)

            to = totient[to]
            chain.append(to)

            length += 1

        for d, n in enumerate(chain):
            chainlen[n] = length - d

    return sum(p for p in primes if chainlen[p] == target)


if __name__ == '__main__':
    print(solution())
