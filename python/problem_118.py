# -*- coding: utf-8 -*-
"""
Problem 118 - Pandigital prime sets

Using all of the digits 1 through 9 and concatenating them freely to form
decimal integers, different sets can be formed. Interestingly with the set
{2,5,47,89,631}, all of the elements belonging to it are prime.

How many distinct sets containing each of the digits one through nine exactly
once contain only prime elements?
"""
from collections import defaultdict
from itertools import permutations, product

from common import primes_up_to


DIGITS = '123456789'


def distinct(length, digits=DIGITS):
    """
    Returns all the numbers of length `length` containing distinct digits from
    `digits`
    """
    return (int(''.join(p)) for p in permutations(digits, length))


def is_distinct(n):
    """Returns `True` if all of `n`'s digits are distinct"""
    nstr = str(n)
    return len(nstr) == len(set(nstr))


def binary_sums(start, limit):
    """Yields the binary partitions of all integers in [start, limit)"""
    for n in range(start, limit):
        for i in range(1, n/2 + 1):
            yield i, n - i


def solution():
    # Maximum length of any pandigital prime number (9-digit pandigitals are
    # all multiples of 3)
    maxl = 8

    # Upper bound on the prime factors of all numbers up to the largest
    # pandigital (987654321)
    maxp = int(10**(maxl/2))

    plst = list(primes_up_to(maxp))
    pset = set(plst)

    # Lengths of potential pandigital prime set members that have not been yet
    # accounted for
    rem_lengths = range(maxl/2 + 1, maxl+1)

    is_prime = lambda n: (n <= maxp and n in pset) or all(n % p for p in plst)

    # `primes` is the list of all primes that can be part of the pandigital
    # prime sets
    primes = filter(is_distinct, plst)
    primes.extend(n for l in rem_lengths for n in distinct(l) if is_prime(n))

    # Mapping between: length => digits => prime tuples
    pindex = defaultdict(lambda: defaultdict(set))

    # Mapping between lengths and primes
    lindex = defaultdict(set)

    # Create indexes on the list of valid primes
    for p in primes:
        key = ''.join(sorted(str(p)))

        # 0 should not be a digit in any of the primes of the researched sets
        if key[0] == '0':
            continue

        pindex[len(key)][key].add((p,))
        lindex[len(key)].add(p)

    # Iteratively combine prime tuples into larger groups containing more
    # digits
    for a, b in binary_sums(2, maxl):
        for ka, kb in product(pindex[a], pindex[b]):
            key = ''.join(sorted(set(ka + kb)))

            # Skip if the keys `ka` and `kb` share any digits
            if len(key) < a + b:
                continue

            pindex[a + b][key].update(
                tuple(sorted(ta + tb))
                for ta in pindex[a][ka] for tb in pindex[b][kb]
            )

    # Pandigital prime sets
    psets = set()

    for length in range(1, maxl):
        for p in lindex[length]:
            missing = ''.join(sorted(d for d in DIGITS if d not in str(p)))

            psets.update(
                tuple(sorted(t + (p,))) for t in pindex[len(missing)][missing]
            )

    return len(psets)


if __name__ == '__main__':
    print(solution())
