# -*- coding: utf-8 -*-
"""
Problem 60 - Prime pair sets

The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes
and concatenating them in any order the result will always be prime. For
example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four
primes, 792, represents the lowest sum for a set of four primes with this
property.

Find the lowest sum for a set of five primes for which any two primes
concatenate to produce another prime.
"""
from itertools import count, combinations, permutations, takewhile

from common import is_prime, primes


def solution():
    base = 1000

    # Keep the same iterator throughout to pull more prime values whenever
    # necessary
    iprimes = primes()

    # Use a set for quick prime lookups
    pset = set()

    for i in count():
        limit = base*(10**i)

        pset.update(takewhile(lambda p: p < limit, iprimes))

        tuples = set((p,) for p in pset)

        for j in range(2, 6):
            new_tuples = set()

            for ts in combinations(tuples, 2):
                t = tuple(sorted(set([e for t in ts for e in t])))

                if len(t) != j:
                    continue

                for a, b in permutations(t, 2):
                    n = int(str(a) + str(b))

                    if n < limit:
                        if n not in pset:
                            break
                    elif not is_prime(n):
                        break
                else:
                    new_tuples.add(t)

            tuples = new_tuples

        if tuples:
            return min((sum(t), t) for t in tuples)


if __name__ == '__main__':
    print(solution())
