# -*- coding: utf-8 -*-
"""
Problem 95 - Amicable chains

The proper divisors of a number are all the divisors excluding the number
itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the
sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of the
proper divisors of 284 is 220, forming a chain of two numbers. For this reason,
220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496, we
form a chain of five numbers:

            12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

Since this chain returns to its starting point, it is called an amicable chain.

Find the smallest member of the longest amicable chain with no element
exceeding one million.
"""
from common import divisors


def sumdivs(n):
    """Returns the sum of the proper divisors of a natural number n"""
    return sum(divisors(n)[:-1])


def solution():
    limit = 10**6 + 1

    amicables = {}

    # Keep pre-processed values in a cache
    sums = {}

    for n in range(1, limit):
        # Skip pre-processed values
        if n in sums:
            continue

        sums[n] = sd = sumdivs(n)

        # If n >= sumdivs(n) and belongs to an amicable chain, then n must have
        # already been processed. Otherwise, n definetely doesn't belong to an
        # amicable chain
        if n < sd:
            chain = [n]

            while n < sd < limit:
                chain.append(sd)

                sums[chain[-1]] = sd = sumdivs(chain[-1])

                # In case a chain which starts midway is discovered
                if sd in chain:
                    chain = chain[chain.index(sd):]

                    # The keys of `amicables` are always the minimum value in
                    # the chain
                    amicables[min(chain)] = chain

                    break

            # A chain has been found only if sd == n, and n is the minimum
            # value in the chain
            if sd == n:
                amicables[n] = chain

    return max((len(c), k) for k, c in amicables.items())[1]


if __name__ == '__main__':
    print(solution())
