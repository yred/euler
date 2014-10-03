# -*- coding: utf-8 -*-
"""
Problem 88 - Product-sum numbers

A natural number, N, that can be written as the sum and product of a given set
of at least two natural numbers, {a1, a2, ... , ak} is called a product-sum
number:

            N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.

For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.

For a given set of size, k, we shall call the smallest N with this property a
minimal product-sum number. The minimal product-sum numbers for sets of size,
k = 2, 3, 4, 5, and 6 are as follows.

            k=2: 4 = 2 × 2 = 2 + 2
            k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
            k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
            k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
            k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6

Hence for 2 ≤ k ≤ 6, the sum of all the minimal product-sum numbers is

            4 + 6 + 8 + 12 = 30

Note that 8 is only counted once in the sum.

In fact, as the complete set of minimal product-sum numbers for 2 ≤ k ≤ 12 is
{4, 6, 8, 12, 15, 16}, the sum is 61.

What is the sum of all the minimal product-sum numbers for 2 ≤ k ≤ 12000?
"""
from collections import defaultdict

from common import divisors, primes_up_to


def product_sums(limit):
    """Returns a dictionary mapping (sequence) lengths to product-sum tuples"""

    # Since primes can't be expressed in terms of any interesting products,
    # they're collected now to be rapidly skipped later
    primes = set(primes_up_to(limit))

    products = defaultdict(set)

    for n in range(2, limit):
        if n in primes:
            continue

        # Iterate over n's divisors, skipping the trivial ones: 1 and n
        for d in divisors(n)[1:-1]:
            # If true, all divisor pairs have been collected at this point
            if d > n/d:
                break

            # Collect tuples containing d and whose product equals n
            products[n].add(tuple(sorted((d, n/d))))

            # Include n/d's own product tuples, if available
            for t in products.get(n/d, []):
                products[n].add(tuple(sorted(((d,) + t))))

    # length => product-sum => list of concise tuples (w/o leading 1's)
    prodsums = defaultdict(lambda: defaultdict(list))

    # Index product-sum tuples by length
    for product, tuples in products.items():
        for t in tuples:
            diff = product - sum(t)
            if diff >= 0:
                prodsums[len(t) + diff][product].append(t)

    return prodsums


def solution():
    # It's necessary to "overshoot" a bit to produce product sums for the last
    # few numbers
    p = product_sums(13000)

    return sum(set(min(p[n]) for n in range(2, 12001)))


if __name__ == '__main__':
    print(solution())
