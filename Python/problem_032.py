# -*- coding: utf-8 -*-
"""
Problem 32 - Pandigital products

We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity
can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only
include it once in your sum.
"""
from itertools import permutations


def get_products(multis):
    """
    Yields the products of any 2 adjacent numbers that span `multis`
    """

    # Independents can swing either way
    dem, independent, rep = multis[0], multis[1:-1], multis[-1]

    for i in range(len(independent)):
        multiplicand, multiplier = map(int, (dem + independent[:i],
                                             independent[i:] + rep))
        yield multiplicand*multiplier


def solution():
    pandigitals = set()

    for s in map(''.join, permutations('123456789')):

        # Through basic reasoning, it can be shown that any viable product in a
        # 1 through 9 pandigital must be of length 4
        multis, product = s[:5], int(s[5:])

        if product in pandigitals:
            continue

        if product in get_products(multis):
            pandigitals.add(product)

    return sum(pandigitals)


if __name__ == '__main__':
    print(solution())
