# -*- coding: utf-8 -*-
"""
Problem 106 - Special subset sums: meta-testing

Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

        S(B) ≠ S(C); that is, sums of subsets cannot be equal.
        If B contains more elements than C then S(B) > S(C).

For this problem we shall assume that a given set contains n strictly
increasing elements and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained from a
set for which n = 4, only 1 of these pairs need to be tested for equality
(first rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need
to be tested.

For n = 12, how many of the 261625 subset pairs that can be obtained need to be
tested for equality?

NOTE: This problem is related to Problem 103 and Problem 105.
"""
from itertools import combinations


def required_tests(n):
    """
    Returns the number of required tests to check if a set containing `n`
    strictly increasing elements and satisfying the second rule of special sum
    sets is in fact a special sum set (i.e., also satisfies the first rule)
    """
    required = 0

    # Since the set contains `n` strictly increasing elements, these can be
    # identified via their position in the set
    elements = set(range(n))

    # Since the tests/comparisons must be between non-trivial sets of equal
    # size, these sets must contain at most n/2 elements
    for size in range(2, n/2 + 1):
        for c1 in combinations(elements, size):
            max1 = c1[-1]

            others = elements - set(c1)
            for c2 in combinations(others, size):
                max2 = c2[-1]

                # The any() comparison uses the fact that c1 and c2 are sorted,
                # and basically checks if the sets/tuples "cross" at any point
                if max1 > max2 and any(e2 > e1 for e1, e2 in zip(c1, c2)):
                    required += 1

    return required


def solution():
    return required_tests(12)


if __name__ == '__main__':
    print(solution())
