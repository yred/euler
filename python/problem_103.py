# -*- coding: utf-8 -*-
"""
Problem 103 - Special subset sums: optimum

Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

            S(B) ≠ S(C); that is, sums of subsets cannot be equal.
            If B contains more elements than C then S(B) > S(C).

If S(A) is minimised for a given n, we shall call it an optimum special sum
set. The first five optimum special sum sets are given below.

            n = 1: {1}
            n = 2: {1, 2}
            n = 3: {2, 3, 4}
            n = 4: {3, 5, 6, 7}
            n = 5: {6, 9, 11, 12, 13}

It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum
set is of the form B = {b, a1 + b, a2 + b, ... , an + b}, where b is the
"middle" element on the previous row.

By applying this "rule" we would expect the optimum set for n = 6 to be
A = {11, 17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the optimum
set, as we have merely applied an algorithm to provide a near optimum set. The
optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and
corresponding set string: 111819202225.

Given that A is an optimum special sum set for n = 7, find its set string.

NOTE: This problem is related to Problem 105 and Problem 106.
"""
from collections import defaultdict
from itertools import combinations

from common import ncr


def is_special_sum_set(ns):
    """
    Returns `True` if the set of (natural) numbers `ns` fulfills both of the
    properties of special sum sets, as outlined in the problem statement
    """
    subsums = defaultdict(set)

    for length in range(1, len(ns)):
        for c in combinations(ns, length):
            subsums[len(c)].add(sum(c))

    # Test if subset sums are unique
    for length, sums in subsums.items():
        if ncr(len(ns), length) != len(sums):
            return False

    # Test if subset sums strictly grow with set size
    for length in sorted(subsums)[:-1]:
        if not max(subsums[length]) < min(subsums[length+1]):
            return False

    return True


def solution():
    target = 7

    # Optimum special sum sets
    osets = {1: [1]}

    for n in range(2, target+1):
        prevset = osets[n-1]
        leading = prevset[len(prevset)/2]

        # Use the algorithm described in the problem statement to produce a
        # near optimum set
        best = [leading] + [leading + e for e in prevset]

        min_second = leading + 1
        max_second = (sum(best) - leading) / (n-1)

        # Possible elements besides the first/leading element. The limit on the
        # last element is derived from the properties of special sum sets, and
        # specifically the following corollary:
        #
        #   If a1, a2, ... , aN are the ordered elements of osets[N], then
        #   for i, j in [2, n], where i < j:
        #
        #               aj - ai <= an - a2 < a1
        #
        elements = range(min_second, max_second + leading)

        for c in combinations(elements, n-1):
            # Uses the fact that the returned combinations are sorted
            if not c[-1] - c[0] < leading:
                continue

            c = [leading] + list(c)

            if sum(c) < sum(best) and is_special_sum_set(c):
                best = c

        # Note: `best` is already sorted
        osets[n] = best

    return ''.join(map(str, osets[target]))


if __name__ == '__main__':
    print(solution())
