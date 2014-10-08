# -*- coding: utf-8 -*-
"""
Problem 105 - Special subset sums: testing

Let S(A) represent the sum of elements in set A of size n. We shall call it a
special sum set if for any two non-empty disjoint subsets, B and C, the
following properties are true:

        S(B) ≠ S(C); that is, sums of subsets cannot be equal.
        If B contains more elements than C then S(B) > S(C).

For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because

        65 + 87 + 88 = 75 + 81 + 84

whereas {157, 150, 164, 119, 79, 159, 161, 139, 158} satisfies both rules for
all possible subset pair combinations and S(A) = 1286.

Using "../resources/p105_sets.txt", a 4K text file with
one-hundred sets containing seven to twelve elements (the two examples given
above are the first two sets in the file), identify all the special sum sets,
A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).

NOTE: This problem is related to Problem 103 and Problem 106.
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
    with open('../resources/p105_sets.txt') as f:
        sets = map(lambda line: map(int, line.strip().split(',')),
                   f.readlines())

    return sum(sum(A) for A in sets if is_special_sum_set(A))


if __name__ == '__main__':
    print(solution())
