# -*- coding: utf-8 -*-
"""
Problem 166 - Criss Cross

A 4x4 grid is filled with digits d, 0 ≤ d ≤ 9.

It can be seen that in the grid

                                6 3 3 0
                                5 0 4 3
                                0 7 1 4
                                1 2 4 5

the sum of each row and each column has the value 12. Moreover the sum of each
diagonal is also 12.

In how many ways can you fill a 4x4 grid with the digits d, 0 ≤ d ≤ 9 so that
each row, each column, and both diagonals have the same sum?
"""
from collections import defaultdict
from itertools import product


def grids(frow, fcol, fdiag, index):
    length = len(frow)
    tupsum = sum(frow)

    rowsets = [[frow]]
    results = 0

    for rownum in range(1, length):
        rowset = (
            index[tupsum][0][fcol[rownum]] &
            index[tupsum][length-rownum-1][fdiag[rownum]]
        )

        if not rowset:
            return 0

        rowsets.append(list(rowset))

    for tuples in product(*rowsets):
        for colnum in range(1, length):
            if sum(t[colnum] for t in tuples) != tupsum:
                break
        else:
            if sum(t[i] for i, t in enumerate(tuples)) == tupsum:
                results += 1

    return results


def solution():
    length = 4
    tuples = list(product(*([range(10)] * length)))

    sum_index = defaultdict(set)

    # Sum -> Position -> Digit -> Tuple set
    spd_index = defaultdict(lambda: defaultdict(lambda: defaultdict(set)))

    for t in tuples:
        tsum = sum(t)
        sum_index[tsum].add(t)

        for position, digit in enumerate(t):
            spd_index[tsum][position][digit].add(t)

    found = 0
    for tsum, tupleset in sum_index.items():
        for frow in tupleset:
            for fcol in spd_index[tsum][0][frow[0]]:
                fst = spd_index[tsum][0][frow[-1]]
                lst = spd_index[tsum][length - 1][fcol[-1]]

                for fdiag in fst & lst:
                    found += grids(frow, fcol, fdiag, spd_index)

    return found


if __name__ == '__main__':
    print(solution())
