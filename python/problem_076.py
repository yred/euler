# -*- coding: utf-8 -*-
"""
Problem 76 - Counting summations

It is possible to write five as a sum in exactly six different ways:

        4 + 1
        3 + 2
        3 + 1 + 1
        2 + 2 + 1
        2 + 1 + 1 + 1
        1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two
positive integers?
"""


def solution():
    n = 100

    # sums[a][b] is the number of sums of `a` consisting of numbers ranging
    # from 0 through `b`
    sums = {1: [0, 1]}

    for i in range(2, n+1):
        sums[i] = [0]

        # In each iteration, `j` will be the leading, or greatest, number in
        # the summations to be considered (i.e., the leftmost in the examples
        # above)
        for j in range(1, i):
            difference = i - j

            sums[i].append(sums[i][-1] + sums[difference][min(j, difference)])

        sums[i].append(sums[i][-1] + 1)

    # Disregard the only sum containing 0 (n = n + 0)
    return sums[n][n] - 1


if __name__ == '__main__':
    print(solution())
