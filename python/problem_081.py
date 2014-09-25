# -*- coding: utf-8 -*-
"""
Problem 81 - Path sum: two ways

In the 5 by 5 matrix below, the minimal path sum from the top left to the
bottom right, by only moving to the right and down, is indicated using
parentheses and is equal to 2427.

                   (131)   673    234    103     18
                   (201)  ( 96)  (342)   965    150
                    630    803   (746)  (422)   111
                    537    699    497   (121)   956
                    805    732    524   ( 37)  (331)

Find the minimal path sum, in "../resources/p081_matrix.txt", a 31K text file
containing an 80 by 80 matrix, from the top left to the bottom right by only
moving right and down.
"""
from collections import defaultdict


def solution():
    matrix = []

    with open('../resources/p081_matrix.txt') as f:
        for line in f.readlines():
            matrix.append(map(int, line.strip().split(',')))

    maxrow = len(matrix) - 1
    maxcol = len(matrix[0]) - 1

    paths = {(0, 0): matrix[0][0]}

    while True:
        new_paths = defaultdict(list)

        for path, psum in paths.items():
            row, col = path

            if row < maxrow:
                new_paths[(row+1, col)].append(psum + matrix[row+1][col])

            if col < maxcol:
                new_paths[(row, col+1)].append(psum + matrix[row][col+1])

        paths = {p: min(v) for p, v in new_paths.items()}

        # Given the limited moves (down and right), all paths will eventually
        # converge at the same time
        if len(paths) == 1:
            return paths.values()[0]


if __name__ == '__main__':
    print(solution())
