# -*- coding: utf-8 -*-
"""
Problem 82 - Path sum: three ways

NOTE: This problem is a more challenging version of Problem 81.

The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the
left column and finishing in any cell in the right column, and only moving up,
down, and right, is indicated is indicated using parentheses and is equal to
994.

             131   673  (234) (103) ( 18)
            (201) ( 96) (342)  965   180
             630   803   746   422   111
             537   699   497   121   956
             805   732   524    37   331

Find the minimal path sum, in "../resources/p082_matrix.txt", a 31K text file
containing an 80 by 80 matrix, from the left column to the right column.
"""


def solution():
    matrix = []

    with open('../resources/p082_matrix.txt') as f:
        for line in f.readlines():
            matrix.append(map(int, line.strip().split(',')))

    nrows = len(matrix)
    ncols = len(matrix[0])

    paths = {(i, 0): matrix[i][0] for i in range(nrows)}

    # Find the global solution by finding the best path sum at each column
    for col in range(1, ncols):

        # Initialize all paths assuming a single rightward move
        new_paths = {(i, col): [paths[(i, col-1)] + matrix[i][col]]
                     for i in range(nrows)}

        # Account for possible vertical moves
        for i in range(nrows):
            for j in range(nrows):
                if i == j:
                    continue

                vertical = sum(matrix[k][col]
                               for k in range(i, j, 1 if i < j else -1))

                new_paths[(i, col)].append(new_paths[(j, col)][0] + vertical)

        # Only keep the best path for each cell in the current column
        paths = {p: min(v) for p, v in new_paths.items()}

    return min(paths.values())


if __name__ == '__main__':
    print(solution())
