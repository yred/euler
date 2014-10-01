# -*- coding: utf-8 -*-
"""
Problem 83 - Path sum: four ways

NOTE: This problem is a significantly more challenging version of Problem 81.

In the 5 by 5 matrix below, the minimal path sum from the top left to the
bottom right, by moving left, right, up, and down, is indicated using
parentheses and is equal to 2297.

                   (131)   673   (234)  (103) ( 18)
                   (201)  ( 96)  (342)   965  (150)
                    630    803   (746)  (422) (111)
                    537    699    497   (121)  956
                    805    732    524   ( 37) (331)

Find the minimal path sum, in "../resources/p083_matrix.txt", a 31K text file
containing a 80 by 80 matrix, from the top left to the bottom right by moving
left, right, up, and down.
"""


class Cell(object):

    def __init__(self, cost, best, predecessor):
        self.cost = cost
        self.best = best
        self.pred = predecessor

    def update(self, other):
        """
        Returns `True` if a move from `other` to `self` results in a better
        path
        """
        if self.best > other.best + self.cost:
            self.best = other.best + self.cost
            self.pred = other
            return True

        return False


def solution():
    matrix = []

    with open('../resources/p082_matrix.txt') as f:
        for line in f.readlines():
            matrix.append(map(int, line.strip().split(',')))

    maxrow = len(matrix) - 1
    maxcol = len(matrix[0]) - 1

    # Upper bound on the path sum
    maxpath = sum(cost for row in matrix for cost in row)

    cells = {(i, j): Cell(cost=matrix[i][j], best=maxpath, predecessor=None)
             for i in range(maxrow+1) for j in range(maxcol+1)}

    # Correct the best estimate for the first cell
    cells[0, 0].best = matrix[0][0]

    # Compute a first estimate of the best path to each cell by restricting
    # possible moves to right and down
    latest = [(0, 0)]

    while latest:
        new = []

        for row, col in latest:
            cell = cells[row, col]

            if row < maxrow and cells[row+1, col].update(cell):
                new.append((row+1, col))

            if col < maxcol and cells[row, col+1].update(cell):
                new.append((row, col+1))

        latest = new

    # Compute the real best path to each cell using continuous updates, until
    # the solution converges for all cells
    while True:
        updated = False

        for row, col in cells:
            cell = cells[row, col]

            # Iterate through all possible moves
            for (dx, dy) in ((-1, 0), (0, -1), (1, 0), (0, 1)):
                try:
                    updated |= cells[row+dx, col+dy].update(cell)
                except KeyError:
                    pass

        if not updated:
            break

    return cells[maxrow, maxcol].best


if __name__ == '__main__':
    print(solution())
