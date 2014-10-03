# -*- coding: utf-8 -*-
"""
Problem 96 - Su Doku

Su Doku (Japanese meaning number place) is the name given to a popular puzzle
concept. Its origin is unclear, but credit must be attributed to Leonhard Euler
who invented a similar, and much more difficult, puzzle idea called Latin
Squares. The objective of Su Doku puzzles, however, is to replace the blanks
(or zeros) in a 9 by 9 grid in such that each row, column, and 3 by 3 box
contains each of the digits 1 to 9. Below is an example of a typical starting
puzzle grid and its solution grid.

            0 0 3 | 0 2 0 | 6 0 0            4 8 3 | 9 2 1 | 6 5 7
            9 0 0 | 3 0 5 | 0 0 1            9 6 7 | 3 4 5 | 8 2 1
            0 0 1 | 8 0 6 | 4 0 0            2 5 1 | 8 7 6 | 4 9 3
            ---------------------            ---------------------
            0 0 8 | 1 0 2 | 9 0 0            5 4 8 | 1 3 2 | 9 7 6
            7 0 0 | 0 0 0 | 0 0 8            7 2 9 | 5 6 4 | 1 3 8
            0 0 6 | 7 0 8 | 2 0 0            1 3 6 | 7 9 8 | 2 4 5
            ---------------------            ---------------------
            0 0 2 | 6 0 9 | 5 0 0            3 7 2 | 6 8 9 | 5 1 4
            8 0 0 | 2 0 3 | 0 0 9            8 1 4 | 2 5 3 | 7 6 9
            0 0 5 | 0 1 0 | 3 0 0            6 9 5 | 4 1 7 | 3 8 2

A well constructed Su Doku puzzle has a unique solution and can be solved by
logic, although it may be necessary to employ "guess and test" methods in order
to eliminate options (there is much contested opinion over this). The
complexity of the search determines the difficulty of the puzzle; the example
above is considered easy because it can be solved by straight forward direct
deduction.

The 6K text file, "../resources/p096_sudoku.txt", contains fifty different
Su Doku puzzles ranging in difficulty, but all with unique solutions (the first
puzzle in the file is the example above).

By solving all fifty puzzles find the sum of the 3-digit numbers found in the
top left corner of each solution grid; for example, 483 is the 3-digit number
found in the top left corner of the solution grid above.
"""


class InvalidPuzzle(Exception):
    def __init__(self, msg):
        super(InvalidPuzzle, self).__init__(msg)


class ImpossiblePuzzle(Exception):
    pass


class Puzzle(object):
    """
    Represents a Sudoku solver

    This solution is heavily-inspired by the Sudoku solver at the end of
    Chapter 1 of "The Ruby Programming Language", by David Flanagan and
    Yukihiro Matsumoto
    """

    # Set of valid final values (empty cells are assigned a value of 0)
    digits = set(range(1, 10))

    def __init__(self, values):
        if len(values) != 81:
            raise InvalidPuzzle('Exactly 81 values must be provided')

        if not 0 <= min(values) < max(values) <= 9:
            raise InvalidPuzzle('All provided values must be between 0 and 9')

        self._vals = values

        # Collect finalized values of each row, column and block
        self._rows = [set(self.row(i)) - set([0]) for i in range(9)]
        self._cols = [set(self.column(i)) - set([0]) for i in range(9)]
        self._blks = [set(self.block(i)) - set([0]) for i in range(9)]

    def row(self, n):
        return self[9*n:9*(n+1)]

    def column(self, n):
        return [self[9*i+n] for i in range(9)]

    def block(self, n):
        # Row and column of the top-left corner
        r, c = (n/3) * 3, (n % 3) * 3
        return [self[9*(r+i) + (c+j)] for i in range(3) for j in range(3)]

    def __contains__(self, item):
        return item in self._vals

    def __iter__(self):
        return iter(self._vals)

    def __getitem__(self, index):
        return self._vals[index]

    def __setitem__(self, index, value):
        self._vals[index] = value

    def solve(self):
        # Keep a list of (index, possible values) for unresolved cells
        unresolved = []

        while True:
            updated = False

            for idx, val in enumerate(self):
                if val > 0:
                    continue

                row = self._rows[idx / 9]
                col = self._cols[idx % 9]
                blk = self._blks[(idx/27)*3 + (idx % 9)/3]

                possibilities = self.digits - row - col - blk

                if not possibilities:
                    raise ImpossiblePuzzle()

                if len(possibilities) > 1:
                    unresolved.append((idx, list(possibilities)))
                else:
                    value = possibilities.pop()

                    self[idx] = value

                    row.add(value)
                    col.add(value)
                    blk.add(value)

                    updated = True

            # Check if there are any more empty cells
            if 0 not in self:
                return self

            if not updated:
                break
            else:
                unresolved = []

        index, possibilities = min(unresolved, key=lambda t: len(t[1]))

        for guess in possibilities:
            new_vals = self[:]
            new_vals[index] = guess

            copy = Puzzle(new_vals)

            try:
                return copy.solve()
            except ImpossiblePuzzle:
                pass

        raise ImpossiblePuzzle()

    def __str__(self):
        return '\n'.join(' '.join(map(str, self.row(i))) for i in range(9))


def solution():
    with open('../resources/p096_sudoku.txt') as f:
        lines = [line.strip() for line in f.readlines()]

    # Keep track of the sum of the first 3 digits of the first row of each
    # solution
    first3sum = 0

    for i in range(0, len(lines), 10):
        try:
            sol = Puzzle(map(int, ''.join(lines[i+1:i+10]))).solve()
            first3sum += reduce(lambda a, b: a*10 + b, sol[:3], 0)
        except ImpossiblePuzzle:
            print('Error: could not solve puzzle number %d' % i/10)

    return first3sum


if __name__ == '__main__':
    print(solution())
