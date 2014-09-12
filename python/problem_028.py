"""
Problem 28 - Number spiral diagonals

Starting with the number 1 and moving to the right in a clockwise direction a
5 by 5 spiral is formed as follows:

                    21 22 23 24 25
                    20  7  8  9 10
                    19  6  1  2 11
                    18  5  4  3 12
                    17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
in the same way?
"""
from itertools import count, islice, takewhile


def spiral_diagonals():
    """Returns an infinite spiral's diagonal elements"""

    yield 1

    spiral = count(2)
    skip = 1

    while True:
        for _ in range(4):
            yield next(islice(spiral, skip, skip+1))

        skip += 2


def solution():
    return sum(takewhile(lambda n: n <= 1001**2, spiral_diagonals()))


if __name__ == '__main__':
    print(solution())
