# -*- coding: utf-8 -*-
"""
Problem 90 - Cube digit pairs

Each of the six faces on a cube has a different digit (0 to 9) written on it;
the same is done to a second cube. By placing the two cubes side-by-side in
different positions we can form a variety of 2-digit numbers.

For example, the square number 64 could be formed by:

            [6][4] (https://projecteuler.net/project/images/p090.gif)

In fact, by carefully choosing the digits on both cubes it is possible to
display all of the square numbers below one-hundred:

            01, 04, 09, 16, 25, 36, 49, 64, and 81

For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on
one cube and {1, 2, 3, 4, 8, 9} on the other cube.

However, for this problem we shall allow the 6 or 9 to be turned upside-down so
that an arrangement like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for
all nine square numbers to be displayed; otherwise it would be impossible to
obtain 09.

In determining a distinct arrangement we are interested in the digits on each
cube, not the order.

            {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
            {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}

But because we are allowing 6 and 9 to be reversed, the two distinct sets in
the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the
purpose of forming 2-digit numbers.

How many distinct arrangements of the two cubes allow for all of the square
numbers to be displayed?
"""
from itertools import combinations


def digits(elements):
    """
    Returns the set of digits in `elements`, completing the set with 6 or 9
    whenever only one of the two exists
    """
    if (6 in elements) ^ (9 in elements):
        return set(elements + (6, 9))

    return set(elements)


def solution():
    # Digit tuples for all squares under 100
    squares = [(n*n / 10, n*n % 10) for n in range(1, 10)]

    # The required set of digits for both cubes
    required = set(digit for square in squares for digit in square)

    arrangements = 0

    for c1 in combinations(range(10), 6):
        set1 = digits(c1)

        for c2 in combinations(range(10), 6):
            # Skip duplicate/permuted arrangements
            if c1 < c2:
                continue

            set2 = digits(c2)

            if required <= set1.union(set2):
                if all(all(a in s or b in s for s in (set1, set2))
                       for a, b in squares):
                    arrangements += 1

    return arrangements


if __name__ == '__main__':
    print(solution())
