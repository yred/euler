# -*- coding: utf-8 -*-
"""
Problem 39 - Integer right triangles

If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120:

        {20, 48, 52}, {24, 45, 51}, {30, 40, 50}

For which value of p ≤ 1000, is the number of solutions maximised?
"""


def is_right_triangular(a, b, c):
    """
    Returns True if the triangle formed by the sides {a, b, c} is a right
    triangle, where a < b < c
    """
    return a*a + b*b == c*c


def solution():
    minimum = 3
    maximum = 1000

    solutions = {p: 0 for p in range(minimum, maximum+1)}

    for p in range(minimum, maximum+1):
        for c in range(p/3 + 1, p/2):
            for b in range((p - c)/2, c):
                a = p - c - b

                if is_right_triangular(a, b, c):
                    solutions[p] += 1

    return max((count, p) for p, count in solutions.items())[1]


if __name__ == '__main__':
    print(solution())
