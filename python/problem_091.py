# -*- coding: utf-8 -*-
"""
Problem 91 - Right triangles with integer coordinates

The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and
are joined to the origin, O(0,0), to form ΔOPQ.

            (https://projecteuler.net/project/images/p091_1.gif)

There are exactly fourteen triangles containing a right angle that can be
formed when each co-ordinate lies between 0 and 2 inclusive; that is,
0 ≤ x1, y1, x2, y2 ≤ 2.

            (https://projecteuler.net/project/images/p091_2.gif)

Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
"""
from collections import namedtuple
from itertools import product, starmap


def slope(A, B):
    """Returns the slope of the line defined by the points A and B"""
    return (A.y - B.y)*1.0 / (A.x - B.x)


def is_triangle(A, B, C):
    """Returns `True` if the points A, B and C are non-collinear"""
    if B.x - A.x and C.x - A.x:
        return slope(A, B) != slope(A, C)
    else:
        return B.x - A.x != C.x - A.x


def is_right(A, B, C):
    """
    Returns `True` if the triangle defined by the points A, B and C is
    right-angled
    """
    # Compute the squares of the sides of the triangle
    a2, b2, c2 = sorted(((I.x - J.x)**2 + (I.y - J.y)**2)
                        for I, J in ((B, C), (A, C), (A, B)))

    return a2 + b2 == c2


def solution():
    Point = namedtuple('Point', ['x', 'y'])

    # The origin
    O = Point(x=0, y=0)

    # Min and Max for x and y
    mx = my = 0
    Mx = My = 50

    rtriangles = 0

    for P in starmap(Point, product(range(mx, Mx+1), range(my, My+1))):
        if P == O:
            continue

        for Q in starmap(Point, product(range(mx, P.x+1), range(my, My+1))):
            # Skip duplicate and invalid point sets
            if (P.x == Q.x and P.y <= Q.y) or Q == O:
                continue

            if is_triangle(O, P, Q) and is_right(O, P, Q):
                rtriangles += 1

    return rtriangles


if __name__ == '__main__':
    print(solution())
