# -*- coding: utf-8 -*-
"""
Problem 102 - Triangle containment

Three distinct points are plotted at random on a Cartesian plane, for which
-1000 ≤ x, y ≤ 1000, such that a triangle is formed.

Consider the following two triangles:

            A(-340,495), B(-153,-910), C(835,-947)

            X(-175,41), Y(-421,-714), Z(574,-645)

It can be verified that triangle ABC contains the origin, whereas triangle XYZ
does not.

Using "../resources/p102_triangles.txt", a 27K text file containing the
co-ordinates of one thousand "random" triangles, find the number of triangles
for which the interior contains the origin.

NOTE: The first two examples in the file represent the triangles in the example
given above.
"""
from collections import namedtuple


def contains_origin(A, B, C):
    """
    Returns `True` if (0, 0) is within the triangle defined by the points A, B
    and C
    """
    xs = map(lambda P: P.x, (A, B, C))
    ys = map(lambda P: P.y, (A, B, C))

    # Check whether the triangle's lines intercept both axes
    if not all(min(ns) <= 0 <= max(ns) for ns in (xs, ys)):
        return False

    xintercepts = []
    yintercepts = []

    # Collect the triangle's segments intersections with both axes
    for P, Q in ((A, B), (A, C), (B, C)):
        if P.x == Q.x:
            if P.x == 0:
                yintercepts.extend([P.y, Q.y])

            if P.y*Q.y <= 0:
                xintercepts.append(P.x)

        elif P.y == Q.y:
            if P.y == 0:
                xintercepts.extend([P.x, Q.x])

            if P.x*Q.x <= 0:
                yintercepts.append(P.y)

        else:
            slope = (P.y - Q.y)*1.0 / (P.x - Q.x)

            xinter = P.x - P.y/slope
            if (xinter - P.x)*(xinter - Q.x) <= 0:
                xintercepts.append(xinter)

            yinter = P.y - P.x*slope
            if (yinter - P.y)*(yinter - Q.y) <= 0:
                yintercepts.append(yinter)

    return all(min(ns) <= 0 <= max(ns) for ns in (xintercepts, yintercepts))


def solution():
    Point = namedtuple('Point', ['x', 'y'])

    # Keep count of the triangles whose interior contains the origin
    triangles = 0

    with open('../resources/p102_triangles.txt') as f:
        for line in f.readlines():
            coords = map(int, line.strip().split(','))
            xs, ys = coords[::2], coords[1::2]

            if contains_origin(*(Point(x, y) for x, y in zip(xs, ys))):
                triangles += 1

    return triangles


if __name__ == '__main__':
    print(solution())
