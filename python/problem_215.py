# -*- coding: utf-8 -*-
"""
Problem 215 - Crack-free Walls

Consider the problem of building a wall out of 2×1 and 3×1 bricks
(horizontal×vertical dimensions) such that, for extra strength, the gaps between
horizontally-adjacent bricks never line up in consecutive layers, i.e. never form
a "running crack".

For example, the following 9×3 wall is not acceptable due to the running crack
shown in red:

        https://projecteuler.net/project/images/p215_crackfree.gif

There are eight ways of forming a crack-free 9×3 wall, written W(9,3) = 8.

Calculate W(32,10).
"""
from common import memoize


INDEX = dict()


def key(set_):
    return tuple(sorted(set_))


def build_index(width, bricks):
    js = joints(width, bricks)
    for j in js:
        INDEX[key(j)] = [key(other) for other in js if j.isdisjoint(other)]


def joints(width, bricks):
    results = []

    for b in bricks:
        if b < width:
            partials = joints(width-b, bricks)

            for partial in partials:
                results.append({b+k for k in {0} | partial})
        elif b == width:
            results.append(set())

    return results


@memoize
def walls(layer, remaining):
    count = 0

    if remaining == 0:
        return 1
    else:
        for neighbor in INDEX[layer]:
            count += walls(neighbor, remaining-1)

    return count


def solution():
    width, height = 32, 10
    bricks = (2, 3)

    build_index(width, bricks)
    return sum(walls(k, height-1) for k in INDEX)


if __name__ == '__main__':
    print(solution())
