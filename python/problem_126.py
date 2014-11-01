# -*- coding: utf-8 -*-
"""
Problem 126 - Cuboid layers

The minimum number of cubes to cover every visible face on a cuboid measuring
3 x 2 x 1 is twenty-two.

            (https://projecteuler.net/project/images/p126.gif)

If we then add a second layer to this solid it would require forty-six cubes to
cover every visible face, the third layer would require seventy-eight cubes,
and the fourth layer would require one-hundred and eighteen cubes to cover
every visible face.

However, the first layer on a cuboid measuring 5 x 1 x 1 also requires
twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1,
7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.

We shall define C(n) to represent the number of cuboids that contain n cubes in
one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.

It turns out that 154 is the least value of n for which C(n) = 10.

Find the least value of n for which C(n) = 1000.
"""
from collections import defaultdict
from itertools import count, takewhile


def first_layer(i, j, k):
    """
    Returns the number of cubes in the first layer of the (i, j, k) cuboid
    """
    return 2*(i*j + i*k + j*k)


def first_delta(i, j, k):
    """
    Returns the number of extra cubes required for the second layer of the
    (i, j, k) cuboid
    """
    return 4*(i + j + k)


def layers(i, j, k):
    """
    Makes an iterator that returns the number of cubes in every layer of the
    (i, j, k) cuboid
    """
    layer = first_layer(i, j, k)
    delta = first_delta(i, j, k)

    # Yield the first layer
    yield layer

    for n in count(1):
        layer += delta
        yield layer

        # The number of required extra cubes increases by 8 between successive
        # layers of a cuboid
        delta += 8


def solution():
    target = 1000

    # Parameters tuned for faster execution (first tested on the smaller
    # C(n) = 10 subproblem)
    longest = 10*target
    maxlayer = 25*target

    # C[n] is the number of cuboids that contain n cubes in one of their layers
    C = defaultdict(int)

    # Iterate over the layers of (i, j, k) cuboids, where i >= j >= k
    for i in range(1, longest+1):
        # The layer with the least number of cubes in the current iteration is
        # the first layer of the (i, 1, 1) cuboid
        least = first_layer(i, 1, 1)
        if least > maxlayer:
            break

        for j in takewhile(lambda j: first_layer(i, j, 1) <= maxlayer,
                           range(1, i+1)):

            for k in takewhile(lambda k: first_layer(i, j, k) <= maxlayer,
                               range(1, j+1)):

                for l in takewhile(lambda l: l <= maxlayer, layers(i, j, k)):
                    # `l` is the number of cubes in one of the layers of the
                    # (i, j, k) cuboid
                    C[l] += 1

        # Check if `target` has been reached for any of the "completed" layer
        # values (completed in the sense that there are no more cuboids
        # containing the same number of cubes in any of their layers)
        for l in sorted(l for l in C if l <= least):
            if C[l] == target:
                return l


if __name__ == '__main__':
    print(solution())
