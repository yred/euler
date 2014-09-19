# -*- coding: utf-8 -*-
"""
Problem 68 - Magic 5-gon ring

Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and
each line adding to nine.

                            4
                             \
                              3
                             / \
                            1 - 2 - 6
                           /
                          5

Working clockwise, and starting from the group of three with the numerically
lowest external node (4,3,2 in this example), each solution can be described
uniquely. For example, the above solution can be described by the set:

        4,3,2; 6,2,1; 5,1,3.

It is possible to complete the ring with four different totals: 9, 10, 11, and
12. There are eight solutions in total.

        Total       Solution Set
        9           4,2,3; 5,3,1; 6,1,2
        9           4,3,2; 6,2,1; 5,1,3
        10          2,3,5; 4,5,1; 6,1,3
        10          2,5,3; 6,3,1; 4,1,5
        11          1,4,6; 3,6,2; 5,2,4
        11          1,6,4; 5,4,2; 3,2,6
        12          1,5,6; 2,6,4; 3,4,5
        12          1,6,5; 3,5,4; 2,4,6

By concatenating each group it is possible to form 9-digit strings; the maximum
string for a 3-gon ring is 432621513.

Using the numbers 1 to 10, and depending on arrangements, it is possible to
form 16- and 17-digit strings. What is the maximum 16-digit string for a
"magic" 5-gon ring?

                            n
                              \
                                n     n
                              /   \  /
                            n       n
                          /  \     /
                        n     n - n - n
                               \
                                n
"""
from itertools import permutations


def lines(outer, inner):
    """Returns the ring lines formed by the outer and inner nodes"""
    # Extend the inner ring for easier manipulation/indexing
    inner = inner + (inner[0],)

    return [(o, inner[idx], inner[idx+1]) for idx, o in enumerate(outer)]


def is_magical(lines):
    """
    Returns `True` if the ring made up of the specified lines is "magical"
    (i.e., the sums of the nodes on each line are equal)
    """
    return all(sum(l) == sum(lines[0]) for l in lines[1:])


def stringify(lines):
    """
    Returns the string representation of the ring made up of `lines`, using the
    method that was outlined in the problem statement
    """
    min_index = lines.index(min(lines))

    # Reorder the lines to start with the one with the smallest external node
    lines = lines[min_index:] + lines[:min_index]

    return ''.join(map(''.join, map(str, (e for l in lines for e in l))))


def solution():
    magic_rings = []

    # By reasoning through the problem statement, one can deduce that the
    # external/outer nodes must hold the values 6 - 10
    for outer in permutations(range(7, 11)):
        outer = (6,) + outer

        for inner in permutations(range(1, 6)):
            ring = lines(outer, inner)

            if is_magical(ring):
                magic_rings.append(stringify(ring))

    return max(magic_rings)


if __name__ == '__main__':
    print(solution())
