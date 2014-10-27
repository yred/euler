# -*- coding: utf-8 -*-
"""
Problem 122 - Efficient exponentiation

The most naive way of computing n^15 requires fourteen multiplications:

            n × n × ... × n = n^15

But using a "binary" method you can compute it in six multiplications:

            n     ×  n    = n^2
            n^2   ×  n^2  = n^4
            n^4   ×  n^4  = n^8
            n^8   ×  n^4  = n^12
            n^12  ×  n^2  = n^14
            n^14  ×  n    = n^15

However it is yet possible to compute it in only five multiplications:

            n     ×  n    = n^2
            n^2   ×  n    = n^3
            n^3   ×  n^3  = n^6
            n^6   ×  n^6  = n^12
            n^12  ×  n^3  = n^15

We shall define m(k) to be the minimum number of multiplications to compute
n^k; for example m(15) = 5.

For 1 ≤ k ≤ 200, find ∑ m(k).
"""
from itertools import combinations_with_replacement, count


class Node(object):
    """Represents a node in an addition-chain tree"""

    def __init__(self, value, parent):
        self.value = value
        self.parent = parent

    def chain(self):
        """Returns all the values in the chain going back to the root node"""
        if self.parent:
            return self.parent.chain() + (self.value,)
        else:
            return (self.value,)


def solution():
    target = set(range(1, 201))

    # `minimum[k]` is the minimum number of multiplications to compute n^k
    minimum = {1: 0}

    # The starting point is the "root" node
    leaves = [Node(1, None)]

    # This solution is mainly inspired by figure 15 from "The Art of Computer
    # Programming, Volume 2: Seminumerical Algorithms", 3rd edition, §4.6.3
    for height in count(1):
        new_leaves = []

        for leaf in leaves:
            for c in combinations_with_replacement(leaf.chain(), 2):
                value = sum(c)
                if value not in minimum:
                    new_leaves.append(Node(value, leaf))

        # The update is done after generating new leaves in order to preserve
        # possibly beneficial duplicate chains
        for leaf in new_leaves:
            minimum[leaf.value] = height

        if target <= set(minimum.keys()):
            break
        else:
            leaves = new_leaves

    return sum(minimum[n] for n in target)


if __name__ == '__main__':
    print(solution())
