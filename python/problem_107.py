# -*- coding: utf-8 -*-
"""
Problem 107 - Minimal network

The following undirected network consists of seven vertices and twelve edges
with a total weight of 243.

        (https://projecteuler.net/project/images/p107_1.gif)

The same network can be represented by the matrix below.

             A    B    C    D    E    F    G
        A    -    16   12   21   -    -    -
        B    16   -    -    17   20   -    -
        C    12   -    -    28   -    31   -
        D    21   17   28   -    18   19   23
        E    -    20   -    18   -    -    11
        F    -    -    31   19   -    -    27
        G    -    -    -    23   11   27   -

However, it is possible to optimise the network by removing some edges and
still ensure that all points on the network remain connected. The network which
achieves the maximum saving is shown below. It has a weight of 93, representing
a saving of 243 − 93 = 150 from the original network.

        (https://projecteuler.net/project/images/p107_2.gif)

Using "../resources/p107_network.txt", a 6K text file containing a network with
forty vertices, and given in matrix form, find the maximum saving which can be
achieved by removing redundant edges whilst ensuring that the network remains
connected.
"""


def is_connected(network):
    """Returns `True` if all of `network`'s vertices are connected"""
    # The set of connected vertices is seeded with the first in the network
    linked = set([0])

    # Processing queue: list of vertices whose connections haven't been checked
    pqueue = [0]

    for vertex in pqueue:
        for idx, edge in enumerate(network[vertex]):
            if edge is not None and idx not in linked:
                linked.add(idx)
                pqueue.append(idx)

    return linked == set(range(len(network)))


def weight(network):
    """Returns the sum of `network`'s edges"""
    # Return half the sum since the network's matrix is symmetric
    return sum(sum(edge or 0 for edge in row) for row in network)/2


def solution():
    with open('../resources/p107_network.txt') as f:
        network = [map(lambda s: None if s == '-' else int(s),
                       line.strip().split(',')) for line in f.readlines()]

    # List of edges to be potentially removed
    edges = sorted([(network[row][col], row, col)
                   for row in range(len(network)) for col in range(row)
                   if network[row][col] is not None], reverse=True)

    original = weight(network)

    for edge, row, col in edges:
        network[row][col] = network[col][row] = None

        # Revert the change if the network is no longer connected
        if not is_connected(network):
            network[row][col] = network[col][row] = edge

    return original - weight(network)


if __name__ == '__main__':
    print(solution())
