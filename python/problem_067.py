"""
Problem 67 - Maximum path sum II

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in "../resources/p067_triangle.txt",
a 15K text file containing a triangle with one-hundred rows.
"""


def hollow(rows):
    """
    Returns an upper bound on the maximum total, as well as a hollowed/carved-
    out equivalent of the input rows/triangle, in which values reflect the
    "penalty" or cost of adding a specific "node" to the path when compared
    with the upper bound maximum total.
    """
    maximums = map(max, rows)

    return (
        sum(maximums),
        map(lambda row, maxi: [maxi - e for e in row], rows, maximums)
    )


def find_minimal_cost(rows):
    """
    Returns the minimal score or cost of traversing the rows (triangle) from
    top to bottom, using a dynamic programming approach
    """
    paths = [(cost, [idx]) for idx, cost in enumerate(rows[-1])]

    for row in reversed(rows[:-1]):
        previous, paths = paths, []

        for idx, cost in enumerate(row):
            best = min(previous[idx:idx+2])
            paths.append((best[0] + cost, best[1] + [idx]))

    return paths[0][0]


def solution():
    with open('../resources/p067_triangle.txt') as f:
        rows = map(lambda line: map(int, line.strip().split()), f.readlines())

    best, hollowed = hollow(rows)

    return best - find_minimal_cost(hollowed)


if __name__ == '__main__':
    print(solution())
