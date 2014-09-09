"""
Problem 18 - Maximum path sum I

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
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
    top to bottom, using a "branch and bound" algorithm
    """
    # (row, column, cost)
    paths = [(0, 0, 0)]

    last_row = len(rows) - 1
    min_cost = float('inf')

    while paths:
        row, column, cost = paths.pop()
        if row == last_row:
            if cost < min_cost:
                min_cost = cost
        else:
            # possible moves are "down" and "down-right"
            for step in range(2):
                new_cost = cost + rows[row + 1][column + step]
                if new_cost < min_cost:
                    paths.append((row + 1, column + step, new_cost))

    return min_cost


def solution():
    rows = map(lambda line: map(int, line.strip().split()), '''
        75
        95 64
        17 47 82
        18 35 87 10
        20 04 82 47 65
        19 01 23 75 03 34
        88 02 77 73 07 63 67
        99 65 04 28 06 16 70 92
        41 41 26 56 83 40 80 70 33
        41 48 72 33 47 32 37 16 94 29
        53 71 44 65 25 43 91 52 97 51 14
        70 11 33 28 77 73 17 78 39 68 17 57
        91 71 52 38 17 14 91 43 58 50 27 29 48
        63 66 04 68 89 53 67 30 73 16 69 87 40 31
        04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    '''.strip().split('\n'))

    best, hollowed = hollow(rows)

    return best - find_minimal_cost(hollowed)


if __name__ == '__main__':
    print(solution())
