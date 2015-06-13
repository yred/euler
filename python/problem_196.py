# -*- coding: utf-8 -*-
"""
Problem 196 - Prime triplets

Build a triangle from all positive integers in the following way:

         1
         2  3
         4  5  6
         7  8  9 10
        11 12 13 14 15
        16 17 18 19 20 21
        22 23 24 25 26 27 28
        29 30 31 32 33 34 35 36
        37 38 39 40 41 42 43 44 45
        46 47 48 49 50 51 52 53 54 55
        56 57 58 59 60 61 62 63 64 65 66
        . . .

Each positive integer has up to 8 neighbours in the triangle.

A set of three primes is called a prime triplet if one of the three primes has
the other two as neighbours in the triangle.

For example, in the 2nd row, the prime numbers 2 and 3 are elements of some
prime triplet.
If row 8 is considered, it contains two primes which are elements of some prime
triplet, i.e. 29 and 31.
If row 9 is considered, it contains only one prime which is an element of some
prime triplet: 37.

Define S(n) as the sum of the primes in row n which are elements of any prime
triplet. Then S(8)=60 and S(9)=37.

You are given that S(10000) = 950007619.

Find  S(5678027) + S(7208785).
"""
from collections import defaultdict
from itertools import takewhile, count

from common import primes_up_to


class Row(object):
    def __init__(self, index):
        assert index >= 1
        self.index = index

        # The elements of the row are self.lower..self.upper
        self.lower = 1 + (index*(index-1))/2
        self.upper = self.lower + index


def in_triplet(row_idx, col_idx, prime_index, check_neighbors=False):
    """
    Return `True` if the prime at row `row_idx` and the column `col_idx` of the
    triangle belongs to a prime triplet
    """
    # Make use of the fact that the current element is prime
    if row_idx % 2:
        deltas = [(-1, 0), (1, -1), (1, 1)]
    else:
        deltas = [(-1, -1), (-1, 1), (1, 0)]

    prime_neighbors = []
    for row_delta, col_delta in deltas:
        row = row_idx + row_delta
        col = col_idx + col_delta
        if prime_index[row].get(col):
            prime_neighbors.append((row, col))

    if len(prime_neighbors) >= 2:
        return True

    return (check_neighbors and
            any(in_triplet(row, col, prime_index) for row, col in prime_neighbors))


def primes_in_range(lo, hi, divisors):
    """Return all primes in the range lo..hi"""
    numbers = set(takewhile(lambda a: a <= hi, count(lo)))

    # Uses the same method as the sieve of Eratosthenes
    for d in divisors:
        if d*d > hi:
            break

        first_multiple = d*(1 + (lo-1)/d)
        for n in count(first_multiple, d):
            if n > hi:
                break
            numbers.discard(n)

    return numbers


def solution():
    # This solution assumes that every row number is >= 5
    row_indices = [5678027, 7208785]

    prime_divisors = list(primes_up_to(max(row_indices)))

    # Map of row_idx -> col_idx -> prime
    # Note: row_idx and col_idx are both 1-based
    primes = defaultdict(dict)

    # Map of row_idx -> primes belonging to at least 1 prime triplet
    triplets = defaultdict(list)

    for row_idx in row_indices:
        for delta in (-2, -1, 0, 1, 2):
            cur_idx = row_idx + delta
            if cur_idx in primes:
                continue

            row = Row(cur_idx)
            for p in primes_in_range(row.lower, row.upper, prime_divisors):
                primes[cur_idx][p - row.lower + 1] = p

        for col_idx in primes[row_idx]:
            if in_triplet(row_idx, col_idx, primes, check_neighbors=True):
                triplets[row_idx].append(primes[row_idx][col_idx])

    return sum(sum(ps) for ps in triplets.values())


if __name__ == '__main__':
    print(solution())
