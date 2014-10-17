# -*- coding: utf-8 -*-
"""
Problem 125 - Palindromic sums

The palindromic number 595 is interesting because it can be written as the sum
of consecutive squares: 6² + 7² + 8² + 9² + 10² + 11² + 12².

There are exactly eleven palindromes below one-thousand that can be written as
consecutive square sums, and the sum of these palindromes is 4164. Note that
1 = 0² + 1² has not been included as this problem is concerned with the squares
of positive integers.

Find the sum of all the numbers less than 10^8 that are both palindromic and
can be written as the sum of consecutive squares.
"""
from common import memoize


@memoize
def is_palindrome(n):
    """Returns `True` if `n` is a palindrome"""
    nstr = str(n)
    return nstr == nstr[::-1]


def solution():
    threshold = 10**8

    start = 2
    limit = int(threshold**0.5)

    # Initiliaze the list of the sums of consecutive squares with the first
    # "empty" sum
    sumsquares = [1]

    # The set of palindromic sums
    palindromes = set()

    for n in range(start, limit):
        # The last element is always an "empty" sum (a square number)
        for idx, sumsq in enumerate(sumsquares[:-1]):
            if is_palindrome(sumsq):
                palindromes.add(sumsq)

            # Increment all available sums with the square of the current term,
            # thus generating a new set of values that can be checked for
            # palindromes
            sumsquares[idx] += n*n

        # Add the square of the current term to the last element of the list,
        # before appending it as well to produce its own sequence of sums
        sumsquares[-1] += n*n
        sumsquares.append(n*n)

        # Drop any terms that are no longer valid
        drop_idx = next(i for i, v in enumerate(sumsquares) if v < threshold)
        if drop_idx:
            sumsquares = sumsquares[drop_idx:]

    return sum(palindromes)


if __name__ == '__main__':
    print(solution())
