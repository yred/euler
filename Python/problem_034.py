# -*- coding: utf-8 -*-
"""
Problem 34 - Digit factorials

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of
their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
"""
from itertools import combinations_with_replacement, count, permutations
from math import factorial


def tuple_to_int(t):
    return reduce(lambda a, b: a*10 + b, t)


def max_length(d):
    """
    Returns the maximum length of numbers equal to the sum of the factorial of
    their digits, when d is the greatest digit value. Uses the fact that all
    digits are strictly less than 10
    """
    for l in count(1):
        if len(str(l*factorial(d))) <= l:
            return l


def sum_digits_factorials():
    checked = set()

    for d in range(2, 10):
        for length in range(2, max_length(d) + 1):
            for c in combinations_with_replacement(range(d+1), length):
                sum_digit_facts = sum(map(factorial, c))

                for p in permutations(c):
                    n = tuple_to_int(p)

                    if n in checked:
                        continue
                    else:
                        checked.add(n)

                    # only yield numbers that are true sums
                    if n > 9 and n == sum_digit_facts:
                        yield n

                        # The sum of digit factorials is unique
                        break

                    # n will only grow from here...
                    if n > sum_digit_facts:
                        break


def solution():
    return sum(sum_digits_factorials())


if __name__ == '__main__':
    print(solution())
