"""
Problem 30 - Digit fifth powers

Surprisingly there are only three numbers that can be written as the sum of
fourth powers of their digits:

        1634 = 1^4 + 6^4 + 3^4 + 4^4
        8208 = 8^4 + 2^4 + 0^4 + 8^4
        9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers
of their digits.
"""
from itertools import combinations_with_replacement, count, permutations


def tuple_to_int(t):
    return reduce(lambda a, b: a*10 + b, t)


def sum_power(ns, p):
    return sum(map(lambda i: i**p, ns))


def max_length(d, p):
    """
    Returns the maximum length of numbers equal to the sum of the p-th power
    of their digits, when d is the greatest digit value. Uses the fact that all
    digits are strictly less than 10
    """
    for l in count(1):
        if len(str(l*(d**p))) <= l:
            return l


def sum_digits_nth_power(power):
    checked = set()

    for d in range(2, 10):
        for length in range(2, max_length(d, power) + 1):
            for c in combinations_with_replacement(range(d+1), length):
                for p in permutations(c):
                    n = tuple_to_int(p)

                    if n in checked:
                        continue
                    else:
                        checked.add(n)

                    # only yield numbers that are true sums of their digits
                    # raised to the fourth power (i.e., contain more than
                    # digit)
                    if n > 9 and n == sum_power(p, power):
                        yield n


def solution():
    return sum(sum_digits_nth_power(5))


if __name__ == '__main__':
    print(solution())
