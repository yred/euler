# -*- coding: utf-8 -*-
"""
Problem 145 - How many reversible numbers are there below one-billion?

Some positive integers n have the property that the sum [ n + reverse(n) ]
consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and
409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904
are reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10^9)?
"""


def all_odd(n):
    if n == 0:
        return False

    while n > 0:
        if n % 2 == 0:
            return False

        n /= 10

    return True


def reverse(n):
    return int(str(n)[::-1])


def ranges(limit):
    yield 11, 1000

    base = 1000
    while base < limit:
        for first_digit in [2, 4, 6, 8]:
            start, stop = first_digit*base + 1, first_digit*base + base
            if start >= limit:
                break

            yield start, min(stop, limit)

        base *= 10


def solution():
    limit = 10**9
    count = 0

    for start, stop in ranges(limit):
        for n in xrange(start, stop, 2):
            if all_odd(n + reverse(n)):
                count += 2

    return count


if __name__ == '__main__':
    print(solution())
