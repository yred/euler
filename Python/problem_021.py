# -*- coding: utf-8 -*-
"""
Problem 21 - Amicable numbers

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).

If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
"""
import common


def d(n):
    # Only use proper divisors -- discard the last divisor of n, which is
    # itself
    return sum(common.divisors(n)[:-1])


def solution():
    ns = set(range(1, 10000))
    amicable = []

    while ns:
        a = ns.pop()

        b = d(a)
        if b != a and d(b) == a:
            amicable.append(a)
            amicable.append(b)

            ns.remove(b)

    return sum(amicable)


if __name__ == '__main__':
    print(solution())
