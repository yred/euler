# -*- coding: utf-8 -*-
"""
Problem 137 - Fibonacci golden nuggets

Consider the infinite polynomial series A_F(x) = xF1 + (x^2)F2 + (x^3)F3 + ...,
where Fk is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ; that
is, Fk = Fk−1 + Fk−2, F1 = 1 and F2 = 1.

For this problem we shall be interested in values of x for which A_F(x) is a
positive integer.

Surprisingly:

    A_F(1/2) = (1/2)*1 + (1/2)^2*1 + (1/2)^3*2 + (1/2)^4*3 + (1/2)^5*5 + ...
             = 1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ...
             = 2

The corresponding values of x for the first five natural numbers are shown
below:

            x           A_F(x)
            √2−1          1
            1/2           2
            (√13−2)/3     3
            (√89−5)/8     4
            (√34−3)/5     5

We shall call A_F(x) a golden nugget if x is rational, because they become
increasingly rarer; for example, the 10th golden nugget is 74049690.

Find the 15th golden nugget.
"""
from common import memoize


@memoize
def fibonacci(n):
    if n in (1, 2):
        return 1
    return fibonacci(n - 1) + fibonacci(n - 2)


def solution():
    # Replacing A_F(x) by S, we can derive the following equation:
    #
    #           S = x + x² + (S - x)x + Sx²
    #
    # which is true for all x such that: |x| < (1/2)*(√5 − 1)
    #
    # Thus:     Sx² + (1 + S)x - S = 0
    #
    # Solving for x, restricting S to integer values and only accounting for
    # rational solutions results in:
    #
    #           S = Fibonnaci(2*i)*Fibonacci(2*i + 1)       (i >= 1)
    #
    # Based on:
    #   http://www.cnd.mcgill.ca/~ivan/Fibonacci_power_series_3618079.pdf
    golden_index = 15
    return fibonacci(2*golden_index)*fibonacci(2*golden_index + 1)


if __name__ == '__main__':
    print(solution())
