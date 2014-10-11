# -*- coding: utf-8 -*-
"""
Problem 113 - Non-bouncy numbers

Working from left-to-right if no digit is exceeded by the digit to its left it
is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a
decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a
"bouncy" number; for example, 155349.

As n increases, the proportion of bouncy numbers below n increases such that
there are only 12951 numbers below one-million that are not bouncy and only
277032 non-bouncy numbers below 10^10.

How many numbers below a googol (10^100) are not bouncy?
"""
from common import memoize


@memoize
def monotonic(num_digits, length):
    """
    Returns the count of `length`-digit numbers containing up to `num_digits`
    digits, and where all digits follow a certain order (i.e., increasing or
    decreasing)
    """
    if length <= 0:
        return 0
    elif length == 1:
        return num_digits
    elif num_digits == 1:
        return 1

    # The 1 on the left accounts for the case where the current digit is
    # repeated for the full remaining length of the number
    return 1 + sum(monotonic(num_digits-1, length - n) for n in range(length))


def solution():
    maxlen = 100

    # Increasing numbers can only contain digits 1 through 9
    increasing = sum(monotonic(9, length) for length in range(1, maxlen+1))

    # Decreasing numbers should subtract 1 from every result returned by
    # monotonic(), since the l-digit 0 is not a valid decreasing number (only
    # positive numbers are being accounted for)
    decreasing = sum(monotonic(10, length)-1 for length in range(1, maxlen+1))

    # The number of "constant" numbers (i.e., all digits are equal)
    constant = 9*maxlen

    # Since constant numbers are counted twice (as part of increasing and
    # decreasing numbers), they should be subtracted from the result
    return increasing + decreasing - constant


if __name__ == '__main__':
    print(solution())
