# -*- coding: utf-8 -*-
"""
Problem 112 - Bouncy numbers

Working from left-to-right if no digit is exceeded by the digit to its left it
is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a
decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a
"bouncy" number; for example, 155349.

Clearly there cannot be any bouncy numbers below one-hundred, but just over
half of the numbers below one-thousand (525) are bouncy. In fact, the least
number for which the proportion of bouncy numbers first reaches 50% is 538.

Surprisingly, bouncy numbers become more and more common and by the time we
reach 21780 the proportion of bouncy numbers is equal to 90%.

Find the least number for which the proportion of bouncy numbers is exactly
99%.
"""
from itertools import count


def is_bouncy(n):
    """Returns `True` if `n` is a bouncy number"""
    n, current = divmod(n, 10)

    # `direction` is equal to 1 if `n` is assumed to be an increasing number,
    # and to -1 if assumed to be a decreasing number
    direction = None

    while n > 0:
        last, current = current, n % 10

        if direction is None:
            if last - current != 0:
                direction = 1 if last > current else -1

        elif (last - current)*direction < 0:
            return True

        n /= 10

    return False


def solution():
    bouncy = 0

    for n in count(100):
        if is_bouncy(n):
            bouncy += 1

            if bouncy*1.0 / n >= 0.99:
                return n


if __name__ == '__main__':
    print(solution())
