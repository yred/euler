# -*- coding: utf-8 -*-
"""
Problem 178 - Step Numbers

Consider the number 45656. It can be seen that each pair of consecutive digits
of 45656 has a difference of one.

A number for which every pair of consecutive digits has a difference of one is
called a step number.

A pandigital number contains every decimal digit from 0 to 9 at least once.

How many pandigital step numbers less than 10^40 are there?
"""
from common import memoize


@memoize
def step_count(d, target, min_d, max_d, length):
    """
    Returns the number of length`-digit step numbers, beginning with the digit
    `d` and ending with `target`.

    Digits must be between `min_d` and `max_d`.
    """
    if d < min_d or d > max_d or abs(d - target) >= length:
        return 0

    if length == 1:
        return 1

    return (
        step_count(d - 1, target, min_d, max_d, length - 1) +
        step_count(d + 1, target, min_d, max_d, length - 1)
    )


def solution():
    minlen, maxlen = 10, 40

    # Pandigital step numbers must have one of the following forms:
    #
    #   1. <left>  9 ([1-8]+) 0 <right>, where left = [^0][0-9]+ and right = [^9]+
    #   2. <left> 10 ([1-8]+) 9 <right>, where left = [^0][0-9]+ and right = [^0]+
    #
    count = 0

    # Form 1 numbers
    for numlen in range(minlen, maxlen + 1):
        for seglen in range(8, numlen - 1):
            segcount = step_count(1, 8, 1, 8, seglen)

            for rlen in range(1, numlen - seglen):
                rcount = sum(step_count(0, d, 0, 8, rlen) for d in range(9))

                llen = (numlen - seglen) - rlen
                lcount = sum(step_count(9, d, 0, 9, llen) for d in range(1, 10))

                count += (lcount * segcount * rcount)

    # Form 2 numbers
    for numlen in range(minlen + 1, maxlen + 1):
        for seglen in range(8, numlen - 2):
            segcount = step_count(1, 8, 1, 8, seglen)

            for rlen in range(1, numlen - seglen - 1):
                rcount = sum(step_count(9, d, 1, 9, rlen) for d in range(1, 10))

                llen = (numlen - seglen - 1) - rlen
                lcount = sum(step_count(1, d, 0, 9, llen) for d in range(1, 10))

                count += (lcount * segcount * rcount)

    return count


if __name__ == '__main__':
    print(solution())
