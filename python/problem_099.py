# -*- coding: utf-8 -*-
"""
Problem 99 - Largest exponential

Comparing two numbers written in index form like 2^11 and 3^7 is not difficult,
as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.

However, confirming that 632382^518061 > 519432^525806 would be much more
difficult, as both numbers contain over three million digits.

Using "../resources/p099_base_exp.txt", a 22K text file containing one thousand
lines with a base/exponent pair on each line, determine which line number has
the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example
given above.
"""
from math import log


def solution():
    with open('../resources/p099_base_exp.txt') as f:
        pairs = [map(int, line.strip().split(',')) for line in f.readlines()]

    # Uses the fact that: a^b = exp(log(a))^b = exp(b*log(a))
    return max((exponent*log(base), index)
               for index, (base, exponent) in enumerate(pairs, start=1))[1]


if __name__ == '__main__':
    print(solution())
