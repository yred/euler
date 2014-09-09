"""
Problem 26 - Reciprocal cycles

A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

    1/2  =   0.5
    1/3  =   0.(3)
    1/4  =   0.25
    1/5  =   0.2
    1/6  =   0.1(6)
    1/7  =   0.(142857)
    1/8  =   0.125
    1/9  =   0.(1)
    1/10 =   0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle
in its decimal fraction part.
"""
from collections import Counter
from decimal import Decimal, getcontext


# Skip the first n digits after the decimal dot, since in most cases they're
# unlikely to be part of any recurring cycle
SKIP = 100

# Chosen precision = skip + 2*upper bound on cycle length
# (For more info, visit:  http://mathworld.wolfram.com/DecimalExpansion.html)
PRECISION = getcontext().prec = SKIP + 2*1000


def parts(string, n):
    """
    Yield successive n-sized parts from string.

    Originally from:
        http://stackoverflow.com/questions/312443/how-do-you-split-a-list-into-evenly-sized-chunks-in-python
    """
    for i in range(0, len(string), n):
        yield string[i:i+n]


def is_cycle(candidate, original):
    for part in parts(original, len(candidate)):
        if candidate[:len(part)] != part:
            return False

    return True


def find_cycle(digit_str):
    c = Counter(digit_str)

    # The case of a signle repeated digit
    if len(c) == 1:
        return digit_str[0]

    # Use the least common digit to check for cycles
    least_common, freq = c.most_common()[-1]

    occurences = [idx for idx, d in enumerate(digit_str) if d == least_common]

    for i in range(1, freq/2 + 1):
        candidate = digit_str[occurences[0]: occurences[i]]
        if is_cycle(candidate, digit_str[occurences[0]:]):
            return candidate

    # No cycles were found
    return ''


def get_unit_fraction_cycle_length(n):
    deci_str = str(1 / Decimal(n)).split('.')[-1]

    if len(deci_str) < PRECISION:
        return 0
    else:
        return len(find_cycle(deci_str[SKIP:]))


def solution():
    return max((get_unit_fraction_cycle_length(n), n)
               for n in range(2, 1000))[1]


if __name__ == '__main__':
    print(solution())
