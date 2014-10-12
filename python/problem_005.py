"""
Problem 5 - Smallest multiple

2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
"""
from common import gcd


def lcm(a, b):
    """Returns the least common multiple of `a` and `b`"""
    return a*b / gcd(a, b)


def solution():
    return reduce(lcm, range(2, 21))


if __name__ == '__main__':
    print(solution())
