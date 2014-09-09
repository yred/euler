"""
Problem 43 - Sub-string divisibility

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
each of the digits 0 to 9 in some order, but it also has a rather interesting
sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
the following:

    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17

Find the sum of all 0 to 9 pandigital numbers with this property.
"""
from itertools import permutations


PRIMES = [2, 3, 5, 7, 11, 13, 17]


def is_special(t):
    s = ''.join(t)
    return all(int(s[i:i+3]) % PRIMES[i] == 0 for i in range(7))


def missing_digit(t):
    for d in '0123456789':
        if d not in t:
            return d


def solution():
    return sum(map(lambda t: int(missing_digit(t) + ''.join(t)),
                   filter(is_special, permutations('0123456789', 9))))


if __name__ == '__main__':
    print(solution())
