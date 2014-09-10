# -*- coding: utf-8 -*-
"""
Problem 42 - Coded triangle numbers

The n-th term of the sequence of triangle numbers is given by, t(n) = ½n(n+1);
so the first ten triangle numbers are:

        1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its
alphabetical position and adding these values we form a word value. For
example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value
is a triangle number then we shall call the word a triangle word.

Using "../resources/p042_words.txt", a 16K text file containing nearly
two-thousand common English words, how many are triangle words?
"""
from itertools import takewhile

from common import triangles


A = ord('A') - 1


def word_value(s):
    return sum(map(lambda c: ord(c) - A, s))


def solution():
    with open('../resources/p042_words.txt') as f:
        words = [w[1:-1] for w in f.read().split(',')]

    max_value = 26*max(len(w) for w in words)
    bounded_t = set(takewhile(lambda t: t <= max_value, iter(triangles)))

    return len([1 for word in words if word_value(word) in bounded_t])


if __name__ == '__main__':
    print(solution())
