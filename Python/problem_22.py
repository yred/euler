# -*- coding: utf-8 -*-
"""
Problem 22 - Names scores

Using resources/p022_names.txt, a 46K text file containing over five-thousand
first names, begin by sorting it into alphabetical order. Then working out the
alphabetical value for each name, multiply this value by its alphabetical
position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
"""


BASE = ord('A') - 1


def score(name):
    return sum(ord(c) - BASE for c in name)


def solution():
    with open('resources/p022_names.txt') as f:
        names = sorted(map(lambda s: s[1:-1], f.read().strip().split(',')))

    return sum(idx*score(name) for idx, name in enumerate(names, start=1))


if __name__ == '__main__':
    print(solution())
