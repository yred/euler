# -*- coding: utf-8 -*-
"""
Problem 79 - Passcode derivation

A common security method used for online banking is to ask the user for three
random characters from a passcode. For example, if the passcode was 531278,
they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be:
317.

The text file, "../resources/p079_keylog.txt", contains fifty successful login
attempts.

Given that the three characters are always asked for in order, analyse the file
so as to determine the shortest possible secret passcode of unknown length.
"""
from operator import itemgetter


def solution():
    with open('../resources/p079_keylog.txt') as f:
        parts = map(lambda s: s.strip(), f.readlines())

    passcode = ''

    while True:
        first = set(map(itemgetter(0), parts))
        other = set(c for cs in map(itemgetter(1, -1), parts) for c in cs)

        leading = (first - other).pop()

        passcode += leading

        # Remove non-informative (single-length) entries, after stripping them
        # all of the latest leading character
        parts = filter(lambda s: len(s) > 1,
                       [p.lstrip(leading) for p in parts])

        # If it's the end...
        if not parts:
            passcode += other.pop()
            break

    return passcode


if __name__ == '__main__':
    print(solution())
