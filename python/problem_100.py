# -*- coding: utf-8 -*-
"""
Problem 100 - Arranged probability

If a box contains twenty-one coloured discs, composed of fifteen blue discs and
six red discs, and two discs were taken at random, it can be seen that the
probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two
blue discs at random, is a box containing eighty-five blue discs and
thirty-five red discs.

By finding the first arrangement to contain over 10^12 = 1,000,000,000,000
discs in total, determine the number of blue discs that the box would contain.
"""
from common.diophantine import diophantine_solutions


def solution():
    threshold = 10**12

    # The probability equation is:
    #
    #              (blue/combined) * ((blue - 1)/(combined - 1)) = 1/2
    #
    # By replacing blue by x and combined by y, the above equation can be
    # rewritten as:
    #
    #               2*x^2 - y^2 - 2*x + y = 0
    #
    # which is a binary quadratic Diophatine equation. By using Lagrange's
    # method for solving the general case, and by simplifying, we obtain the
    # following Diophantine equation:
    #
    #               X^2 - 8*Y^2 = -4
    #
    # where X = 4*y - 2 and Y = 2*x - 1
    #
    for X, Y in diophantine_solutions(8, -4):
        blue = (Y+1) / 2
        combined = (X+2) / 4

        if combined >= threshold:
            return blue


if __name__ == '__main__':
    print(solution())
