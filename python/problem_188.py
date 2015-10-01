# -*- coding: utf-8 -*-
"""
Problem 188 - The hyperexponentiation of a number

The hyperexponentiation or tetration of a number a by a positive integer b,
denoted by a↑↑b or ᵇa, is recursively defined by:

a↑↑1 = a,
a↑↑(k+1) = a^(a↑↑k).

Thus we have e.g. 3↑↑2 = 3^3 = 27, hence 3↑↑3 = 3^27 = 7625597484987 and 3↑↑4 is
roughly 10^(3.6383346400240996*10^12).

Find the last 8 digits of 1777↑↑1855.
"""


def solution():
    a = hyper_a = 1777
    b = 1855
    modulus = 10**8

    for _ in range(1, b):
        hyper_a = pow(a, hyper_a, modulus)

    return hyper_a


if __name__ == '__main__':
    print(solution())
