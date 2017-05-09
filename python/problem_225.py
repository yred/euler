# -*- coding: utf-8 -*-
"""
Problem 225 - Tribonacci non-divisors

The sequence 1, 1, 1, 3, 5, 9, 17, 31, 57, 105, 193, 355, 653, 1201 ...
is defined by T1 = T2 = T3 = 1 and Tn = Tn-1 + Tn-2 + Tn-3.

It can be shown that 27 does not divide any terms of this sequence. In fact, 27
is the first odd number with this property.

Find the 124th odd number that does not divide any terms of the above sequence.
"""
from collections import deque
from itertools import count


def tribonacci(n):
    assert n > 1

    a = b = c = 1
    yield a
    yield b

    while True:
        yield c % n
        a, b, c = b, c, a + b + c


def solution():
    target = 124
    nondvs = []
    keylen = 3

    for n in count(start=3, step=2):
        divides = True

        for nd in nondvs:
            if n % nd == 0:
                divides = False
                break

        if divides:
            keys = set()
            last = deque(maxlen=keylen)

            for t in tribonacci(n):
                if t == 0:
                    break

                last.append(t)

                if len(last) == keylen:
                    key = tuple(last)

                    if key in keys:
                        divides = False
                        break

                    keys.add(key)

        if not divides:
            nondvs.append(n)

            if target == len(nondvs):
                return n


if __name__ == '__main__':
    print(solution())
