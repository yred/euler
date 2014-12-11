# -*- coding: utf-8 -*-
"""
Problem 151 - Paper sheets of standard sizes: an expected-value problem

A printing shop runs 16 batches (jobs) every week and each batch requires a
sheet of special colour-proofing paper of size A5.

Every Monday morning, the foreman opens a new envelope, containing a large
sheet of the special paper with size A1.

He proceeds to cut it in half, thus getting two sheets of size A2. Then he cuts
one of them in half to get two sheets of size A3 and so on until he obtains the
A5-size sheet needed for the first batch of the week.

All the unused sheets are placed back in the envelope.

            (https://projecteuler.net/project/images/p151.gif)

At the beginning of each subsequent batch, he takes from the envelope one sheet
of paper at random. If it is of size A5, he uses it. If it is larger, he repeats
the 'cut-in-half' procedure until he has what he needs and any remaining sheets
are always placed back in the envelope.

Excluding the first and last batch of the week, find the expected number of
times (during each week) that the foreman finds a single sheet of paper in the
envelope.

Give your answer rounded to six decimal places using the format x.xxxxxx
"""
from decimal import Decimal, getcontext


A1, A2, A3, A4, A5 = 16, 8, 4, 2, 1


def draws(envelope):
    """Returns all possible sheet draws"""
    for sheet, count in envelope.items():
        yield sheet, count, {s: c for s, c in envelope.items() if s != sheet}


def batch(envelope, is_first=False):
    """
    Given the contents of the envelope, returns the expected number of times
    that the foreman finds a single sheet of paper in the envelope (excluding
    the first and last batches)
    """
    singles, total = Decimal(0), Decimal(sum(envelope.values()))

    if not is_first and total == 1 and envelope.keys()[0] != A5:
        singles += 1

    for sheet, count, rem in draws(envelope):
        if count > 1:
            rem[sheet] = count - 1

        while sheet > A5:
            sheet /= 2
            rem[sheet] = rem.get(sheet, 0) + 1

        if rem:
            singles += (count/total)*batch(rem)

    return singles


def solution():
    getcontext().prec = 7
    return round(batch({A1: 1}, is_first=True), 6)


if __name__ == '__main__':
    print(solution())
