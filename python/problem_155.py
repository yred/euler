# -*- coding: utf-8 -*-
"""
Problem 155 - Counting Capacitor Circuits

An electric circuit uses exclusively identical capacitors of the same value C.

The capacitors can be connected in series or in parallel to form sub-units,
which can then be connected in series or in parallel with other capacitors or
other sub-units to form larger sub-units, and so on up to a final circuit.

Using this simple procedure and up to n identical capacitors, we can make
circuits having a range of different total capacitances. For example, using up
to n=3 capacitors of 60 F each, we can obtain the following 7 distinct total
capacitance values:

            https://projecteuler.net/project/images/p155_capacitors1.gif

If we denote by D(n) the number of distinct total capacitance values we can
obtain when using up to n equal-valued capacitors and the simple procedure
described above, we have: D(1)=1, D(2)=3, D(3)=7 ...

Find D(18).

Reminder:
When connecting capacitors C1, C2 etc in parallel, the total capacitance is:
            CT = C1 + C2 + ...
whereas when connecting them in series, the overall capacitance is given by:
            1/CT = 1/C1 + 1/C2 + ...
"""
from collections import defaultdict
from fractions import Fraction


def parallel_capacitance(nca, dca, ncb, dcb):
    return Fraction(nca*dcb + ncb*dca, dca*dcb)


def serial_capacitance(nca, dca, ncb, dcb):
    return Fraction(nca*ncb, nca*dcb + ncb*dca)


def solution():
    cvalues = set([str(Fraction(60, 1))])

    circuits = defaultdict(set)
    circuits[1].add(Fraction(60, 1))

    target = 18
    for n in range(2, target+1):
        for na in circuits.keys():
            for ca in circuits[na]:
                for cb in circuits[n - na]:
                    nca, dca = ca.numerator, ca.denominator
                    ncb, dcb = cb.numerator, cb.denominator

                    parallel = parallel_capacitance(nca, dca, ncb, dcb)
                    if str(parallel) not in cvalues:
                        cvalues.add(str(parallel))
                        circuits[n].add(parallel)

                    serial = serial_capacitance(nca, dca, ncb, dcb)
                    if str(serial) not in cvalues:
                        cvalues.add(str(serial))
                        circuits[n].add(serial)

    return len(cvalues)


if __name__ == '__main__':
    print(solution())
