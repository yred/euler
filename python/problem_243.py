# -*- coding: utf-8 -*-
"""
Problem 243 - Resilience

A positive fraction whose numerator is less than its denominator is called a
proper fraction.

For any denominator, d, there will be d−1 proper fractions; for example, with
d = 12:

    1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12

We shall call a fraction that cannot be cancelled down a resilient fraction.

Furthermore we shall define the resilience of a denominator, R(d), to be the
ratio of its proper fractions that are resilient; for example, R(12) = 4/11

In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10

Find the smallest denominator d, having a resilience R(d) < 15499/94744 .
"""
from fractions import Fraction

from common import primes as all_primes


def to_number(factors):
    n = 1

    for p, m in factors.iteritems():
        n *= p**m

    return n


def totient(factors):
    result = 1

    for p, m in factors.iteritems():
        result *= (p-1) * (p**(m-1))

    return result


def decompose(n, primes):
    for p in primes:
        if n == 1:
            return

        if n in primes:
            yield n
            break

        while n % p == 0:
            yield p
            n /= p


def solution():
    threshold = Fraction(15499, 94744)

    factors, primes = {}, []

    for p in all_primes():
        primes.append(p)
        factors[p] = 1

        if Fraction(totient(factors), to_number(factors)-1) < threshold:
            factors.pop(p)

            for k in range(2, p+1):
                kfactors = dict(factors)

                for kp in decompose(k, primes):
                    kfactors[kp] += 1

                n = to_number(kfactors)
                if Fraction(totient(kfactors), n-1) < threshold:
                    return n


if __name__ == '__main__':
    print(solution())
