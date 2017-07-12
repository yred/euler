# -*- coding: utf-8 -*-
"""
Problem 347 - Largest integer divisible by two primes

The largest integer ≤ 100 that is only divisible by both the primes 2 and 3 is 96,
as 96 = 32*3 = 2^5*3. For two distinct primes p and q let M(p, q, N) be the largest
positive integer ≤N only divisible by both p and q and M(p, q, N) = 0 if such a
positive integer does not exist.

E.g.
    M(2, 3, 100) = 96.
    M(3, 5, 100) = 75 and not 90 because 90 is divisible by 2 ,3 and 5.
    Also M(2, 73, 100) = 0 because there does not exist a positive integer ≤ 100
    that is divisible by both 2 and 73.

Let S(N) be the sum of all distinct M(p, q, N). S(100) = 2262.

Find S(10 000 000).
"""
from common import primes_up_to


def powers(p, N):
    power = p

    while power <= N:
        yield power
        power *= p


def M(p, q, N):
    if p*q > N:
        return 0

    if p > q:
        p, q = q, p

    found = p*q
    for _p in powers(p, N):
        for _q in powers(q, N//_p):
            curr = _p*_q

            if curr > N:
                break

            if curr > found:
                found = curr

    return found


def solution():
    limit = 10**7
    primes = list(primes_up_to(limit//2))

    S = 0
    
    for ix, p in enumerate(primes):
        for q in primes[ix + 1:]:
            m = M(p, q, limit)
            if m == 0:
                break

            S += m

    return S


if __name__ == '__main__':
    print(solution())
