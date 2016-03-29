# -*- coding: utf-8 -*-
"""
Problem 204 - Generalised Hamming Numbers

A Hamming number is a positive number which has no prime factor larger than 5.
So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15.
There are 1105 Hamming numbers not exceeding 10^8.

We will call a positive number a generalised Hamming number of type n, if it has
no prime factor larger than n. Hence the Hamming numbers are the generalised Hamming
numbers of type 5.

How many generalised Hamming numbers of type 100 are there which don't exceed 10^9?
"""
from common import primes_up_to


def count_hamming_numbers(current, maximum, primes):
    """
    Starting with `current`, count how many numbers can be generated from the
    product of the supplied primes, without exceeding the specified limit.
    """
    if not primes:
        return 1

    count, ppower = 0, 1
    while ppower*current <= maximum:
        count += count_hamming_numbers(ppower*current, maximum, primes[1:])
        ppower *= primes[0]

    return count


def solution():
    maxnum = 10**9
    maxprime = 100

    primes = list(primes_up_to(maxprime))
    return count_hamming_numbers(1, maxnum, primes[::-1])


if __name__ == '__main__':
    print(solution())
