# -*- coding: utf-8 -*-
"""
Problem 78 - Coin partitions

Let p(n) represent the number of different ways in which n coins can be
separated into piles. For example, five coins can separated into piles in
exactly seven different ways, so p(5) = 7.

        OOOOO
        OOOO   O
        OOO   OO
        OOO   O   O
        OO   OO   O
        OO   O   O   O
        O   O   O   O   O

Find the least value of n for which p(n) is divisible by one million.
"""
from itertools import count


def solution():
    modulus = 10**6

    # Initializing the list of the number of partitions, where:
    #
    #           p(i) ≡ partitions[i] (mod modulus)
    #
    partitions = [1, 1]

    for n in count(2):
        # The number of partitions of n, p(n), is computed using the pentagonal
        # number recursion formula:
        #
        #   p(n) = Σ(-1)^k+1 * [p(n - k(3k-1)/2) + p(n - k(3k+1)/2)]   (k=1..n)
        #
        # where p(i) = 0 for all i < 0
        #
        # source:
        #   http://www.had2know.com/academics/integer-partition-calculator.html
        #
        p_n = 0

        for k in range(1, n+1):
            fst = k*(3*k - 1)/2
            snd = k*(3*k + 1)/2

            # All remaining terms are null if `fst` is greater than `n`
            if fst > n:
                break

            p_fst = partitions[n - fst]
            p_snd = partitions[n - snd] if snd <= n else 0

            p_n += ((-1)**(k+1))*(p_fst + p_snd)

        if (p_n % modulus) == 0:
            return n
        else:
            # Only keep track of the last (and in this case, meaningful) digits
            partitions.append(p_n % modulus)


if __name__ == '__main__':
    print(solution())
