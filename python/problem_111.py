# -*- coding: utf-8 -*-
"""
Problem 111 - Primes with runs

Considering 4-digit primes containing repeated digits it is clear that they
cannot all be the same: 1111 is divisible by 11, 2222 is divisible by 22, and
so on. But there are nine 4-digit primes containing three ones:

            1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111

We shall say that M(n, d) represents the maximum number of repeated digits for
an n-digit prime where d is the repeated digit, N(n, d) represents the number
of such primes, and S(n, d) represents the sum of these primes.

So M(4, 1) = 3 is the maximum number of repeated digits for a 4-digit prime
where one is the repeated digit, there are N(4, 1) = 9 such primes, and the
sum of these primes is S(4, 1) = 22275. It turns out that for d = 0, it is only
possible to have M(4, 0) = 2 repeated digits, but there are N(4, 0) = 13 such
cases.

In the same way we obtain the following results for 4-digit primes.

            Digit, d    M(4, d)     N(4, d)     S(4, d)
            0           2           13            67061
            1           3           9             22275
            2           3           1              2221
            3           3           12            46214
            4           3           2              8888
            5           3           1              5557
            6           3           1              6661
            7           3           9             57863
            8           3           1              8887
            9           3           7             48073

For d = 0 to 9, the sum of all S(4, d) is 273700.

Find the sum of all S(10, d).
"""
from collections import defaultdict
from itertools import combinations, combinations_with_replacement, permutations

from common import primes_up_to


def numbers(digit, times, length):
    """
    Yields numbers of length `length`, where `digit` is repeated `times` times.
    """
    other = set(range(10)) - set([digit])

    # Number of "empty" positions to fill in every returned result
    empty = length - times

    if not empty:
        yield int(str(digit) * length)

    else:
        # The smallest number of length `length`
        minimum = 10**(length - 1)

        for digits in combinations_with_replacement(other, empty):
            # Prevent the possibility of returning repeated results by wrapping
            # the digit permutations in a set
            for ds in set(permutations(digits)):
                for positions in combinations(range(length), empty):
                    num = idx = 0

                    # Compute the next return value by using `digit`, and
                    # filling in with digits from `ds` whenever necessary
                    for i in range(length):
                        num *= 10

                        if i == positions[idx % empty]:
                            num += ds[idx]
                            idx += 1
                        else:
                            num += digit

                    # Skip cases where there's a leading 0
                    if num >= minimum:
                        yield num


def solution():
    length = 10

    # Only primes up to sqrt(10**length) are required to test the
    # `length`-digit numbers primality
    primes = list(primes_up_to(int((10 ** (length/2.0)))))

    # dprimes[d] is the set of primes where `d` is repeated M(length, d) times
    dprimes = defaultdict(list)

    for digit in range(10):
        # This is true in most cases, with a few minor exceptions (e.g., when
        # `digit` is equal to 1 in the case of repunit primes)
        maxlen = length-1 if digit else length-2

        # Assume M(length, digit) is maxlen, and decrement it until the first
        # prime is found
        for l in reversed(range(2, maxlen+1)):
            for n in numbers(digit, l, length):
                # Test if `n` is prime
                if all(n % p for p in primes):
                    dprimes[digit].append(n)

            # Stop as soon as primes have been found
            # (i.e., M(length, digit) = l)
            if digit in dprimes:
                break

    return sum(sum(nums) for _, nums in dprimes.items())


if __name__ == '__main__':
    print(solution())
