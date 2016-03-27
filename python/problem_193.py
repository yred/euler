# -*- coding: utf-8 -*-
"""
Problem 193 - Squarefree Numbers

A positive integer n is called squarefree, if no square of a prime divides n,
thus 1, 2, 3, 5, 6, 7, 10, 11 are squarefree, but not 4, 8, 9, 12.

How many squarefree numbers are there below 2^50?
"""
from common import primes_up_to


def solution():
    maximum = 2**50 - 1

    squares = []
    for p in primes_up_to(int(maximum**0.5)):
        square = p**2

        # Compute the number of the current square's multiples
        pcounts = [maximum/square]

        # Remove the intersection of current and previously accounted-for squares
        for sq, counts in squares:
            if sq*square > maximum:
                break

            for c in counts:
                pcount = abs(c)/square
                if not pcount:
                    break

                pcounts.append(pcount if c < 0 else -pcount)

        squares.append((square, sorted(pcounts, key=abs, reverse=True)))

    squareful = sum(sum(counts) for _, counts in squares)
    return maximum - squareful


if __name__ == '__main__':
    print(solution())
