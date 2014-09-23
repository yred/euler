# -*- coding: utf-8 -*-
"""
Problem 92 - Square digit chains

A number chain is created by continuously adding the square of the digits in a
number to form a new number until it has been seen before.

For example,

        44 → 32 → 13 → 10 → 1 → 1
        85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless
loop. What is most amazing is that EVERY starting number will eventually arrive
at 1 or 89.

How many starting numbers below ten million will arrive at 89?
"""
from common import digits


def solution():
    limit = 10**7

    chains = {n: set([n]) for n in (1, 89)}

    for n in range(1, limit):
        if n in chains:
            continue

        path = [n]

        while True:
            path.append(sum(map(lambda a: a*a, digits(path[-1]))))

            for chain in chains.values():
                if path[-1] in chain:
                    chain.update(path)
                    break
            else:
                continue

            break

    return len(chains[89])


if __name__ == '__main__':
    print(solution())
