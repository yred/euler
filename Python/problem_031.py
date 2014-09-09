# -*- coding: utf-8 -*-
"""
Problem 31 - Coin sums

In England the currency is made up of pound, £, and pence, p, and there are
eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
"""


def get_combination_count(coins, amount):
    assert len(coins) > 0
    assert amount > 0

    coins = sorted(coins)
    count = 0

    if len(coins) == 1:
        if amount % coins[0] == 0:
            return 1
        else:
            raise ValueError("It's not possible to produce %d using %dp coins"
                             % (amount, coins[0]))
    else:
        while len(coins) > 1:
            greatest = coins.pop()
            remaining = amount

            while remaining >= greatest:
                remaining -= greatest

                if remaining > 0:
                    count += get_combination_count(coins[:], remaining)
                else:
                    count += 1

        count += get_combination_count(coins, amount)

    return count


def solution():
    return get_combination_count([1, 2, 5, 10, 20, 50, 100, 200], 200)


if __name__ == '__main__':
    print(solution())
