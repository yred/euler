"""
Problem 6 - Sum square difference

The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.
"""


def square_of_sum(nmax):
    return sum(range(nmax+1))**2


def sum_of_squares(nmax):
    return sum(n*n for n in range(nmax+1))


def solution():
    return square_of_sum(100) - sum_of_squares(100)


if __name__ == '__main__':
    print(solution())
