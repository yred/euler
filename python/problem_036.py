"""
Problem 36 - Double-base palindromes

The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in
base 10 and base 2.

(Please note that the palindromic number, in either base, may not include
leading zeros.)
"""
from itertools import count, takewhile


def palindromes():
    """Yields all base 10 palindromes"""
    for n in count(1):
        if str(n) == str(n)[::-1]:
            yield n


def is_palindrome_2(n):
    b = bin(n)[2:]

    return b == b[::-1]


def solution():
    return sum(n for n in takewhile(lambda a: a < 1e6, palindromes())
               if is_palindrome_2(n))


if __name__ == '__main__':
    print(solution())
