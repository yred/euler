from itertools import count
from math import sqrt


def memoize(func):
    """
    Returns a memoized version of `func`, where `func` is a function that
    accepts a single, hashable argument
    """
    data = {}

    def wrapped(arg):
        if arg not in data:
            data[arg] = func(arg)

        return data[arg]

    return wrapped


def gcd(a, b):
    while b != 0:
        a, b = b, a % b

    return a


def is_prime(n):
    if n <= 1:
        return False

    for i in range(2, int(sqrt(n)) + 1):
        if n % i == 0:
            return False

    return True


def primes():
    yield 2

    for n in count(3, 2):
        if is_prime(n):
            yield n


def divisors(n):
    """Returns the divisors of `n` as a sorted list"""
    ds = set()

    for i in range(1, int(sqrt(n) + 1)):
        if n % i == 0:
            ds.add(i)
            ds.add(n/i)

    return list(sorted(ds))
