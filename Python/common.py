from itertools import count
from math import sqrt


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
