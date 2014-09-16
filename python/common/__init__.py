from itertools import count, islice
from math import sqrt, factorial


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
    """Returns the greatest common divisor of a and b"""
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
    """Makes an iterator that returns *all* primes numbers"""
    yield 2

    for n in count(3, 2):
        if is_prime(n):
            yield n


def primes_up_to(limit):
    """Returns a list of all primes up to `limit`"""
    yield 2

    sieve = set(range(1, (limit - 1)/2 + 1))

    for j in range(1, int(sqrt(limit/2)) + 1):
        for i in range(1, (limit - j)/(1 + 2*j) + 1):
            sieve.discard(i + j + 2*i*j)

    for n in sieve:
        yield 2*n + 1


def divisors(n):
    """Returns the divisors of `n` as a sorted list"""
    ds = set()

    for i in range(1, int(sqrt(n) + 1)):
        if n % i == 0:
            ds.add(i)
            ds.add(n/i)

    return list(sorted(ds))


def ncr(n, r):
    """
    Returns the number of r-element combinations chosen from an n-element set
    """
    if r > n:
        raise ValueError('r must be less than %d in ncr(%d, %d)' % (n, n, r))

    return factorial(n)/factorial(r)/factorial(n-r)


def npr(n, r):
    """
    Returns the number of r-element permutations chosen from an n-element set
    """
    if r > n:
        raise ValueError('r must be less than %d in npr(%d, %d)' % (n, n, r))

    return factorial(n)/factorial(n-r)
