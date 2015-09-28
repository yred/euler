from itertools import count
from math import sqrt, factorial


def memoize(func):
    """Returns a memoized version of `func`"""
    data = {}

    def wrapped(*args, **kwargs):
        key = tuple(list(args) + sorted(kwargs.items()))

        if key not in data:
            data[key] = func(*args, **kwargs)

        return data[key]

    return wrapped


def gcd(a, b):
    """Returns the greatest common divisor of a and b"""
    while b != 0:
        a, b = b, a % b

    return a


def is_prime(n):
    if n <= 1:
        return False

    for i in xrange(2, int(sqrt(n)) + 1):
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

    sieve = set(range(1, (limit + 1)/2))

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


def idivisors(n):
    """Yields the sorted divisors of `n`"""
    isqrtn = int(sqrt(n))

    for i in xrange(1, isqrtn):
        if n % i == 0:
            yield i

    if n % isqrtn == 0:
        yield isqrtn

        if isqrtn != n/isqrtn:
            yield n/isqrtn

    for i in xrange(isqrtn-1, 0, -1):
        if n % i == 0:
            yield n/i


def divisor_pairs(n):
    """Yields the divisor pairs of `n`"""

    for i in range(1, int(sqrt(n) + 1)):
        if n % i == 0:
            yield i, n/i


def product(ns):
    """Returns the product of the sequence `ns`"""
    return reduce(lambda a, b: a*b, ns, 1)


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


def digits(n):
    """Returns the digits of n"""
    return map(int, str(n))


def multicount(start, steps):
    """
    A modified version of itertools.count() that adds support for unevenly
    spaced values

    >>> zip(range(7), multicount(10, 1, 3, 10))
    [(0, 10), (1, 11), (2, 13), (3, 20), (4, 21), (5, 23), (6, 30)]

    >>> zip(range(5), multicount(100, 25, 100))
    [(0, 100), (1, 125), (2, 200), (3, 225), (4, 300)]
    """
    steps = sorted(steps) or [1]

    for n in count(start, steps[-1]):
        yield n

        for step in steps[:-1]:
            yield n + step


def is_square(n):
    """Returns `True` if n is a perfect square"""
    return int(sqrt(n) + 0.5)**2 == n
