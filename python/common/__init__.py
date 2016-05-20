from itertools import count
from math import sqrt, factorial


def memoize(fn=None, ignore_kwargs=False):
    """
    Create a memoized version of a function.

    Note: can be used:

        - As a simple decorator:

            @memoize
            def do_x(a):
                # ...

        - In a customized manner:

            @memoize(ignore_kwargs=True):
            def do_y(b, c=True):
                # ...
    """
    def wrapper(func):
        data = {}

        def wrapped(*args, **kwargs):
            if ignore_kwargs:
                key = tuple(list(args))
            else:
                key = tuple(list(args) + sorted(kwargs.items()))

            if key not in data:
                data[key] = func(*args, **kwargs)

            return data[key]

        return wrapped

    if fn:
        return wrapper(fn)
    else:
        return wrapper


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
    """Yields all primes up to `limit`"""
    yield 2

    maxindex = (limit - 1)/2
    numbytes = maxindex/8
    if maxindex % 8 != 0:
        numbytes += 1
    sieve = bytearray(numbytes)

    # Set "non-primes" to 1, using the sieve of Sundaram
    for i in xrange(1, int(sqrt(limit/2)) + 1):
        for j in xrange(1, (limit - i)/(1 + 2*i) + 1):
            index = i + j + 2*i*j - 1
            if index >= maxindex:
                break

            sieve[index/8] |= 1 << (index % 8)

    # Mark the sieve's "extra bits" as non-primes
    for k in range(maxindex, numbytes*8):
        sieve[k/8] |= 1 << (k % 8)

    for i, byte in enumerate(sieve):
        first = i*8 + 1
        for j in range(8):
            if not byte & (1 << j):
                yield 2*(first + j) + 1


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

    >>> zip(range(7), multicount(10, [1, 3, 10]))
    [(0, 10), (1, 11), (2, 13), (3, 20), (4, 21), (5, 23), (6, 30)]

    >>> zip(range(5), multicount(100, [25, 100]))
    [(0, 100), (1, 125), (2, 200), (3, 225), (4, 300)]
    """
    steps = sorted(steps)

    for n in count(start, steps[-1]):
        yield n

        for step in steps[:-1]:
            yield n + step


def is_square(n):
    """Returns `True` if n is a perfect square"""
    return int(sqrt(n) + 0.5)**2 == n


class OrderedSet(object):

    def __init__(self, iterable):
        self.values = list(iterable)
        self.index = set(self.values)

    def __getitem__(self, item):
        return self.values[item]

    def __iter__(self):
        return iter(self.values)

    def __contains__(self, value):
        return value in self.index
