from itertools import count, islice
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


def divisors(n):
    """Returns the divisors of `n` as a sorted list"""
    ds = set()

    for i in range(1, int(sqrt(n) + 1)):
        if n % i == 0:
            ds.add(i)
            ds.add(n/i)

    return list(sorted(ds))


class Numbers(object):
    """
    Container-like sequence, generated via an invertible function defined over
    strictly positive integers
    """

    def __init__(self, function, inverse):
        self.function = function
        self.inverse = inverse

    def __getitem__(self, n):
        """
        Returns:

            - when used with slices, an iterator using the normal (0-indexed)
              slice semantics -- i.e.,

                    list(instance[0:1])[0] == instance function applied to 1,
                                              the first valid input value

            - when used with (strictly positive) integers, the value of the
              instance's defining function applied to the provided input.
        """
        if isinstance(n, slice):
            if any(idx is not None and idx < 0 for idx in (n.start, n.stop)):
                raise IndexError('Slice indices must be positive')

            # Note: negative steps are not supported. Using islice()
            # automatically handles this edge case by raising a ValueError
            return islice(iter(self), n.start, n.stop, n.step)

        elif isinstance(n, (int, long)):
            if n <= 0:
                raise IndexError('Indices must be strictly positive')

            return self.function(n)

        else:
            raise TypeError("Only integer and slice indices are supported")

    def __contains__(self, n):
        return n == self.function(self.inverse(n))

    def __iter__(self):
        for n in count(1):
            yield self.function(n)


triangles = Numbers(function=lambda n: n*(n+1)/2,
                    inverse=lambda n: int(sqrt(2*n)))

pentagonals = Numbers(function=lambda n: n*(3*n - 1)/2,
                      inverse=lambda n: int(sqrt(2*n / 3)) + 1)

hexagonals = Numbers(function=lambda n: n*(2*n - 1),
                     inverse=lambda n: int(sqrt(n/2)) + 1)
