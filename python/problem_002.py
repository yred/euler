"""
Problem 2 - Even Fibonacci numbers

Each new term in the Fibonacci sequence is generated by adding the previous two
terms. By starting with 1 and 2, the first 10 terms will be:

    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed
four million, find the sum of the even-valued terms.
"""
from itertools import count, takewhile


def memoize(func):
    """
    Returns a memoized version of `func`, where `func` is a function that
    accepts a single argument
    """
    data = {}

    def wrapped(arg):
        if arg not in data:
            data[arg] = func(arg)

        return data[arg]

    return wrapped


@memoize
def fibonacci(n):
    if n in [1, 2]:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)


def solution():
    return sum(val for val in takewhile(lambda n: n <= 4000000,
                                        (fibonacci(n) for n in count(1)))
               if val % 2 == 0)


if __name__ == '__main__':
    print(solution())
