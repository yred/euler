"""
Problem 9 - Special Pythagorean triplet

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
"""


def solution():
    # a + b + c = 1000 and a < b < c => 3*a < 1000
    for a in range(1000/3 + 1):
        #   b + c = 1000 - a and a < b < c => 2*b < (1000 - a)
        for b in range(a + 1, (1000 - a)/2):
            c = 1000 - a - b
            if a*a + b*b == c*c:
                return a*b*c


if __name__ == '__main__':
    print(solution())
