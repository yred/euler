"""
Problem 19 - Counting Sundays

You are given the following information, but you may prefer to do some research
for yourself.

    - 1 Jan 1900 was a Monday.

    - Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.

    - A leap year occurs on any year evenly divisible by 4, but not on a
      century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century
(1 Jan 1901 to 31 Dec 2000)?
"""
from itertools import count, dropwhile, takewhile


# Months with 30 days, January being 0
THIRTIES = [3, 5, 8, 10]


def is_leap(year):
    return (year % 100 and year % 4 == 0) or year % 400 == 0


def days(month, year):
    if month == 1:
        return 29 if is_leap(year) else 28
    elif month in THIRTIES:
        return 30
    else:
        return 31


def calendar():
    # Day of the week at the start date (01/01/1900)
    day = 0

    for year in count(1900):
        for month in range(12):
            for date in range(days(month, year)):
                yield (day, date, month, year)
                day = (day + 1) % 7


def solution():
    century = takewhile(lambda t: t[-1] <= 2000,
                        dropwhile(lambda t: t[-1] < 1901,
                                  calendar()))

    return len([1 for day, date, _, _ in century if day == 6 and date == 1])


if __name__ == '__main__':
    print(solution())
