# -*- coding: utf-8 -*-
"""
Problem 191 - Prize Strings

A particular school offers cash rewards to children with good attendance and
punctuality. If they are absent for three consecutive days or late on more than
one occasion then they forfeit their prize.

During an n-day period a trinary string is formed for each child consisting of
L's (late), O's (on time), and A's (absent).

Although there are eighty-one trinary strings for a 4-day period that can be
formed, exactly forty-three strings would lead to a prize:

        OOOO OOOA OOOL OOAO OOAA OOAL OOLO OOLA OAOO OAOA
        OAOL OAAO OAAL OALO OALA OLOO OLOA OLAO OLAA AOOO
        AOOA AOOL AOAO AOAA AOAL AOLO AOLA AAOO AAOA AAOL
        AALO AALA ALOO ALOA ALAO ALAA LOOO LOOA LOAO LOAA
        LAOO LAOA LAAO

How many "prize" strings exist over a 30-day period?
"""
from common import memoize


@memoize
def prizes(days, cons_abs=0, late=0):
    """Returns how many prize strings exist over a period of `days` days"""
    if cons_abs >= 3 or late > 1:
        return 0

    if days == 0:
        return 1

    return (
        prizes(days - 1, late=late) +
        prizes(days - 1, cons_abs + 1, late) +
        prizes(days - 1, late=late + 1)
    )


def solution():
    return prizes(30)


if __name__ == '__main__':
    print(solution())
