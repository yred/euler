# -*- coding: utf-8 -*-
"""
Problem 158 - Exploring strings for which only one character comes
              lexicographically after its neighbour to the left

Taking three different letters from the 26 letters of the alphabet, character
strings of length three can be formed. Examples are 'abc', 'hat' and 'zyx'.

When we study these three examples we see that for 'abc' two characters come
lexicographically after its neighbour to the left. For 'hat' there is exactly
one character that comes lexicographically after its neighbour to the left.

For 'zyx' there are zero characters that come lexicographically after its
neighbour to the left. In all there are 10400 strings of length 3 for which
exactly one character comes lexicographically after its neighbour to the left.

We now consider strings of n ≤ 26 different characters from the alphabet.

For every n, p(n) is the number of strings of length n for which exactly one
character comes lexicographically after its neighbour to the left.

What is the maximum value of p(n)?
"""
from common import ncr


def solution():
    minlen, maxlen = 3, 26
    alphabet = range(26)

    pvalues = {}
    for length in range(minlen, maxlen + 1):
        pvalues[length] = 0

        for insertion_idx in range(length - 1):
            lpositions = insertion_idx
            rpositions = length - insertion_idx - 2

            # `rchar` is the character on the RHS of the insertion
            for rchar in alphabet[rpositions+1:]:
                right_possibs = rchar - 1

                # `lchar` is the character on the LHS of the insertion
                for lchar in alphabet[:rchar]:

                    # `upper_possibs` are characters that are lexicographically
                    # "greater" than `rchar`
                    upper_possibs = len(alphabet) - (rchar + 1)

                    # `inter_possibs` are characters that are lexicographically
                    # between `lchar` and `rchar`
                    inter_possibs = rchar - lchar - 1

                    if upper_possibs + inter_possibs < lpositions:
                        break

                    upper_limit = min(lpositions, upper_possibs) + 1
                    for upper_count in range(upper_limit):
                        inter_count = lpositions - upper_count
                        if inter_count > inter_possibs:
                            continue

                        right_actual = right_possibs - inter_count
                        if right_actual < rpositions:
                            continue

                        pvalues[length] += (ncr(upper_possibs, upper_count) *
                                            ncr(inter_possibs, inter_count) *
                                            ncr(right_actual, rpositions))

    return max(pvalues.values())


if __name__ == '__main__':
    print(solution())
