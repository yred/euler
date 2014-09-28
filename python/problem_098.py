# -*- coding: utf-8 -*-
"""
Problem 98 - Anagramic squares

By replacing each of the letters in the word CARE with 1, 2, 9, and 6
respectively, we form a square number: 1296 = 36^2. What is remarkable is that,
by using the same digital substitutions, the anagram, RACE, also forms a square
number: 9216 = 96^2. We shall call CARE (and RACE) a square anagram word pair
and specify further that leading zeroes are not permitted, neither may a
different letter have the same digital value as another letter.

Using "../resources/p098_words.txt", a 16K text file containing nearly
two-thousand common English words, find all the square anagram word pairs (a
palindromic word is NOT considered to be an anagram of itself).

What is the largest square number formed by any member of such a pair?

NOTE: All anagrams formed must be contained in the given text file.
"""
from collections import defaultdict
from itertools import count, combinations, groupby, takewhile


def positions(word):
    """
    Returns a dictionary mapping each distinct character in `word` to a list of
    its positions
    """
    positions = defaultdict(list)

    for idx, c in enumerate(word):
        # Inserts indices at the beginning of the list so that they can later
        # be "popped" in insertion order
        positions[c].insert(0, idx)

    return positions


def permuter(original, anagram):
    """
    Returns a function that accepts a string and returns one of its anagrams,
    using the provided inputs as an example of the required permutation
    """
    index = positions(original)
    order = sorted(v[:] for v in index.values())

    permutation = [index[c].pop() for c in anagram]

    def permute(string):
        strindex = positions(string)

        # Only return an anagram if the indices of `string` and `original`
        # match (i.e., character/digit duplicates are contained in the same
        # positions)
        if sorted(strindex.values()) == order:
            return ''.join(string[permutation[i]] for i in range(len(string)))
        else:
            return ''

    return permute


def solution():
    words = defaultdict(list)

    with open('../resources/p098_words.txt') as f:
        for word in f.read().split(','):
            word = word.strip('"')
            words[''.join(sorted(word))].append(word)

    # Filter out words without any anagrams in the file
    anagrams = {k: v for k, v in words.items() if len(v) > 1}

    # Sort the anagram sequences by decreasing length
    anagrams = sorted(anagrams.values(), key=lambda v: len(v[0]), reverse=True)

    # Pre-compute all square anagrams up to a certain length (the length of the
    # longest anagram in the file)
    maxsquare = 10**(len(anagrams[0][0]) + 1)

    squares = defaultdict(lambda: defaultdict(list))
    for n in takewhile(lambda n: n*n < maxsquare, count(1)):
        square = str(n*n)

        # Skip palindromic squares
        if square != square[::-1]:
            # Squares will be indexed by their length and the number of
            # distinct digits they contain
            l = len(square)
            s = len(set(square))

            squares[l, s][''.join(sorted(square))].append(square)

    # Find the maximal anagramic square corresponding to the file's anagrams
    for length, group in groupby(anagrams, key=lambda words: len(words[0])):
        anagramics = defaultdict(list)

        for words in group:
            distinct = len(set(words[0]))

            for wa, wb in combinations(words, 2):
                permute = permuter(wa, wb)

                for grams in squares[length, distinct].values():
                    for square in grams:
                        psquare = permute(square)

                        if psquare in grams:
                            # Simple string comparison is valid in this case,
                            # since both arguments are of equal length
                            key = max(square, psquare)

                            anagramics[key].append((wa, wb, square, psquare))

        if anagramics:
            return max(anagramics)


if __name__ == '__main__':
    print(solution())
