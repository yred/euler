# -*- coding: utf-8 -*-
"""
Problem 59 - XOR decryption

Each character on a computer is assigned a unique code and the preferred
standard is ASCII (American Standard Code for Information Interchange). For
example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII,
then XOR each byte with a given value, taken from a secret key. The advantage
with the XOR function is that using the same encryption key on the cipher text,
restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text
message, and the key is made up of random bytes. The user would keep the
encrypted message and the encryption key in different locations, and without
both "halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified
method is to use a password as a key. If the password is shorter than the
message, which is likely, the key is repeated cyclically throughout the
message. The balance for this method is using a sufficiently long password key
for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower
case characters. Using "../resources/p059_cipher.txt", a file containing the
encrypted ASCII codes, and the knowledge that the plain text must contain
common English words, decrypt the message and find the sum of the ASCII values
in the original text.
"""
from itertools import cycle, izip, permutations
import re


def solution():
    with open('../resources/english_common_10000.txt') as f:
        common = set(l.strip() for l in f.readlines())

    with open('../resources/p059_cipher.txt') as f:
        cipher = map(int, f.read().split(','))

    key_space = range(ord('a'), ord('z') + 1)
    non_lower = re.compile(r'[^a-z ]')

    for key in permutations(key_space, 3):
        ptext = ''.join(chr(c ^ k) for c, k in izip(cipher, cycle(key)))
        words = non_lower.sub('', ptext.lower()).split()

        recognized = filter(lambda w: w in common, words)
        if recognized and len(recognized)*100 / len(words) > 80:
            return (sum(map(ord, ptext)), ''.join(map(chr, key)))[0]


if __name__ == '__main__':
    print(solution())
