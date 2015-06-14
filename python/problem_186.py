# -*- coding: utf-8 -*-
"""
Problem 186 - Connectedness of a network

Here are the records from a busy telephone system with one million users:

            RecNr   Caller   Called
            1       200007   100053
            2       600183   500439
            3       600863   701497
            ...     ...      ...

The telephone number of the caller and the called number in record n are
Caller(n) = S(2n-1) and Called(n) = S(2n) where S(1,2,3,...) come from the "Lagged
Fibonacci Generator":

            For 1 ≤ k ≤ 55, S(k) = [100003 - 200003k + 300007k^3] (modulo 1000000)
            For 56 ≤ k, S(k) = [S(k-24) + S(k-55)] (modulo 1000000)

If Caller(n) = Called(n) then the user is assumed to have misdialled and the
call fails; otherwise the call is successful.

From the start of the records, we say that any pair of users X and Y are friends
if X calls Y or vice-versa. Similarly, X is a friend of a friend of Z if X is a
friend of Y and Y is a friend of Z; and so on for longer chains.

The Prime Minister's phone number is 524287. After how many successful calls,
not counting misdials, will 99% of the users (including the PM) be a friend, or
a friend of a friend etc., of the Prime Minister?
"""
from collections import deque
from itertools import count


class S(object):
    def __init__(self):
        self._vals = deque([], 55)

    def __iter__(self):
        for k in count(start=1):
            if k <= 55:
                curval = (100003 - 200003*k + 300007*(k**3)) % 1000000
            else:
                curval = (self._vals[-24] + self._vals[-55]) % 1000000

            yield curval
            self._vals.append(curval)


def calls():
    phone_numbers = iter(S())

    while True:
        caller = next(phone_numbers)
        called = next(phone_numbers)
        if caller != called:
            yield caller, called


def solution():
    net_ids = count()
    network = {}
    members = {}

    pm_number = 524287
    threshold = 0.99 * 10**6

    for n, (a, b) in enumerate(calls(), start=1):
        if a in network and b in network:
            if network[a] == network[b]:
                continue

            larger, smaller = network[a], network[b]
            if len(members[network[a]]) < len(members[network[b]]):
                larger, smaller = smaller, larger

            for member in members[smaller]:
                network[member] = larger

            members[larger] |= members[smaller]
            del members[smaller]

        elif a in network:
            network[b] = network[a]
            members[network[a]].add(b)

        elif b in network:
            network[a] = network[b]
            members[network[b]].add(a)

        else:
            net_id = next(net_ids)
            network[a] = network[b] = net_id
            members[net_id] = set([a, b])

        if pm_number in network:
            pm_friend_count = len(members[network[pm_number]])
            if pm_friend_count >= threshold:
                return n


if __name__ == '__main__':
    print(solution())
