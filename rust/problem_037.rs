// Problem 37 - Truncatable primes
//
// The number 3797 has an interesting property. Being prime itself, it is possible
// to continuously remove digits from left to right, and remain prime at each
// stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
// 379, 37, and 3.
//
// Find the sum of the only eleven primes that are both truncatable from left to
// right and right to left.
//
// NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
use std::collections::HashSet;

mod common;
use common::primes::Primes;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let pcount = 11;

    let mut primeset = HashSet::new();
    let mut truncatable = Vec::new();

    for p in Primes::new() {
        primeset.insert(p);

        if p < 10 {
            continue;
        }

        if l_drop(p).iter().any(|n| !primeset.contains(&n)) {
            continue;
        }

        if r_drop(p).iter().any(|n| !primeset.contains(&n)) {
            continue;
        }

        truncatable.push(p);
        if truncatable.len() == pcount {
            break;
        }
    }

    truncatable.iter().fold(0, |sum, p| sum + p)
}

fn l_drop(n: usize) -> Vec<usize> {
    let mut results = Vec::new();

    let mut pow_10 = 10usize.pow((n.to_string().len() - 1) as u32);
    let mut _n = n;

    while pow_10 >= 10 {
        _n = _n % pow_10;
        results.push(_n);

        pow_10 /= 10;
    }

    results
}

fn r_drop(n: usize) -> Vec<usize> {
    let mut results = Vec::new();

    let mut _n = n;

    while _n >= 10 {
        _n = _n/10;
        results.push(_n);
    }

    results
}
