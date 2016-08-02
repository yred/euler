// Problem 35 - Circular primes
//
// The number, 197, is called a circular prime because all rotations of the
// digits: 197, 971, and 719, are themselves prime.
//
// There are thirteen such primes below 100:
//
//     2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
//
// How many circular primes are there below one million?
use std::collections::HashSet;

mod common;
use common::primes::Primes;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let plimit = 1_000_000;
    let primes = Primes::new().take_while(|&p| p < plimit)
                              .collect::<Vec<usize>>();

    let mut primeset = HashSet::new();
    let mut circulars = Vec::new();

    for p in primes {
        primeset.insert(p);

        let mut is_circular = true;
        let pgroup = rotations(p);

        for &n in &pgroup {
            if !primeset.contains(&n) {
                is_circular = false;
                break;
            }
        }

        if !is_circular {
            continue;
        }

        circulars.push(p);
        circulars.extend(pgroup.iter());
    }

    circulars.len()
}

fn rotations(n: usize) -> Vec<usize> {
    let mut results = Vec::new();

    let pow_10 = 10usize.pow((n.to_string().len() - 1) as u32);
    let mut _n = n;

    while true {
        _n = (_n % 10)*pow_10 + (_n / 10);

        if _n != n {
            results.push(_n);
        } else {
            break;
        }
    }

    results
}
