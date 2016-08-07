// Problem 41 - Pandigital prime
//
// We shall say that an n-digit number is pandigital if it makes use of all the
// digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
// also prime.
//
// What is the largest n-digit pandigital prime that exists?
mod common;
use common::primes::Primes;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    // 8 and 9-digit pandigital numbers cannot be prime. The largest pandigital
    // must be at most 7 digits long
    let pmin = 1_000_000;
    let pmax = 9_999_999;

    let primes = Primes::new()
                        .skip_while(|&p| p < pmin)
                        .take_while(|&p| p < pmax)
                        .collect::<Vec<usize>>();

    let mut result = 0;
    for &p in primes.iter().rev() {
        if is_pandigital17(p) {
            result = p;
            break;
        }
    }

    result
}

fn is_pandigital17(n: usize) -> bool {
    let mut chars = n.to_string().chars().collect::<Vec<char>>();
    chars.sort();

    let sorted = chars.iter().cloned().collect::<String>();
    sorted == "1234567"
}
