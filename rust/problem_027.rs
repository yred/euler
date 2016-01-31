// Problem 27 - Quadratic primes
//
// Euler discovered the remarkable quadratic formula:
//
//         n² + n + 41
//
// It turns out that the formula will produce 40 primes for the consecutive values
// n = 0 to 39. However, when n = 40, 40² + 40 + 41 = 40(40 + 1) + 41 is divisible
// by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
//
// The incredible formula  n² − 79n + 1601 was discovered, which produces 80
// primes for the consecutive values n = 0 to 79. The product of the coefficients,
// −79 and 1601, is −126479.
//
// Considering quadratics of the form:
//
//     n² + an + b, where |a| < 1000 and |b| < 1000
//
//     where |n| is the modulus/absolute value of n
//     e.g. |11| = 11 and |−4| = 4
//
// Find the product of the coefficients, a and b, for the quadratic expression
// that produces the maximum number of primes for consecutive values of n,
// starting with n = 0.
use std::collections::HashSet;

fn main() {
    println!("{}", solution());
}

struct Primes {
    previous: Vec<usize>,
}

impl Iterator for Primes {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        let start = match self.previous.last() {
            Some(&n) => n+1,
            None     => 2,
        };

        let mut next_prime = 0;

        for n in start.. {
            let isqrn = (n as f64).sqrt().floor() as usize;

            if self.previous.iter().take_while(|k| **k <= isqrn).all(|k| n%k > 0) {
                next_prime = n;
                break;
            }
        }

        self.previous.push(next_prime);
        Some(next_prime)
    }
}

fn solution() -> isize {
    let max_b = 1000;
    let b_values = Primes{previous: Vec::new()}.take_while(|&p| p < max_b)
                                               .map(|p| p as isize)
                                               .collect::<Vec<_>>();

    let max_a = 1000;
    let min_a = -100;

    let primeset = Primes{previous: Vec::new()}.take_while(|&p| p < 2*max_b*max_b)
                                               .collect::<HashSet<_>>();

    // For a = 1 and b = 41
    let mut max_product = 41;
    let mut max_primes  = 40;

    for b in b_values {
        for a in min_a..max_a {

            // `a` must be odd
            if a%2 == 0 {
                continue;
            }

            for n in 1.. {
                let value = n*n + a*n + b;

                if value < 0 || !primeset.contains(&(value as usize)) {
                    if n > max_primes {
                        max_product = a*b;
                        max_primes  = n;
                    }
                    break;
                }
            }

        }
    }

    max_product
}
