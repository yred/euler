// Problem 23 - Non-abundant sums
//
// A perfect number is a number for which the sum of its proper divisors is
// exactly equal to the number. For example, the sum of the proper divisors of 28
// would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
//
// A number n is called deficient if the sum of its proper divisors is less than n
// and it is called abundant if this sum exceeds n.
//
// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
// number that can be written as the sum of two abundant numbers is 24.
//
// By mathematical analysis, it can be shown that all integers greater than 28123
// can be written as the sum of two abundant numbers. However, this upper limit
// cannot be reduced any further by analysis even though it is known that the
// greatest number that cannot be expressed as the sum of two abundant numbers is
// less than this limit.
//
// Find the sum of all the positive integers which cannot be written as the sum of
// two abundant numbers.
use std::collections::HashSet;

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let nmax = 28123;

    let mut abundant = HashSet::new();
    let mut nsum = 0;

    for n in 1..(nmax + 1) {
        if is_abundant(n) {
            abundant.insert(n);
        }

        if !is_sum(n, &abundant) {
            nsum += n;
        }
    }

    nsum
}

fn is_abundant(n: u32) -> bool {
    let upper = (n as f64).sqrt().floor() as u32 + 1;
    let mut sum = 1;

    for k in 2..upper {
        if n % k == 0 {
            sum += k;

            if n/k > k {
                sum += n/k;
            }
        }
    }

    sum > n
}

fn is_sum(n: u32, set: &HashSet<u32>) -> bool {
    for k in set {
        if set.contains(&(n - k)) {
            return true
        }
    }

    false
}
