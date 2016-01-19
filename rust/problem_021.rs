// Problem 21 - Amicable numbers
//
// Let d(n) be defined as the sum of proper divisors of n (numbers less than n
// which divide evenly into n).
//
// If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
// each of a and b are called amicable numbers.
//
// For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
// and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
// 142; so d(284) = 220.
//
// Evaluate the sum of all the amicable numbers under 10000.
use std::collections::HashMap;

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let mut dvalues = HashMap::new();
    let mut aminums = Vec::new();

    let limit = 10000;

    for a in 1..limit {
        match dvalues.get(&a) {
            Some(_) => continue,
            None    => {
                let b = d(a);
                let c = d(b);

                if c == a {
                    aminums.push(a);
                    aminums.push(b);
                }

                dvalues.insert(a, b);
                dvalues.insert(b, c);
            }
        }
    }

    aminums.iter()
           .filter(|&n| *n < limit)
           .fold(0, |sum, n| sum + n)
}

fn d(n: u32) -> u32 {
    let upper = (n as f64).sqrt().floor() as u32;
    let mut sum = 0;

    for k in 1..(upper + 1) {
        if n % k == 0 {
            sum += k
        }
    }

    sum
}
