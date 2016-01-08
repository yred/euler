// Problem 10 - Summation of primes
//
// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
// Find the sum of all the primes below two million.

fn main() {
    println!("{}", solution());
}

fn solution() -> u64 {
    (2..).filter(|&n| is_prime(n))
         .take_while(|&p| p < 2000000)
         .fold(0, |sum, p| sum + p)
}

fn is_prime(n: u64) -> bool {
    let isqrn = (n as f64).sqrt().floor() as u64;

    for k in 2..(isqrn + 1) {
        if n % k == 0 {
            return false;
        }
    }

    true
}
