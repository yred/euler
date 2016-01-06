// Problem 7 - 10001st prime
//
// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
// that the 6th prime is 13.
//
// What is the 10001st prime number?

fn main() {
    println!("{}", solution());
}

fn solution() -> i32 {
    (2..).filter(|&n| is_prime(n)).skip(10000).take(1).next().unwrap()
}

fn is_prime(n: i32) -> bool {
    let isqrn = (n as f64).sqrt().floor() as i32;

    for k in 2..(isqrn + 1) {
        if n % k == 0 {
            return false;
        }
    }

    true
}
