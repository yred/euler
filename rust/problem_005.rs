// Problem 5 - Smallest multiple
//
// 2520 is the smallest number that can be divided by each of the numbers from 1
// to 10 without any remainder.
//
// What is the smallest positive number that is evenly divisible by all of the
// numbers from 1 to 20?

fn main() {
    println!("{}", solution());
}

fn solution() -> i64 {
    (2..21).fold(1, |lcm, n| (lcm*n)/gcd(lcm, n))
}

fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        return a;
    }

    gcd(b, a % b)
}
