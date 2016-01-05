// Problem 6 - Sum square difference
//
// The sum of the squares of the first ten natural numbers is,
//
//             1^2 + 2^2 + ... + 10^2 = 385
//
// The square of the sum of the first ten natural numbers is,
//
//             (1 + 2 + ... + 10)^2 = 55^2 = 3025
//
// Hence the difference between the sum of the squares of the first ten natural
// numbers and the square of the sum is 3025 - 385 = 2640.
//
// Find the difference between the sum of the squares of the first one hundred
// natural numbers and the square of the sum.

fn main() {
    println!("{}", solution());
}

fn solution() -> i32 {
    let limit = 101 as i32;

    let sumsq = (1..limit).map(|n| n.pow(2)).fold(0, sum);
    let sqsum = (1..limit).fold(0, sum).pow(2);

    sqsum - sumsq
}

fn sum(a: i32, b: i32) -> i32 {
    a + b
}
