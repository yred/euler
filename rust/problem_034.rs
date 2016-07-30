// Problem 34 - Digit factorials
//
// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
//
// Find the sum of all numbers which are equal to the sum of the factorial of
// their digits.
//
// Note: as 1! = 1 and 2! = 2 are not sums they are not included.

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let mut total = 0;

    let facts = (0..10).map(factorial).collect::<Vec<usize>>();

    let nmin = 10;
    let nmax = facts[9]*7;  // Solutions are at most 7 digits long

    for n in nmin..nmax {
        let digit_sum = digits(n).iter().fold(0, |sum, d| sum + facts[*d]);

        if digit_sum == n {
            total += n;
        }
    }

    total
}

fn factorial(n: usize) -> usize {
    if n <= 1 {
        return 1
    }
    n * factorial(n - 1)
}

fn digits(n : usize) -> Vec<usize> {
    n.to_string()
     .chars()
     .map(|c| c.to_digit(10).unwrap() as usize)
     .collect()
}