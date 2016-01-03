// Problem 4 - Largest palindrome product
//
// A palindromic number reads the same both ways. The largest palindrome made
// from the product of two 2-digit numbers is 9009 = 91 × 99.
//
// Find the largest palindrome made from the product of two 3-digit numbers.

fn main() {
    println!("{}", solution());
}

fn solution() -> i32 {
    let mut result = 0;

    for a in 100..1000 {
        for b in 100..(a+1) {
            let n = a*b;

            if n > result && palindrome(n) {
                result = n;
            }
        }
    }

    result
}

fn palindrome(n: i32) -> bool {
    let ds = digits(n);

    let mut rds = ds.to_vec();
    rds.reverse();

    equal(ds, rds)
}

fn equal(a: Vec<i32>, b: Vec<i32>) -> bool {
    a.iter().zip(b.iter()).all(|(_a, _b)| _a == _b)
}

fn digits(n: i32) -> Vec<i32> {
    let mut current = n;
    let mut ndigits = vec![];

    while current > 0 {
        ndigits.push(current % 10);
        current /= 10;
    }

    ndigits.reverse();
    ndigits
}
