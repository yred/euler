// Problem 36 - Double-base palindromes
//
// The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
//
// Find the sum of all numbers, less than one million, which are palindromic in
// base 10 and base 2.
//
// (Please note that the palindromic number, in either base, may not include
// leading zeros.)

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let mut total = 0;

    for n in 1..1_000_000 {
        if n % 2 == 0 {
            continue
        }

        if palindrome(n, 10) && palindrome(n, 2) {
            total += n
        }
    }

    total
}

fn palindrome(n: usize, base: usize) -> bool {
    let ds = digits(n, base);

    let mut rds = ds.to_vec();
    rds.reverse();

    equal(ds, rds)
}

fn digits(n: usize, base: usize) -> Vec<usize> {
    let mut ds = Vec::new();

    let mut _n = n;
    while _n >= base {
        ds.push(_n % base);
        _n = _n/base;
    }

    if _n > 0 {
        ds.push(_n);
    }

    ds
}

fn equal(a: Vec<usize>, b: Vec<usize>) -> bool {
    a.iter().zip(b.iter()).all(|(_a, _b)| _a == _b)
}
