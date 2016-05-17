// Problem 30 - Digit fifth powers
//
// Surprisingly there are only three numbers that can be written as the sum of
// fourth powers of their digits:
//
//         1634 = 1^4 + 6^4 + 3^4 + 4^4
//         8208 = 8^4 + 2^4 + 0^4 + 8^4
//         9474 = 9^4 + 4^4 + 7^4 + 4^4
//
// As 1 = 1^4 is not a sum it is not included.
//
// The sum of these numbers is 1634 + 8208 + 9474 = 19316.
//
// Find the sum of all the numbers that can be written as the sum of fifth powers
// of their digits.

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let power = 5u32;

    let mut max_len = 2;
    while 10u32.pow(max_len - 1) < 9u32.pow(power) * max_len {
        max_len += 1;
    }
    let max_num = 9u32.pow(power) * max_len;

    let mut results = Vec::new();

    for n in 10..(max_num+1) {
        let mut curr = n;
        let mut psum = 0;

        while curr > 0 {
            psum += (curr%10).pow(power);
            curr /= 10;
        }

        if n == psum {
            results.push(n);
        }
    }

    results.iter().fold(0, |sum, val| sum + val)
}
