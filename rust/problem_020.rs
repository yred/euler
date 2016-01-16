// Problem 20 - Factorial digit sum
//
// n! means n x (n - 1) x ... x 3 x 2 x 1
//
// For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
// and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
//
// Find the sum of the digits in the number 100!

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let mut factorial = vec![1];

    for n in 2..101 {
        let mut new_fact = vec![];
        let mut carry  = 0;

        for digit in factorial {
            let product = digit*n + carry;

            new_fact.push(product%10);
            carry = product/10;
        }

        while carry > 0 {
            new_fact.push(carry%10);
            carry /= 10;
        }

        factorial = new_fact;
    }

    factorial.iter().fold(0, |sum, d| sum + d)
}
