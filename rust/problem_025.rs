// Problem 25 - 1000-digit Fibonacci number
//
// The Fibonacci sequence is defined by the recurrence relation:
//
//     F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
//
// Hence the first 12 terms will be:
//
//     F(1) = 1
//     F(2) = 1
//     F(3) = 2
//     F(4) = 3
//     F(5) = 5
//     F(6) = 8
//     F(7) = 13
//     F(8) = 21
//     F(9) = 34
//     F(10) = 55
//     F(11) = 89
//     F(12) = 144
//
// The 12th term, F(12), is the first term to contain three digits.
//
// What is the first term in the Fibonacci sequence to contain 1000 digits?

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let threshold = 1000;

    let mut a = "1".to_string();
    let mut b = "1".to_string();

    let mut first = 0;

    for term in 3.. {
        let sum = add(&a, &b);

        if sum.len() >= threshold {
            first = term;
            break;
        }

        a = b;
        b = sum;
    }

    first
}

fn add(a: &String, b: &String) -> String {
    let digits_a = digits(a);
    let digits_b = digits(b);

    let mut sum = Vec::new();
    let mut carry = 0;

    // `b` is assumed to be "longer"/"greater" than `a`
    for (ix, da) in digits_a.iter().enumerate() {
        let dsum = carry + da + digits_b[ix];

        sum.push(dsum % 10);
        carry = dsum/10;
    }

    for db in digits_b.iter().skip(digits_a.len()) {
        let dsum = carry + db;

        sum.push(dsum % 10);
        carry = dsum/10;
    }

    if carry != 0 {
        sum.push(carry);
    }

    sum.iter()
       .rev()
       .fold(String::new(), |mut acc, d| { acc.push_str(&*d.to_string()); acc })
}

fn digits(nstr: &String) -> Vec<u32> {
    nstr.chars().rev().map(|c| c.to_digit(10).unwrap() as u32).collect()
}
