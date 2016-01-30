// Problem 26 - Reciprocal cycles
//
// A unit fraction contains 1 in the numerator. The decimal representation of the
// unit fractions with denominators 2 to 10 are given:
//
//     1/2  =   0.5
//     1/3  =   0.(3)
//     1/4  =   0.25
//     1/5  =   0.2
//     1/6  =   0.1(6)
//     1/7  =   0.(142857)
//     1/8  =   0.125
//     1/9  =   0.(1)
//     1/10 =   0.1
//
// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
// seen that 1/7 has a 6-digit recurring cycle.
//
// Find the value of d < 1000 for which 1/d contains the longest recurring cycle
// in its decimal fraction part.
use std::collections::HashMap;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let digits = 2100;
    let mut cyclens = Vec::new();

    for n in 2..1000 {
        let mut res = reciprocal(n, digits);

        if res.len() == digits {
            // the first few digits may be noisy
            res = res.split_off(100);

            cyclens.push((find_cycle_len(&res), n));
        }
    }

    cyclens.iter().max().unwrap().1
}

fn reciprocal(n: usize, max_digits: usize) -> Vec<usize> {
    let mut dividend = 1;
    let mut result = Vec::new();

    for _ in 0..max_digits {
        result.push(dividend/n);

        dividend %= n;
        dividend *= 10;

        if dividend == 0 {
            break;
        }
    }

    result
}

fn least_frequent(v: &Vec<usize>) -> usize {
    let mut index = HashMap::new();

    for &elem in v {
        let new_count = match index.get(&elem) {
            Some(count) => count + 1,
            None        => 1,
        };

        index.insert(elem, new_count);
    }

    let mut counts = Vec::new();
    for (elem, count) in index {
        counts.push((count, elem));
    }

    let &(_, elem) = counts.iter().min().unwrap();
    elem
}

fn find_cycle_len(v: &Vec<usize>) -> usize {
    let lf_digit = least_frequent(v);

    let mut start = 0;

    for (ix, &digit) in v.iter().enumerate() {
        if digit == lf_digit {
            if start == 0 {
                start = ix;
                continue;
            }

            let length = ix - start;
            let first  = &v[start..ix];

            if v[ix..].chunks(length).all(|c| c.len() < length || c == first) {
                return length;
            }
        }
    }

    0
}
