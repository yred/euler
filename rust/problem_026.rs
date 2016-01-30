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

fn solution() -> u32 {
    let mut xs = reciprocal(7, 50);
    xs = xs.split_off(10);

    least_frequent(&xs)
}

fn reciprocal(n: u32, max_digits: u32) -> Vec<u32> {
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

fn least_frequent(v: &Vec<u32>) -> u32 {
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
