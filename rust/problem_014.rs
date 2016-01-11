// Problem 14 - Longest Collatz sequence
//
// The following iterative sequence is defined for the set of positive integers:
//
//     n → n/2 (n is even)
//     n → 3n + 1 (n is odd)
//
// Using the rule above and starting with 13, we generate the following sequence:
//
//     13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//
// It can be seen that this sequence (starting at 13 and finishing at 1) contains
// 10 terms. Although it has not been proved yet (Collatz Problem), it is thought
// that all starting numbers finish at 1.
//
// Which starting number, under one million, produces the longest chain?
//
// NOTE: Once the chain starts the terms are allowed to go above one million.

fn main() {
    println!("{}", solution());
}

fn solution() -> u64 {
    let mut maxlen = 1;
    let mut maxnum = 1;

    for n in 2..1000000 {
        let length = collatz_seq_length(n);

        if length > maxlen {
            maxlen = length;
            maxnum = n;
        }
    }

    maxnum
}

fn collatz_seq_length(n: u64) -> u64 {
    if n == 1 {
        1
    } else if n % 2 == 0 {
        1 + collatz_seq_length(n/2)
    } else {
        1 + collatz_seq_length(3*n + 1)
    }
}
