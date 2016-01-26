// Problem 24 - Lexicographic permutations
//
// A permutation is an ordered arrangement of objects. For example, 3124 is one
// possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
// are listed numerically or alphabetically, we call it lexicographic order. The
// lexicographic permutations of 0, 1 and 2 are:
//
//     012   021   102   120   201   210
//
// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,
// 6, 7, 8 and 9?

fn main() {
    println!("{}", solution());
}

fn solution() -> String {
    let mut index = 999999;
    let mut elements = (0..10).map(|d| d.to_string()).collect::<Vec<_>>();

    let counts = (1..elements.len()).rev()
                                    .map(|n| factorial(n as u64))
                                    .collect::<Vec<_>>();

    let mut permutation = Vec::new();

    for count in counts {
        let current = (index/count) as usize;
        permutation.push(elements.remove(current));

        index %= count;
        if index == 0 {
            permutation.extend_from_slice(&elements);
            break;
        }
    }

    permutation[..].join("")
}

fn factorial(n: u64) -> u64 {
    (1..(n + 1)).fold(1, |prod, k| prod*k)
}
