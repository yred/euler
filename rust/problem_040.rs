// Problem 40 - Champernowne's constant
//
// An irrational decimal fraction is created by concatenating the positive
// integers:
//
//         0.12345678910[1]112131415161718192021...
//
// It can be seen that the 12th digit of the fractional part is 1.
//
// If d(n) represents the n-th digit of the fractional part, find the value of the
// following expression.
//
//     d(1) × d(10) × d(100) × d(1000) × d(10000) × d(100000) × d(1000000)

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let indices = vec![1, 10, 100, 1_000, 10_000, 100_000, 1_000_000];
    let max_idx = *indices.last().unwrap();

    let fstr = (1..max_idx).map(|n| n.to_string())
                           .collect::<String>();
    let fbytes = fstr.as_bytes();

    let zero = '0' as u8;
    let mut product: usize = 1;

    for idx in indices {
        product *= (fbytes[idx - 1] - zero) as usize;
    }

    product
}
