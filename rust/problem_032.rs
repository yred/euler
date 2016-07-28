// Problem 32 - Pandigital products
//
// We shall say that an n-digit number is pandigital if it makes use of all the
// digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
// through 5 pandigital.
//
// The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
// multiplicand, multiplier, and product is 1 through 9 pandigital.
//
// Find the sum of all products whose multiplicand/multiplier/product identity
// can be written as a 1 through 9 pandigital.
//
// HINT: Some products can be obtained in more than one way so be sure to only
// include it once in your sum.
use std::collections::HashSet;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let mut products = HashSet::new();
    let mut sum = 0;

    for start_pair in vec![(1, 1000), (10, 100)] {
        let m = start_pair.0;
        let n = start_pair.1;

        for a in m..(m*10) {
            for b in n..(n*10) {
                if a*b >= 10000 {
                    break;
                }

                let p = a*b;
                if products.contains(&p) {
                    continue;
                }

                let concatenated = a.to_string() + &b.to_string() + &p.to_string();
                if is_pandigital(concatenated)  {
                    products.insert(p);
                    sum += p;
                }
            }
        }
    }

    sum
}

fn is_pandigital(s: String) -> bool {
    let mut chars = s.chars().collect::<Vec<char>>();
    chars.sort();

    let sorted = chars.iter().cloned().collect::<String>();
    sorted == "123456789"
}
