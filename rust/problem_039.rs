// Problem 39 - Integer right triangles
//
// If p is the perimeter of a right angle triangle with integral length sides,
// {a,b,c}, there are exactly three solutions for p = 120:
//
//         {20, 48, 52}, {24, 45, 51}, {30, 40, 50}
//
// For which value of p ≤ 1000, is the number of solutions maximised?
use std::collections::HashMap;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let pmax = 1000;

    let mut squares = HashMap::new();
    for n in 1..1000 {
        squares.insert(n*n, n);
    }

    let mut perimeters = HashMap::new();

    // Note: a < b < c
    for c in 2..pmax {
        for a in 1..(c/2) {
            let bsq = c*c - a*a;

            if squares.contains_key(&bsq) {
                let b = squares.get(&bsq).unwrap();

                let p: usize = a + b + c;
                if p > pmax {
                    break;
                }

                let new_count = match perimeters.get(&p) {
                    Some(count) => count + 1,
                    None        => 1
                };
                perimeters.insert(p, new_count);
            }
        }
    }

    perimeters.iter().map(|(&p, &count)| (count, p)).max().unwrap().1
}
