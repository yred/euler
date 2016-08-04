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
    let mut squares = HashMap::new();
    for n in 1..1000 {
        squares.insert(n*n, n);
    }

    let mut perimeters = HashMap::new();

    for c in 2..1000 {
        for a in 1..c {
            let bsq = c*c - a*a;
            if bsq <= a*a && squares.contains_key(&bsq) {
                let b = squares.get(&bsq).unwrap();
                let p = a as usize + b + c;

                let new_count = match perimeters.get(&p) {
                    Some(count) => count + 1,
                    None        => 1
                };
                perimeters.insert(p, new_count);
            }
        }
    }

    let mut maxp = 0;
    let mut maxc = 0;

    for (&p, &c) in perimeters.iter() {
        if c > maxc {
            maxc = c;
            maxp = p;
        }
    }

    maxp
}
