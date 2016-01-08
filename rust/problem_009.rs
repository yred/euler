// Problem 9 - Special Pythagorean triplet
//
// A Pythagorean triplet is a set of three natural numbers, a < b < c, for
// which, a² + b² = c²
//
// For example, 3² + 4² = 9 + 16 = 25 = 5².
//
// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
// Find the product abc.

fn main() {
    println!("{}", solution());
}

fn solution() -> i32 {
    let perimeter = 1000;

    let min_a = (perimeter/3) + 1;
    let max_a = perimeter - 2;

    let mut product = 0;

    'outer: for a in min_a..(max_a + 1) {
        let max_b = (perimeter - a)/2;

        for b in 1..(max_b + 1) {
            let c = perimeter - a - b;

            if a*a == b*b + c*c {
                product = a*b*c;
                break 'outer;
            }
        }
    }

    product
}
