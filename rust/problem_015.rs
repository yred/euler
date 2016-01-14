// Problem 15 - Lattice paths
//
// Starting in the top left corner of a 2×2 grid, and only being able to move to
// the right and down, there are exactly 6 routes to the bottom right corner.
//
// How many such routes are there through a 20×20 grid?

fn main() {
    println!("{}", solution());
}

fn solution() -> u64 {
    nCr(40, 20)
}

fn nCr(n: u64, r: u64) -> u64 {
    let mut numer = 1;
    let mut denom = 1;

    // Attempting to solve this without resorting to big integers...
    for k in 1..(n - r + 1) {
        numer *= r + k;
        denom *= k;

        let d = gcd(numer, denom);
        numer /= d;
        denom /= d;
    }

    numer/denom
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 {
        return a;
    }

    gcd(b, a % b)
}
