// Problem 3 - Largest prime factor
//
// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143?

fn main() {
    println!("{}", solution());
}

fn solution() -> u64 {
    let mut n: u64 = 600851475143;
    let upper = (n as f64).sqrt().floor() as u64;

    let mut maxp = 1;

    for d in 2..upper {
        while n % d == 0 {
            n /= d;
        }

        if n == 1 {
            maxp = d;
            break
        }
    }

    maxp
}
