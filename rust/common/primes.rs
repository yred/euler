pub struct Primes {
    previous: Vec<usize>,
}

impl Primes {
    pub fn new() -> Primes {
        Primes{ previous: Vec::new() }
    }
}

impl Iterator for Primes {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        let start = match self.previous.last() {
            Some(&n) => n+1,
            None     => 2,
        };

        let mut next_prime = 0;

        for n in start.. {
            let isqrn = (n as f64).sqrt().floor() as usize;

            if self.previous.iter().take_while(|k| **k <= isqrn).all(|k| n%k > 0) {
                next_prime = n;
                break;
            }
        }

        self.previous.push(next_prime);
        Some(next_prime)
    }
}
