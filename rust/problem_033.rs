// Problem 33 - Digit canceling fractions
//
// The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
// attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
// correct, is obtained by cancelling the 9s.
//
// We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
//
// There are exactly four non-trivial examples of this type of fraction, less than
// one in value, and containing two digits in the numerator and denominator.
//
// If the product of these four fractions is given in its lowest common terms,
// find the value of the denominator.

fn main() {
    println!("{}", solution());
}

#[derive(PartialEq, Eq)]
struct Fraction {
    numer: usize,
    denom: usize,
}

impl Fraction {
    fn reduce(&self) -> Fraction {
        let d = gcd(self.numer, self.denom);
        Fraction{numer: self.numer/d, denom: self.denom/d}
    }
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }

    gcd(b, a%b)
}

fn solution() -> usize {
    let mut p_numer = 1;
    let mut p_denom = 1;

    for denom in 11..100 {
        if denom%10 == 0 {
            continue;
        }

        for numer in 11..denom {
            if numer%10 == 0 || numer%10 != denom/10 {
                continue;
            }

            let reduced = Fraction{numer: numer, denom: denom}.reduce();
            if reduced.numer >= 10 || reduced.denom >= 10 {
                continue;
            }

            let fake_reduced = Fraction{numer: numer/10, denom: denom%10}.reduce();
            if reduced == fake_reduced {
                p_numer *= numer;
                p_denom *= denom;
            }
        }
    }

    let product = Fraction{numer: p_numer, denom: p_denom}.reduce();
    product.denom
}
