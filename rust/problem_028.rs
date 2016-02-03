// Problem 28 - Number spiral diagonals
//
// Starting with the number 1 and moving to the right in a clockwise direction a
// 5 by 5 spiral is formed as follows:
//
//                     21 22 23 24 25
//                     20  7  8  9 10
//                     19  6  1  2 11
//                     18  5  4  3 12
//                     17 16 15 14 13
//
// It can be verified that the sum of the numbers on the diagonals is 101.
//
// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
// in the same way?

fn main() {
    println!("{}", solution());
}

struct Spiral {
    curr: usize,
    step: usize,
    side: usize
}

impl Spiral {
    fn new() -> Spiral {
        Spiral{ curr: 1, step: 2, side: 0 }
    }
}

impl Iterator for Spiral {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        let curval = self.curr;

        self.curr += self.step;
        self.side = (self.side + 1) % 4;
        if self.side == 0 {
            self.step += 2;
        }

        Some(curval)
    }
}

fn solution() -> usize {
    let side_len = 1001;
    let diag_cnt = side_len*2 - 1;

    Spiral::new().take(diag_cnt).fold(0, |sum, v| sum + v)
}
