// Problem 31 - Coin sums
//
// In England the currency is made up of pound, £, and pence, p, and there are
// eight coins in general circulation:
//
//     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
//
// It is possible to make £2 in the following way:
//
//     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
//
// How many different ways can £2 be made using any number of coins?

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    combinations(200, vec![200, 100, 50, 20, 10, 5, 2, 1])
}

fn combinations(total: usize, rscoins: Vec<usize>) -> usize {
    let mut ccount = 0;

    for ix in 0..(rscoins.len() - 1) {
        let mut amount = total;

        while rscoins[ix] <= amount {
            amount -= rscoins[ix];
            ccount += combinations(amount, rscoins[(ix + 1)..].to_vec());
        }
    }

    if total % rscoins.last().unwrap() == 0 {
        ccount += 1;
    }

    ccount
}
