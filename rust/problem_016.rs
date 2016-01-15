// Problem 16 - Power digit sum
//
// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
//
// What is the sum of the digits of the number 2^1000?

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    power_of_2(1000).iter().fold(0, |sum, d| sum + d)
}

fn power_of_2(n: u32) -> Vec<u32> {
    let mut result = vec![1];

    for _ in 0..n {
        let mut newres = vec![];
        let mut carry  = 0;

        for digit in result {
            let double = digit*2 + carry;

            newres.push(double%10);
            carry = double/10;
        }

        if carry > 0 {
            newres.push(carry);
        }

        result = newres;
    }

    result
}
