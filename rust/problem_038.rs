// Problem 38 - Pandigital multiples
//
// Take the number 192 and multiply it by each of 1, 2, and 3:
//
//     192 × 1 = 192
//     192 × 2 = 384
//     192 × 3 = 576
//
// By concatenating each product we get the 1 to 9 pandigital, 192384576. We will
// call 192384576 the concatenated product of 192 and (1,2,3)
//
// The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and
// 5, giving the pandigital, 918273645, which is the concatenated product of 9 and
// (1,2,3,4,5).
//
// What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
// concatenated product of an integer with (1,2, ... , n) where n > 1?

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let mut largest = 0.to_string();

    for n in 1..10_000 {
        let mut nstr = n.to_string();

        for k in 2..10 {
            nstr.push_str(&(n*k).to_string());

            if nstr.len() >= 9 {
                break;
            }
        }

        if nstr.len() == 9 && nstr > largest && is_pandigital(nstr.clone()) {
            largest = nstr;
        }
    }

    largest.parse().unwrap()
}

fn is_pandigital(s: String) -> bool {
    let mut chars = s.chars().collect::<Vec<char>>();
    chars.sort();

    let sorted = chars.iter().cloned().collect::<String>();
    sorted == "123456789"
}
