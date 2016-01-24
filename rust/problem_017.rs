// Problem 17 - Number letter counts
//
// If the numbers 1 to 5 are written out in words: one, two, three, four, five,
// then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
//
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
// words, how many letters would be used?
//
// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
// forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
// letters. The use of "and" when writing out numbers is in compliance with
// British usage.

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let mut letter_count = 0;

    for n in 1..1001 {
        for c in in_words(n).chars() {
            letter_count += match c {
                'a'...'z' => 1,
                _         => 0,
            };
        }
    }

    letter_count
}

fn in_words(n: u32) -> String {
    match n {
        a @ 0...20 => {
            match a {
                0  => "",
                1  => "one",
                2  => "two",
                3  => "three",
                4  => "four",
                5  => "five",
                6  => "six",
                7  => "seven",
                8  => "eight",
                9  => "nine",
                10 => "ten",
                11 => "eleven",
                12 => "twelve",
                13 => "thirteen",
                14 => "fourteen",
                15 => "fifteen",
                16 => "sixteen",
                17 => "seventeen",
                18 => "eighteen",
                19 => "nineteen",
                20 => "twenty",
                _  => unreachable!(),
            }.to_string()
        },

        b if n < 100 && n%10 == 0 => {
            match b {
                30 => "thirty",
                40 => "forty",
                50 => "fifty",
                60 => "sixty",
                70 => "seventy",
                80 => "eighty",
                90 => "ninety",
                _  => unreachable!(),
            }.to_string()
        },

        _ if n < 100 => {
            in_words(n - n%10) + "-" + &in_words(n%10)
        },

        _ if n < 1000 && n%100 == 0 => {
            in_words(n/100) + " hundred"
        },

        _ if n < 1000 => {
            in_words(n - n%100) + " and " + &in_words(n%100)
        },

        _ if n%1000 == 0 => {
            in_words(n/1000) + " thousand"
        },

        _ => {
            in_words(n/1000) + " thousand and " + &in_words(n%1000)
        },
    }
}
