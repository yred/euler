// Problem 42 - Coded triangle numbers
//
// The n-th term of the sequence of triangle numbers is given by, t(n) = ½n(n+1);
// so the first ten triangle numbers are:
//
//         1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
//
// By converting each letter in a word to a number corresponding to its
// alphabetical position and adding these values we form a word value. For
// example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value
// is a triangle number then we shall call the word a triangle word.
//
// Using "../resources/p042_words.txt", a 16K text file containing nearly
// two-thousand common English words, how many are triangle words?
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    println!("{}", solution());
}

fn solution() -> usize {
    let file = match File::open("../resources/p042_words.txt") {
        Ok(f)  => BufReader::new(f),
        Err(_) => panic!("Error opening file"),
    };

    let words = file.split(b',')
                    .map(Result::unwrap)
                    .map(unquote)
                    .collect::<Vec<_>>();

    let maxlen = words.iter().map(String::len).max().unwrap();

    let triangles = (1..).map(triangle)
                         .take_while(|&t| t < maxlen*26)
                         .collect::<HashSet<usize>>();

    words.iter()
         .map(numerify)
         .filter(|n| triangles.contains(&n))
         .count()
}

fn unquote(s: Vec<u8>) -> String {
    String::from_utf8(s).unwrap().trim_matches('"').to_string()
}

fn triangle(n: usize) -> usize {
    n*(n + 1)/2
}

fn numerify(s: &String) -> usize {
    s.chars().map(|c| (c as usize) - ('A' as usize) + 1).fold(0, |s, n| s + n)
}
