// Problem 22 - Names scores
//
// Using "../resources/p022_names.txt", a 46K text file containing over
// five-thousand first names, begin by sorting it into alphabetical order.
// Then working out the alphabetical value for each name, multiply this value by
// its alphabetical position in the list to obtain a name score.
//
// For example, when the list is sorted into alphabetical order, COLIN, which is
// worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
// obtain a score of 938 × 53 = 49714.
//
// What is the total of all the name scores in the file?
use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let file = match File::open("../resources/p022_names.txt") {
        Ok(f)  => BufReader::new(f),
        Err(_) => panic!("Error opening file"),
    };

    let mut names = file.split(b',')
                        .map(|res| get_name(res.unwrap()))
                        .collect::<Vec<_>>();

    names.sort();

    names.iter()
         .enumerate()
         .map(|(ix, name)| (ix as u32 + 1)*value(name))
         .fold(0, |sum, score| sum + score)
}

fn get_name(vstr: Vec<u8>) -> String {
    String::from_utf8(vstr).unwrap().trim_matches('"').to_string()
}

fn value(s: &str) -> u32 {
    let zero = b'A' - 1;
    s.bytes().fold(0, |sum, c| { sum + (c - zero) as u32 })
}
