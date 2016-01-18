// Problem 19 - Counting Sundays
//
// You are given the following information, but you may prefer to do some research
// for yourself.
//
//     - 1 Jan 1900 was a Monday.
//
//     - Thirty days has September,
//       April, June and November.
//       All the rest have thirty-one,
//       Saving February alone,
//       Which has twenty-eight, rain or shine.
//       And on leap years, twenty-nine.
//
//     - A leap year occurs on any year evenly divisible by 4, but not on a
//       century unless it is divisible by 400.
//
// How many Sundays fell on the first of the month during the twentieth century
// (1 Jan 1901 to 31 Dec 2000)?

fn main() {
    println!("{}", solution());
}

fn solution() -> u32 {
    let mut first_sundays = 0;

    // 0 = Sunday, 1 = Monday, ...
    let mut current_first = 1;

    for year in 1900..2001 {
        for month in 1..13 {
            if year > 1900 && current_first == 0 {
                first_sundays += 1;
            }

            current_first += day_count(year, month);
            current_first %= 7;
        }
    }

    first_sundays
}

fn day_count(year: u32, month: u32) -> u32 {
    if [1, 3, 5, 7, 8, 10, 12].contains(&month) {
        return 31;
    } else if [4, 6, 9, 11].contains(&month) {
        return 30;
    }

    if year % 400 == 0 || (year % 100 != 0 && year % 4 == 0) {
        return 29;
    }

    28
}

