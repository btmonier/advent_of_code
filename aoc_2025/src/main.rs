use std::env;
use aoc_2025::days;

fn main() {
    let args: Vec<String> = env::args().collect();
    let day: u32 = args
        .get(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(1);

    match day {
        1 => days::day_01::run(),
        2 => days::day_02::run(),
        3 => days::day_03::run(),
        4 => days::day_04::run(),
        5 => days::day_05::run(),
        _ => eprintln!("Day {} not implemented yet!", day),
    }
}

