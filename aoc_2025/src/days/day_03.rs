use crate::read_input;

pub fn run() {
    let input = read_input("day_03");

    // Part 1: Find the maximum two-digit joltage from each bank and sum them
    let res01: u32 = input.iter().map(|bank| max_joltage(bank)).sum();

    // Part 2: Find the maximum 12-digit joltage from each bank and sum them
    let res02: u64 = input.iter().map(|bank| max_joltage_k(bank, 12)).sum();

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Find the maximum joltage (two-digit number) that can be formed by
/// selecting exactly two batteries from the bank, maintaining their order.
fn max_joltage(bank: &str) -> u32 {
    let digits: Vec<u32> = bank.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let n = digits.len();

    let mut max_jolt = 0;

    // For each possible first battery position
    for i in 0..n - 1 {
        let first = digits[i];
        // Find the maximum digit after position i
        let second = *digits[i + 1..].iter().max().unwrap();
        let joltage = first * 10 + second;
        max_jolt = max_jolt.max(joltage);
    }

    max_jolt
}

/// Find the maximum joltage (k-digit number) that can be formed by
/// selecting exactly k batteries from the bank, maintaining their order.
///
/// Uses a greedy approach: at each step, pick the largest digit from the
/// valid range (ensuring enough digits remain for the remaining positions).
fn max_joltage_k(bank: &str, k: usize) -> u64 {
    use std::cmp::Reverse;

    let digits: Vec<u32> = bank.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let n = digits.len();

    let mut result: u64 = 0;
    let mut start = 0;

    for remaining in (1..=k).rev() {
        // We can pick from start to (n - remaining) inclusive
        // This ensures we have enough positions left for remaining-1 more picks
        let end = n - remaining;

        // Find the FIRST position with the maximum digit in [start, end]
        // Using min_by_key with Reverse to get max digit, then smallest index on ties
        let (best_pos, best_digit) = (start..=end)
            .map(|i| (i, digits[i]))
            .min_by_key(|&(i, d)| (Reverse(d), i))
            .unwrap();

        result = result * 10 + best_digit as u64;
        start = best_pos + 1;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_max_joltage() {
        assert_eq!(max_joltage("987654321111111"), 98);
        assert_eq!(max_joltage("811111111111119"), 89);
        assert_eq!(max_joltage("234234234234278"), 78);
        assert_eq!(max_joltage("818181911112111"), 92);
    }

    #[test]
    fn test_example_sum() {
        let banks = vec![
            "987654321111111",
            "811111111111119",
            "234234234234278",
            "818181911112111",
        ];
        let total: u32 = banks.iter().map(|b| max_joltage(b)).sum();
        assert_eq!(total, 357);
    }

    // Part 2 tests

    #[test]
    fn test_max_joltage_k() {
        assert_eq!(max_joltage_k("987654321111111", 12), 987654321111);
        assert_eq!(max_joltage_k("811111111111119", 12), 811111111119);
        assert_eq!(max_joltage_k("234234234234278", 12), 434234234278);
        assert_eq!(max_joltage_k("818181911112111", 12), 888911112111);
    }

    #[test]
    fn test_part2_example_sum() {
        let banks = vec![
            "987654321111111",
            "811111111111119",
            "234234234234278",
            "818181911112111",
        ];
        let total: u64 = banks.iter().map(|b| max_joltage_k(b, 12)).sum();
        assert_eq!(total, 3121910778619);
    }
}

