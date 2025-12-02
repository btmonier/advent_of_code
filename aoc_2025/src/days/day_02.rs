use std::collections::HashSet;

use crate::read_input_raw;

pub fn run() {
    let input = read_input_raw("day_02");

    // Part 1: Sum all invalid IDs in the given ranges
    // Invalid IDs are numbers made of a digit sequence repeated exactly twice (e.g., 55, 6464, 123123)

    let ranges: Vec<(u64, u64)> = input
        .split(',')
        .filter(|s| !s.trim().is_empty())
        .map(|range| {
            let parts: Vec<&str> = range.trim().split('-').collect();
            let start: u64 = parts[0].parse().unwrap();
            let end: u64 = parts[1].parse().unwrap();
            (start, end)
        })
        .collect();

    let mut sum: u64 = 0;
    for (start, end) in &ranges {
        sum += sum_invalid_in_range(*start, *end);
    }

    let res01 = sum;

    // Part 2: Invalid IDs are now patterns repeated at least twice (e.g., 111, 1212, 123123123)
    let mut sum2: u64 = 0;
    for (start, end) in &ranges {
        sum2 += sum_invalid_in_range_part2(*start, *end);
    }

    let res02 = sum2;

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Sum all invalid numbers in range [start, end].
///
/// Invalid numbers are those where the digits form a pattern repeated twice:
/// - 11, 22, ..., 99 (single digit repeated)
/// - 1010, 1111, ..., 9999 (two digits repeated)
/// - 100100, 101101, ..., 999999 (three digits repeated)
/// - etc.
///
/// For a k-digit base number n, the invalid number is n * (10^k + 1).
/// For example: base=123, k=3, multiplier=1001, invalid=123123
fn sum_invalid_in_range(start: u64, end: u64) -> u64 {
    let mut sum = 0;

    // Generate invalid numbers by digit length of the "base" number
    // k=1: base 1-9, multiplier=11, invalid numbers 11-99
    // k=2: base 10-99, multiplier=101, invalid numbers 1010-9999
    // k=3: base 100-999, multiplier=1001, invalid numbers 100100-999999
    // etc.
    for k in 1..=10 {
        // Use checked arithmetic to avoid overflow
        let Some(ten_pow_k) = 10u64.checked_pow(k) else {
            break;
        };
        let multiplier = ten_pow_k + 1;
        let base_start = if k == 1 { 1 } else { 10u64.pow(k - 1) };
        let base_end = ten_pow_k - 1;

        // Range of invalid numbers for this k
        let invalid_min = base_start.saturating_mul(multiplier);
        let Some(invalid_max) = base_end.checked_mul(multiplier) else {
            break; // Numbers too large, stop here
        };

        // Skip if no overlap with our target range
        if invalid_max < start || invalid_min > end {
            continue;
        }

        // Find the range of base values that produce invalid numbers within [start, end]
        // For invalid number to be >= start: base * multiplier >= start => base >= start / multiplier (rounded up)
        // For invalid number to be <= end: base * multiplier <= end => base <= end / multiplier
        let actual_base_start = ((start + multiplier - 1) / multiplier).max(base_start);
        let actual_base_end = (end / multiplier).min(base_end);

        if actual_base_start > actual_base_end {
            continue;
        }

        // Sum of invalid numbers = multiplier * sum of bases from actual_base_start to actual_base_end
        // Sum of consecutive integers from a to b = (b - a + 1) * (a + b) / 2
        let count = actual_base_end - actual_base_start + 1;
        let base_sum = count * (actual_base_start + actual_base_end) / 2;
        sum += multiplier * base_sum;
    }

    sum
}

/// Sum all invalid numbers in range [start, end] for Part 2.
///
/// Invalid numbers are those where the digits form a pattern repeated at least twice:
/// - 11, 111, 1111, ... (1 repeated 2+, 3+, 4+ times)
/// - 1212, 121212, ... (12 repeated 2+, 3+ times)
/// - etc.
///
/// For a k-digit base number n repeated r times, the invalid number is:
/// n * (10^(k*r) - 1) / (10^k - 1)
///
/// We use a HashSet to avoid counting duplicates (e.g., 1111 = "1"×4 = "11"×2)
fn sum_invalid_in_range_part2(start: u64, end: u64) -> u64 {
    let mut invalid_set: HashSet<u64> = HashSet::new();

    // For each pattern length k and repetition count r (r >= 2)
    for k in 1u32..=10 {
        for r in 2u32..=20 {
            let total_digits = k * r;
            if total_digits > 19 {
                break; // u64 max is ~19 digits
            }

            // multiplier = (10^(k*r) - 1) / (10^k - 1)
            // e.g., k=1, r=3: (1000-1)/(10-1) = 999/9 = 111
            // e.g., k=2, r=3: (1000000-1)/(100-1) = 999999/99 = 10101
            let Some(ten_pow_kr) = 10u64.checked_pow(total_digits) else {
                break;
            };
            let ten_pow_k = 10u64.pow(k);
            let multiplier = (ten_pow_kr - 1) / (ten_pow_k - 1);

            // Base range for k-digit numbers (no leading zeros)
            let base_start = if k == 1 { 1 } else { 10u64.pow(k - 1) };
            let base_end = ten_pow_k - 1;

            // Range of invalid numbers for this (k, r)
            let invalid_min = base_start.saturating_mul(multiplier);
            let Some(invalid_max) = base_end.checked_mul(multiplier) else {
                break;
            };

            // Skip if no overlap with target range
            if invalid_max < start || invalid_min > end {
                continue;
            }

            // Find bases that produce invalid numbers in [start, end]
            let actual_base_start = ((start + multiplier - 1) / multiplier).max(base_start);
            let actual_base_end = (end / multiplier).min(base_end);

            if actual_base_start > actual_base_end {
                continue;
            }

            // Add all invalid numbers from this (k, r) to the set
            for base in actual_base_start..=actual_base_end {
                invalid_set.insert(base * multiplier);
            }
        }
    }

    invalid_set.iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_invalid_pattern() {
        // Test by checking sum_invalid_in_range for single numbers
        assert_eq!(sum_invalid_in_range(11, 11), 11); // 11 is invalid
        assert_eq!(sum_invalid_in_range(12, 12), 0); // 12 is valid
        assert_eq!(sum_invalid_in_range(99, 99), 99); // 99 is invalid
        assert_eq!(sum_invalid_in_range(100, 100), 0); // 100 is valid (odd digits)
        assert_eq!(sum_invalid_in_range(1010, 1010), 1010); // 1010 is invalid
        assert_eq!(sum_invalid_in_range(1234, 1234), 0); // 1234 is valid
        assert_eq!(sum_invalid_in_range(123123, 123123), 123123); // 123123 is invalid
    }

    #[test]
    fn test_example_ranges() {
        // From the problem description
        assert_eq!(sum_invalid_in_range(11, 22), 11 + 22); // 11 and 22
        assert_eq!(sum_invalid_in_range(95, 115), 99); // only 99
        assert_eq!(sum_invalid_in_range(998, 1012), 1010); // only 1010
        assert_eq!(sum_invalid_in_range(1188511880, 1188511890), 1188511885);
        assert_eq!(sum_invalid_in_range(222220, 222224), 222222);
        assert_eq!(sum_invalid_in_range(1698522, 1698528), 0); // no invalid
        assert_eq!(sum_invalid_in_range(446443, 446449), 446446);
        assert_eq!(sum_invalid_in_range(38593856, 38593862), 38593859);
        assert_eq!(sum_invalid_in_range(824824821, 824824827), 0); // no invalid
        assert_eq!(sum_invalid_in_range(2121212118, 2121212124), 0); // no invalid
    }

    #[test]
    fn test_full_example() {
        // Total from all example ranges should be 1227775554
        let total = sum_invalid_in_range(11, 22)
            + sum_invalid_in_range(95, 115)
            + sum_invalid_in_range(998, 1012)
            + sum_invalid_in_range(1188511880, 1188511890)
            + sum_invalid_in_range(222220, 222224)
            + sum_invalid_in_range(1698522, 1698528)
            + sum_invalid_in_range(446443, 446449)
            + sum_invalid_in_range(38593856, 38593862)
            + sum_invalid_in_range(824824821, 824824827)
            + sum_invalid_in_range(2121212118, 2121212124);

        assert_eq!(total, 1227775554);
    }

    // Part 2 tests

    #[test]
    fn test_part2_is_invalid_pattern() {
        // Test Part 2 patterns (at least 2 repetitions)
        assert_eq!(sum_invalid_in_range_part2(11, 11), 11); // "1" × 2
        assert_eq!(sum_invalid_in_range_part2(111, 111), 111); // "1" × 3
        assert_eq!(sum_invalid_in_range_part2(1111, 1111), 1111); // "1" × 4 or "11" × 2
        assert_eq!(sum_invalid_in_range_part2(1212, 1212), 1212); // "12" × 2
        assert_eq!(sum_invalid_in_range_part2(121212, 121212), 121212); // "12" × 3
        assert_eq!(sum_invalid_in_range_part2(123, 123), 0); // not a pattern
        assert_eq!(sum_invalid_in_range_part2(1234, 1234), 0); // not a pattern
    }

    #[test]
    fn test_part2_example_ranges() {
        // From the Part 2 problem description
        assert_eq!(sum_invalid_in_range_part2(11, 22), 11 + 22); // 11 and 22
        assert_eq!(sum_invalid_in_range_part2(95, 115), 99 + 111); // 99 and 111
        assert_eq!(sum_invalid_in_range_part2(998, 1012), 999 + 1010); // 999 and 1010
        assert_eq!(sum_invalid_in_range_part2(1188511880, 1188511890), 1188511885);
        assert_eq!(sum_invalid_in_range_part2(222220, 222224), 222222);
        assert_eq!(sum_invalid_in_range_part2(1698522, 1698528), 0);
        assert_eq!(sum_invalid_in_range_part2(446443, 446449), 446446);
        assert_eq!(sum_invalid_in_range_part2(38593856, 38593862), 38593859);
        assert_eq!(sum_invalid_in_range_part2(565653, 565659), 565656); // "56" × 3
        assert_eq!(sum_invalid_in_range_part2(824824821, 824824827), 824824824); // "824" × 3
        assert_eq!(sum_invalid_in_range_part2(2121212118, 2121212124), 2121212121); // "21" × 5
    }

    #[test]
    fn test_part2_full_example() {
        // Total from all example ranges should be 4174379265
        let total = sum_invalid_in_range_part2(11, 22)
            + sum_invalid_in_range_part2(95, 115)
            + sum_invalid_in_range_part2(998, 1012)
            + sum_invalid_in_range_part2(1188511880, 1188511890)
            + sum_invalid_in_range_part2(222220, 222224)
            + sum_invalid_in_range_part2(1698522, 1698528)
            + sum_invalid_in_range_part2(446443, 446449)
            + sum_invalid_in_range_part2(38593856, 38593862)
            + sum_invalid_in_range_part2(565653, 565659)
            + sum_invalid_in_range_part2(824824821, 824824827)
            + sum_invalid_in_range_part2(2121212118, 2121212124);

        assert_eq!(total, 4174379265);
    }
}
