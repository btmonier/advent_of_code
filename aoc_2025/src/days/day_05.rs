use crate::read_input;

pub fn run() {
    let input = read_input("day_05");

    let (ranges, ingredients) = parse_input(&input);

    // Part 1: Count fresh ingredients
    let res01 = count_fresh(&ranges, &ingredients);

    // Part 2: Count total unique IDs considered fresh by all ranges
    let res02 = count_total_fresh_ids(&ranges);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Parse the input into ranges and ingredient IDs.
/// The input consists of ranges (start-end) until a blank line,
/// then individual ingredient IDs.
fn parse_input(input: &[String]) -> (Vec<(u64, u64)>, Vec<u64>) {
    let mut ranges: Vec<(u64, u64)> = Vec::new();
    let mut ingredients: Vec<u64> = Vec::new();
    let mut parsing_ranges = true;

    for line in input {
        if line.is_empty() {
            parsing_ranges = false;
            continue;
        }

        if parsing_ranges {
            // Parse range like "3-5"
            let parts: Vec<&str> = line.split('-').collect();
            let start: u64 = parts[0].parse().unwrap();
            let end: u64 = parts[1].parse().unwrap();
            ranges.push((start, end));
        } else {
            // Parse ingredient ID
            let id: u64 = line.parse().unwrap();
            ingredients.push(id);
        }
    }

    (ranges, ingredients)
}

/// Check if an ingredient ID is fresh (falls within any range).
fn is_fresh(id: u64, ranges: &[(u64, u64)]) -> bool {
    for &(start, end) in ranges {
        if id >= start && id <= end {
            return true;
        }
    }
    false
}

/// Count how many ingredient IDs are fresh.
fn count_fresh(ranges: &[(u64, u64)], ingredients: &[u64]) -> usize {
    ingredients
        .iter()
        .filter(|&&id| is_fresh(id, ranges))
        .count()
}

/// Count the total number of unique IDs considered fresh by merging overlapping ranges.
fn count_total_fresh_ids(ranges: &[(u64, u64)]) -> u64 {
    if ranges.is_empty() {
        return 0;
    }

    // Sort ranges by start value
    let mut sorted_ranges: Vec<(u64, u64)> = ranges.to_vec();
    sorted_ranges.sort_by_key(|r| r.0);

    // Merge overlapping ranges
    let mut merged: Vec<(u64, u64)> = Vec::new();
    merged.push(sorted_ranges[0]);

    for &(start, end) in &sorted_ranges[1..] {
        let last = merged.last_mut().unwrap();
        if start <= last.1 + 1 {
            // Ranges overlap or are adjacent, merge them
            last.1 = last.1.max(end);
        } else {
            // No overlap, add new range
            merged.push((start, end));
        }
    }

    // Count total IDs in merged ranges
    merged.iter().map(|(start, end)| end - start + 1).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example() {
        let input: Vec<String> = vec![
            "3-5",
            "10-14",
            "16-20",
            "12-18",
            "",
            "1",
            "5",
            "8",
            "11",
            "17",
            "32",
        ]
        .into_iter()
        .map(String::from)
        .collect();

        let (ranges, ingredients) = parse_input(&input);
        assert_eq!(count_fresh(&ranges, &ingredients), 3);
    }

    #[test]
    fn test_is_fresh() {
        let ranges = vec![(3, 5), (10, 14), (16, 20), (12, 18)];

        assert!(!is_fresh(1, &ranges)); // spoiled
        assert!(is_fresh(5, &ranges)); // fresh (3-5)
        assert!(!is_fresh(8, &ranges)); // spoiled
        assert!(is_fresh(11, &ranges)); // fresh (10-14)
        assert!(is_fresh(17, &ranges)); // fresh (16-20 and 12-18)
        assert!(!is_fresh(32, &ranges)); // spoiled
    }

    #[test]
    fn test_count_total_fresh_ids() {
        // Example: ranges 3-5, 10-14, 16-20, 12-18
        // Fresh IDs: 3, 4, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 = 14 total
        let ranges = vec![(3, 5), (10, 14), (16, 20), (12, 18)];
        assert_eq!(count_total_fresh_ids(&ranges), 14);
    }
}

