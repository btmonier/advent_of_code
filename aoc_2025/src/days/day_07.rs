use std::collections::{HashMap, HashSet};

use crate::read_input;

pub fn run() {
    let input = read_input("day_07");
    let grid = parse_grid(&input);

    let res01 = count_splits(&grid);
    let res02 = count_timelines(&grid);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Parse input lines into a 2D character grid.
fn parse_grid(lines: &[String]) -> Vec<Vec<char>> {
    lines.iter().map(|line| line.chars().collect()).collect()
}

/// Count the number of times tachyon beams are split.
/// Beams move downward from S, and split at ^ characters into left and right beams.
fn count_splits(grid: &[Vec<char>]) -> usize {
    if grid.is_empty() {
        return 0;
    }

    // Find the starting position S
    let mut start_col: Option<usize> = None;
    let mut start_row: usize = 0;

    for (row_idx, row) in grid.iter().enumerate() {
        for (col_idx, &ch) in row.iter().enumerate() {
            if ch == 'S' {
                start_col = Some(col_idx);
                start_row = row_idx;
                break;
            }
        }
        if start_col.is_some() {
            break;
        }
    }

    let start_col = start_col.expect("No starting position S found");
    let width = grid.iter().map(|r| r.len()).max().unwrap_or(0);
    let height = grid.len();

    // Track beam positions as a set (beams at same position merge)
    let mut beam_positions: HashSet<usize> = HashSet::new();
    beam_positions.insert(start_col);

    let mut total_splits = 0;

    // Move beams down row by row, starting from the row after S
    for row_idx in (start_row + 1)..height {
        let row = &grid[row_idx];

        let mut new_positions: HashSet<usize> = HashSet::new();

        for &col in &beam_positions {
            // Get the character at this position (treat out-of-bounds as empty)
            let ch = row.get(col).copied().unwrap_or('.');

            if ch == '^' {
                // Beam hits a splitter: count the split, emit left and right beams
                total_splits += 1;

                // Left beam (if in bounds)
                if col > 0 {
                    new_positions.insert(col - 1);
                }
                // Right beam (if in bounds)
                if col + 1 < width {
                    new_positions.insert(col + 1);
                }
            } else {
                // Beam continues straight down
                new_positions.insert(col);
            }
        }

        beam_positions = new_positions;

        // If no beams left, we're done
        if beam_positions.is_empty() {
            break;
        }
    }

    total_splits
}

/// Count the number of timelines after a single particle completes its journey.
/// Each splitter creates a timeline split - one where the particle went left, one where it went right.
/// Unlike Part 1, timelines don't merge even if they're at the same position.
fn count_timelines(grid: &[Vec<char>]) -> u64 {
    if grid.is_empty() {
        return 0;
    }

    // Find the starting position S
    let mut start_col: Option<usize> = None;
    let mut start_row: usize = 0;

    for (row_idx, row) in grid.iter().enumerate() {
        for (col_idx, &ch) in row.iter().enumerate() {
            if ch == 'S' {
                start_col = Some(col_idx);
                start_row = row_idx;
                break;
            }
        }
        if start_col.is_some() {
            break;
        }
    }

    let start_col = start_col.expect("No starting position S found");
    let width = grid.iter().map(|r| r.len()).max().unwrap_or(0);
    let height = grid.len();

    // Track timeline counts at each position: position -> number of timelines
    let mut timeline_counts: HashMap<usize, u64> = HashMap::new();
    timeline_counts.insert(start_col, 1);

    // Move down row by row, starting from the row after S
    for row_idx in (start_row + 1)..height {
        let row = &grid[row_idx];

        let mut new_counts: HashMap<usize, u64> = HashMap::new();

        for (&col, &count) in &timeline_counts {
            // Get the character at this position
            let ch = row.get(col).copied().unwrap_or('.');

            if ch == '^' {
                // Particle hits a splitter: timeline splits into left and right
                // Left timeline (if in bounds)
                if col > 0 {
                    *new_counts.entry(col - 1).or_insert(0) += count;
                }
                // Right timeline (if in bounds)
                if col + 1 < width {
                    *new_counts.entry(col + 1).or_insert(0) += count;
                }
            } else {
                // Particle continues straight down
                *new_counts.entry(col).or_insert(0) += count;
            }
        }

        timeline_counts = new_counts;

        // If no timelines left, we're done
        if timeline_counts.is_empty() {
            break;
        }
    }

    // Sum all timeline counts
    timeline_counts.values().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_test_input(input: &str) -> Vec<Vec<char>> {
        let lines: Vec<String> = input
            .lines()
            .filter(|line| !line.is_empty())
            .map(String::from)
            .collect();
        parse_grid(&lines)
    }

    #[test]
    fn test_example_part1() {
        let input = "\
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............";

        let grid = parse_test_input(input);
        assert_eq!(count_splits(&grid), 21);
    }

    #[test]
    fn test_example_part2() {
        let input = "\
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............";

        let grid = parse_test_input(input);
        assert_eq!(count_timelines(&grid), 40);
    }
}
