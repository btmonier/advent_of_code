use crate::read_input;

pub fn run() {
    let input = read_input("day_04");

    // Part 1: Count rolls that can be accessed (fewer than 4 adjacent rolls)
    let grid: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
    let res01 = count_accessible_rolls(&grid);

    // Part 2: Remove rolls iteratively until no more can be removed
    let res02 = count_total_removable(&grid);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Count the total number of paper rolls that can be removed by iteratively
/// removing accessible rolls until no more can be removed.
fn count_total_removable(grid: &[Vec<char>]) -> usize {
    let mut grid = grid.to_vec();
    let rows = grid.len();
    let cols = grid[0].len();
    let mut total_removed = 0;

    loop {
        // Find all currently accessible rolls
        let mut to_remove = Vec::new();
        for r in 0..rows {
            for c in 0..cols {
                if grid[r][c] == '@' {
                    let adjacent = count_adjacent_rolls(&grid, r, c);
                    if adjacent < 4 {
                        to_remove.push((r, c));
                    }
                }
            }
        }

        // If no rolls can be removed, we're done
        if to_remove.is_empty() {
            break;
        }

        // Remove all accessible rolls
        for (r, c) in &to_remove {
            grid[*r][*c] = '.';
        }
        total_removed += to_remove.len();
    }

    total_removed
}

/// Count the number of paper rolls that can be accessed by a forklift.
/// A roll is accessible if it has fewer than 4 adjacent rolls (in 8 directions).
fn count_accessible_rolls(grid: &[Vec<char>]) -> usize {
    let rows = grid.len();
    let cols = grid[0].len();

    let mut count = 0;

    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == '@' {
                let adjacent = count_adjacent_rolls(grid, r, c);
                if adjacent < 4 {
                    count += 1;
                }
            }
        }
    }

    count
}

/// Count the number of adjacent paper rolls (8 directions) for a given position.
fn count_adjacent_rolls(grid: &[Vec<char>], row: usize, col: usize) -> usize {
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;
    let r = row as i32;
    let c = col as i32;

    let directions = [
        (-1, -1), (-1, 0), (-1, 1),
        ( 0, -1),          ( 0, 1),
        ( 1, -1), ( 1, 0), ( 1, 1),
    ];

    let mut count = 0;
    for (dr, dc) in directions {
        let nr = r + dr;
        let nc = c + dc;
        if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
            if grid[nr as usize][nc as usize] == '@' {
                count += 1;
            }
        }
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example() {
        let input = vec![
            "..@@.@@@@.",
            "@@@.@.@.@@",
            "@@@@@.@.@@",
            "@.@@@@..@.",
            "@@.@@@@.@@",
            ".@@@@@@@.@",
            ".@.@.@.@@@",
            "@.@@@.@@@@",
            ".@@@@@@@@.",
            "@.@.@@@.@.",
        ];
        let grid: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
        assert_eq!(count_accessible_rolls(&grid), 13);
    }

    #[test]
    fn test_part2_example() {
        let input = vec![
            "..@@.@@@@.",
            "@@@.@.@.@@",
            "@@@@@.@.@@",
            "@.@@@@..@.",
            "@@.@@@@.@@",
            ".@@@@@@@.@",
            ".@.@.@.@@@",
            "@.@@@.@@@@",
            ".@@@@@@@@.",
            "@.@.@@@.@.",
        ];
        let grid: Vec<Vec<char>> = input.iter().map(|line| line.chars().collect()).collect();
        assert_eq!(count_total_removable(&grid), 43);
    }
}

