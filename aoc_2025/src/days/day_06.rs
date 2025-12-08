use std::fs;
use std::path::Path;

pub fn run() {
    // Read input without trimming to preserve column positions
    let path = "../input/2025/day_06.txt";
    let content = fs::read_to_string(Path::new(&path))
        .expect(&format!("Could not read file: {}", path));
    
    // Split into lines, keeping each line's content (but removing trailing newlines)
    let input: Vec<String> = content
        .lines()
        .filter(|line| !line.is_empty())
        .map(String::from)
        .collect();

    // Part 1: Solve the worksheet and sum all answers (horizontal reading)
    let res01 = solve_worksheet(&input);

    // Part 2: Solve with vertical reading (each column is a number)
    let res02 = solve_worksheet_vertical(&input);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Parse the worksheet and compute the grand total of all problems.
fn solve_worksheet(input: &[String]) -> u64 {
    if input.is_empty() {
        return 0;
    }

    // The last non-empty line contains the operators
    let operator_line = input.last().unwrap();
    let number_lines: Vec<&String> = input.iter().take(input.len() - 1).collect();

    // Find the maximum width of all lines
    let max_width = input.iter().map(|l| l.len()).max().unwrap_or(0);

    // Pad all lines to the same width for easier processing
    let padded_number_lines: Vec<String> = number_lines
        .iter()
        .map(|l| format!("{:<width$}", l, width = max_width))
        .collect();

    let padded_operator_line = format!("{:<width$}", operator_line, width = max_width);

    // Combine all lines for separator detection (problems are separated by columns 
    // that are spaces in ALL rows, including the operator row)
    let mut all_lines: Vec<String> = padded_number_lines.clone();
    all_lines.push(padded_operator_line.clone());

    // Find column groups - problems are separated by columns that are all spaces
    let mut problems: Vec<(Vec<u64>, char)> = Vec::new();
    let mut col = 0;

    while col < max_width {
        // Skip separator columns (all spaces in ALL rows including operator)
        while col < max_width && is_separator_column(&all_lines, col) {
            col += 1;
        }

        if col >= max_width {
            break;
        }

        // Find the end of this problem (next separator or end of line)
        let start_col = col;
        while col < max_width && !is_separator_column(&all_lines, col) {
            col += 1;
        }
        let end_col = col;

        // Extract numbers from this column range
        let numbers = extract_numbers(&padded_number_lines, start_col, end_col);

        // Only process if we found numbers
        if !numbers.is_empty() {
            // Extract operator from this column range
            let operator = extract_operator(&padded_operator_line, start_col, end_col);
            problems.push((numbers, operator));
        }
    }

    // Compute results and sum
    let mut total: u64 = 0;
    for (numbers, operator) in problems {
        let result: u64 = match operator {
            '+' => numbers.iter().sum(),
            '*' => numbers.iter().product(),
            _ => panic!("Unknown operator: {}", operator),
        };
        total += result;
    }

    total
}

/// Parse the worksheet with vertical reading (Part 2).
/// Each column within a problem is a separate number, read top-to-bottom.
fn solve_worksheet_vertical(input: &[String]) -> u64 {
    if input.is_empty() {
        return 0;
    }

    // The last non-empty line contains the operators
    let operator_line = input.last().unwrap();
    let number_lines: Vec<&String> = input.iter().take(input.len() - 1).collect();

    // Find the maximum width of all lines
    let max_width = input.iter().map(|l| l.len()).max().unwrap_or(0);

    // Pad all lines to the same width for easier processing
    let padded_number_lines: Vec<String> = number_lines
        .iter()
        .map(|l| format!("{:<width$}", l, width = max_width))
        .collect();

    let padded_operator_line = format!("{:<width$}", operator_line, width = max_width);

    // Combine all lines for separator detection
    let mut all_lines: Vec<String> = padded_number_lines.clone();
    all_lines.push(padded_operator_line.clone());

    // Find column groups - problems are separated by columns that are all spaces
    let mut problems: Vec<(Vec<u64>, char)> = Vec::new();
    let mut col = 0;

    while col < max_width {
        // Skip separator columns
        while col < max_width && is_separator_column(&all_lines, col) {
            col += 1;
        }

        if col >= max_width {
            break;
        }

        // Find the end of this problem (next separator or end of line)
        let start_col = col;
        while col < max_width && !is_separator_column(&all_lines, col) {
            col += 1;
        }
        let end_col = col;

        // Extract numbers vertically - each column is a separate number
        let numbers = extract_numbers_vertical(&padded_number_lines, start_col, end_col);

        // Only process if we found numbers
        if !numbers.is_empty() {
            let operator = extract_operator(&padded_operator_line, start_col, end_col);
            problems.push((numbers, operator));
        }
    }

    // Compute results and sum
    let mut total: u64 = 0;
    for (numbers, operator) in problems {
        let result: u64 = match operator {
            '+' => numbers.iter().sum(),
            '*' => numbers.iter().product(),
            _ => panic!("Unknown operator: {}", operator),
        };
        total += result;
    }

    total
}

/// Extract numbers vertically - each column in the range is a separate number.
/// Digits are read top-to-bottom, with the topmost being most significant.
fn extract_numbers_vertical(lines: &[String], start_col: usize, end_col: usize) -> Vec<u64> {
    let mut numbers = Vec::new();

    // Process each column individually
    for col in start_col..end_col {
        let mut num_str = String::new();
        
        // Read digits from top to bottom
        for line in lines {
            let ch = line.chars().nth(col).unwrap_or(' ');
            if ch.is_ascii_digit() {
                num_str.push(ch);
            }
        }

        // If we found any digits in this column, it's a number
        if !num_str.is_empty() {
            if let Ok(num) = num_str.parse::<u64>() {
                numbers.push(num);
            }
        }
    }

    numbers
}

/// Check if a column is a separator (all spaces in all lines).
/// A column is a separator if every line at that position is a space.
fn is_separator_column(lines: &[String], col: usize) -> bool {
    for line in lines {
        if col < line.len() {
            let ch = line.chars().nth(col).unwrap_or(' ');
            if ch != ' ' {
                return false;
            }
        }
    }
    true
}

/// Extract all numbers from the given column range across all number lines.
fn extract_numbers(lines: &[String], start_col: usize, end_col: usize) -> Vec<u64> {
    let mut numbers = Vec::new();

    for line in lines {
        let segment: String = line
            .chars()
            .skip(start_col)
            .take(end_col - start_col)
            .collect();

        // Parse number from segment (ignore spaces)
        let num_str: String = segment.chars().filter(|c| c.is_ascii_digit()).collect();
        if !num_str.is_empty() {
            if let Ok(num) = num_str.parse::<u64>() {
                numbers.push(num);
            }
        }
    }

    numbers
}

/// Extract the operator from the given column range in the operator line.
fn extract_operator(operator_line: &str, start_col: usize, end_col: usize) -> char {
    let segment: String = operator_line
        .chars()
        .skip(start_col)
        .take(end_col - start_col)
        .collect();

    // Find the operator character (+ or *)
    for ch in segment.chars() {
        if ch == '+' || ch == '*' {
            return ch;
        }
    }

    panic!(
        "No operator found in segment '{}' (cols {}-{})",
        segment, start_col, end_col
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_part1() {
        let input: Vec<String> = vec![
            "123 328  51 64 ",
            " 45 64  387 23 ",
            "  6 98  215 314",
            "*   +   *   +  ",
        ]
        .into_iter()
        .map(String::from)
        .collect();

        // 123 * 45 * 6 = 33210
        // 328 + 64 + 98 = 490
        // 51 * 387 * 215 = 4243455
        // 64 + 23 + 314 = 401
        // Total = 33210 + 490 + 4243455 + 401 = 4277556
        assert_eq!(solve_worksheet(&input), 4277556);
    }

    #[test]
    fn test_example_part2() {
        let input: Vec<String> = vec![
            "123 328  51 64 ",
            " 45 64  387 23 ",
            "  6 98  215 314",
            "*   +   *   +  ",
        ]
        .into_iter()
        .map(String::from)
        .collect();

        // Reading columns vertically:
        // Rightmost problem: 4 + 431 + 623 = 1058
        // Second from right: 175 * 581 * 32 = 3253600
        // Third from right: 8 + 248 + 369 = 625
        // Leftmost: 356 * 24 * 1 = 8544
        // Total = 1058 + 3253600 + 625 + 8544 = 3263827
        assert_eq!(solve_worksheet_vertical(&input), 3263827);
    }
}
