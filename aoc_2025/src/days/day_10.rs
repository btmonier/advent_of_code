use crate::read_input;
use std::collections::{HashSet, VecDeque};

pub fn run() {
    let input = read_input("day_10");

    let res01 = p1(&input);
    let res02 = p2(&input);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

const EPSILON: f64 = 1e-9;

/// Represents a machine with lights, buttons, and joltage requirements.
#[derive(Debug)]
struct Machine {
    lights: usize,
    buttons: Vec<Vec<usize>>,
    joltages: Vec<usize>,
}

impl From<&str> for Machine {
    fn from(value: &str) -> Self {
        let mut parts = value.split_whitespace();

        // Parse the lights pattern [.##.] into a bitmask
        let lights = parts
            .next()
            .map(|l| {
                // We rev here to make calculating below easier.
                l.trim_matches(['[', ']'])
                    .chars()
                    .rev()
                    .fold(0, |acc, c| (acc << 1) | if c == '#' { 1 } else { 0 })
            })
            .unwrap();

        let mut parts: Vec<&str> = parts.collect();

        // Parse joltage requirements {3,5,4,7}
        let joltages = parts
            .pop()
            .unwrap()
            .trim_matches(['{', '}'])
            .split(',')
            .map(|v| v.parse().unwrap())
            .collect();

        // Parse buttons (0,1,2) etc.
        let mut buttons: Vec<Vec<usize>> = parts
            .iter()
            .map(|b| {
                b.trim_matches(['(', ')'])
                    .split(',')
                    .map(|v| v.parse().unwrap())
                    .collect()
            })
            .collect();

        // Sorting seems to help here. Not sure why, was just trying stuff.
        buttons.sort_by_key(|b| std::cmp::Reverse(b.len()));

        Self {
            lights,
            buttons,
            joltages,
        }
    }
}

/// Part 1: Find the minimum button presses to toggle lights to match the target pattern.
/// Uses BFS over the state space where state is represented as a bitmask.
fn p1(input: &[String]) -> usize {
    input
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let machine = Machine::from(line.as_str());

            let mut frontier = VecDeque::new();
            frontier.push_back((0, 0)); // (current_lights_state, distance)

            let mut seen = HashSet::new();
            seen.insert(0);

            while let Some((lights, dist)) = frontier.pop_front() {
                if lights == machine.lights {
                    return dist;
                }

                for neighbor in machine.buttons.iter() {
                    // Toggle each light listed in the button (XOR)
                    let neighbor = neighbor.iter().fold(lights, |acc, n| acc ^ (1 << n));
                    if seen.insert(neighbor) {
                        frontier.push_back((neighbor, dist + 1));
                    }
                }
            }
            unreachable!()
        })
        .sum()
}

/// Matrix for Gaussian elimination to solve the linear system for Part 2.
struct Matrix {
    data: Vec<Vec<f64>>,
    rows: usize,
    cols: usize,
    dependents: Vec<usize>,
    independents: Vec<usize>,
}

impl Matrix {
    /// Make a matrix, do a Gaussian elimination and setup the fixed and free variables.
    fn from_machine(machine: &Machine) -> Self {
        let rows = machine.joltages.len();
        let cols = machine.buttons.len();
        let mut data = vec![vec![0.0; cols + 1]; rows];

        // Add all of our buttons.
        for (c, button) in machine.buttons.iter().enumerate() {
            for &r in button {
                if r < rows {
                    data[r][c] = 1.0;
                }
            }
        }

        // Add our joltages to the last column
        for (r, &val) in machine.joltages.iter().enumerate() {
            data[r][cols] = val as f64;
        }

        let mut matrix = Self {
            data,
            rows,
            cols,
            dependents: Vec::new(),
            independents: Vec::new(),
        };
        matrix.gaussian_elimination();
        matrix
    }

    /// Perform Gaussian elimination to reduce the matrix to row echelon form.
    fn gaussian_elimination(&mut self) {
        let mut pivot = 0;
        let mut col = 0;

        while pivot < self.rows && col < self.cols {
            // Find the maximum absolute value in this column (for numerical stability)
            let max_row = (pivot..self.rows)
                .max_by(|&a, &b| {
                    self.data[a][col]
                        .abs()
                        .partial_cmp(&self.data[b][col].abs())
                        .unwrap()
                })
                .unwrap();

            if self.data[max_row][col].abs() < EPSILON {
                // No pivot in this column, it's a free variable
                self.independents.push(col);
                col += 1;
                continue;
            }

            // Swap rows
            self.data.swap(pivot, max_row);

            // This column has a pivot (dependent variable)
            self.dependents.push(col);

            // Normalize pivot row.
            let pivot_value = self.data[pivot][col];
            for val in &mut self.data[pivot][col..=self.cols] {
                *val /= pivot_value;
            }

            // Eliminate this column in all other rows.
            for r in 0..self.rows {
                if r != pivot {
                    let factor = self.data[r][col];
                    if factor.abs() > EPSILON {
                        let pivot_row = self.data[pivot][col..=self.cols].to_vec();
                        self.data[r][col..=self.cols]
                            .iter_mut()
                            .zip(&pivot_row)
                            .for_each(|(val, &pivot_val)| {
                                *val -= factor * pivot_val;
                            });
                    }
                }
            }

            pivot += 1;
            col += 1;
        }

        // Any remaining columns are free variables
        self.independents.extend(col..self.cols);
    }

    /// Check if the given values for our independent variables are valid.
    /// If so, return the total button presses.
    fn valid(&self, values: &[usize]) -> Option<usize> {
        // We start with how many times we've pressed the free variables.
        let mut total = values.iter().sum::<usize>();

        // Calculate dependent variable values based on independent variables.
        for row in 0..self.dependents.len() {
            // Calculate this dependent by subtracting the sum of the free variable pushes from the solution.
            let val = self
                .independents
                .iter()
                .enumerate()
                .fold(self.data[row][self.cols], |acc, (i, &col)| {
                    acc - self.data[row][col] * (values[i] as f64)
                });

            // We need non-negative, whole numbers for a valid solution.
            if val < -EPSILON {
                return None;
            }
            let rounded = val.round();
            if (val - rounded).abs() > EPSILON {
                return None;
            }

            total += rounded as usize;
        }

        Some(total)
    }
}

/// DFS to find the minimum button presses by trying different values for independent variables.
fn dfs(matrix: &Matrix, idx: usize, values: &mut [usize], min: &mut usize, max: usize) {
    // When we've assigned all independent variables, check if it's a valid solution.
    if idx == matrix.independents.len() {
        if let Some(total) = matrix.valid(values) {
            *min = (*min).min(total);
        }
        return;
    }

    // Try different values for the current independent variable.
    let total: usize = values[..idx].iter().sum();
    for val in 0..max {
        // Optimization: If we ever go above our min, we can't possibly do better.
        if total + val >= *min {
            break;
        }
        values[idx] = val;
        dfs(matrix, idx + 1, values, min, max);
    }
}

/// Part 2: Find the minimum button presses to reach the joltage requirements.
/// Uses Gaussian elimination to reduce the search space, then DFS.
fn p2(input: &[String]) -> usize {
    input
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let machine = Machine::from(line.as_str());
            let matrix = Matrix::from_machine(&machine);

            // Now we can DFS over a much smaller solution space.
            let max = *machine.joltages.iter().max().unwrap() + 1;
            let mut min = usize::MAX;
            let mut values = vec![0; matrix.independents.len()];

            dfs(&matrix, 0, &mut values, &mut min, max);

            min
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_test_input(input: &str) -> Vec<String> {
        input
            .lines()
            .filter(|line| !line.is_empty())
            .map(String::from)
            .collect()
    }

    #[test]
    fn test_p1() {
        let input = parse_test_input(
            "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}",
        );
        assert_eq!(p1(&input), 7);
    }

    #[test]
    fn test_p2() {
        let input = parse_test_input(
            "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}",
        );
        assert_eq!(p2(&input), 33);
    }
}
