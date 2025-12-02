use std::fs;
use std::path::Path;

pub mod days;

/// Reads lines from the given input txt file.
pub fn read_input(name: &str) -> Vec<String> {
    let path = format!("../input/2025/{}.txt", name);
    fs::read_to_string(Path::new(&path))
        .expect(&format!("Could not read file: {}", path))
        .trim()
        .lines()
        .map(String::from)
        .collect()
}

/// Reads the entire input file as a single string.
pub fn read_input_raw(name: &str) -> String {
    let path = format!("../input/2025/{}.txt", name);
    fs::read_to_string(Path::new(&path))
        .expect(&format!("Could not read file: {}", path))
        .trim()
        .to_string()
}
