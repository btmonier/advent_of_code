use crate::read_input;

pub fn run() {
    let input = read_input("day_01");

    // Part 1
    // Dial points to 0-99, starts at 50
    // L = left (toward lower numbers), R = right (toward higher numbers)
    // Count how many times the dial points at 0 after any rotation
    let mut position: i32 = 50;
    let mut count_zeros_p1 = 0;

    for line in &input {
        let direction = line.chars().next().unwrap();
        let distance: i32 = line[1..].parse().unwrap();

        match direction {
            'L' => position -= distance,
            'R' => position += distance,
            _ => panic!("Unknown direction: {}", direction),
        }

        // Wrap around to 0-99 range
        position = ((position % 100) + 100) % 100;

        if position == 0 {
            count_zeros_p1 += 1;
        }
    }

    let res01 = count_zeros;

    // Part 2
    // Count every click that causes the dial to point at 0 (during or at end of rotation)
    let mut position: i32 = 50;
    let mut count_zeros_p2 = 0;

    for line in &input {
        let direction = line.chars().next().unwrap();
        let distance: i32 = line[1..].parse().unwrap();

        count_zeros_p2 += count_zeros_in_rotation(position, direction, distance);

        match direction {
            'L' => position -= distance,
            'R' => position += distance,
            _ => panic!("Unknown direction: {}", direction),
        }

        position = ((position % 100) + 100) % 100;
    }

    let res02 = count_zeros_p2;

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// Count how many times we pass through 0 during a rotation
fn count_zeros_in_rotation(start: i32, direction: char, distance: i32) -> i32 {
    match direction {
        'L' => {
            // Moving left (decreasing), we hit 0 at step: start, start+100, start+200, etc.
            if start == 0 {
                // From 0, we hit 0 again at steps 100, 200, etc.
                distance / 100
            } else if distance >= start {
                // First hit at step `start`, then every 100 steps
                1 + (distance - start) / 100
            } else {
                0
            }
        }
        'R' => {
            // Moving right (increasing), we hit 0 at step: (100-start), (200-start), etc.
            if start == 0 {
                // From 0, we hit 0 again at steps 100, 200, etc.
                distance / 100
            } else {
                let first_zero = 100 - start;
                if distance >= first_zero {
                    1 + (distance - first_zero) / 100
                } else {
                    0
                }
            }
        }
        _ => 0,
    }
}

