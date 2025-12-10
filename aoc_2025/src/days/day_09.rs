use crate::read_input;

pub fn run() {
    let input = read_input("day_09");
    let red_tiles = parse_red_tiles(&input);

    let res01 = largest_rectangle_area(&red_tiles);
    let res02 = largest_rectangle_area_with_green(&red_tiles);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02); // ooofff...
}

/// A 2D point representing a red tile position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}

/// Parse input lines into red tile coordinates.
fn parse_red_tiles(lines: &[String]) -> Vec<Point> {
    lines
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let parts: Vec<i64> = line.split(',').map(|s| s.trim().parse().unwrap()).collect();
            Point {
                x: parts[0],
                y: parts[1],
            }
        })
        .collect()
}

/// Find the largest rectangle area using any two red tiles as opposite corners.
/// The area of a rectangle with opposite corners at (x1, y1) and (x2, y2) is:
/// area = (|x2 - x1| + 1) * (|y2 - y1| + 1)
/// The +1 accounts for inclusive endpoints (both corners are part of the rectangle).
fn largest_rectangle_area(red_tiles: &[Point]) -> i64 {
    let n = red_tiles.len();
    let mut max_area = 0;

    // Check all pairs of red tiles
    for i in 0..n {
        for j in (i + 1)..n {
            let p1 = red_tiles[i];
            let p2 = red_tiles[j];

            // Calculate the area of the rectangle formed by these opposite corners
            // Both corners are included, so we add 1 to both dimensions
            let width = (p2.x - p1.x).abs() + 1;
            let height = (p2.y - p1.y).abs() + 1;
            let area = width * height;

            max_area = max_area.max(area);
        }
    }

    max_area
}

/// Determine if a point is inside a polygon using ray casting algorithm.
/// The polygon is defined by the ordered red tiles (which form a closed loop).
/// We shoot a horizontal ray to the right and count edge intersections.
fn point_in_polygon(point: Point, polygon: &[Point]) -> bool {
    let n = polygon.len();
    if n < 3 {
        return false;
    }

    let mut inside = false;
    let mut j = n - 1;

    for i in 0..n {
        let pi = polygon[i];
        let pj = polygon[j];

        // Skip horizontal edges (they don't affect ray casting with horizontal rays)
        if pi.y == pj.y {
            j = i;
            continue;
        }

        // Check if ray from point going right intersects edge (pi, pj)
        // The ray intersects if:
        // 1. The edge crosses the ray's y-level (one endpoint above, one below)
        // 2. The intersection point is to the right of the point
        if (pi.y > point.y) != (pj.y > point.y) {
            // Calculate x-coordinate of intersection
            let dx = pj.x - pi.x;
            let dy = pj.y - pi.y;
            let x_intersect = pi.x + dx * (point.y - pi.y) / dy;

            if point.x < x_intersect {
                inside = !inside;
            }
        }

        j = i;
    }

    inside
}

/// Check if a point is on any edge of the polygon (including vertices).
fn point_on_polygon_edge(point: Point, polygon: &[Point]) -> bool {
    let n = polygon.len();
    for i in 0..n {
        let pi = polygon[i];
        let pj = polygon[(i + 1) % n];

        if pi.y == pj.y {
            // Horizontal edge
            if point.y == pi.y {
                let min_x = pi.x.min(pj.x);
                let max_x = pi.x.max(pj.x);
                if min_x <= point.x && point.x <= max_x {
                    return true;
                }
            }
        } else {
            // Vertical edge
            if point.x == pi.x {
                let min_y = pi.y.min(pj.y);
                let max_y = pi.y.max(pj.y);
                if min_y <= point.y && point.y <= max_y {
                    return true;
                }
            }
        }
    }
    false
}

/// Check if a point is inside the polygon or on its boundary.
fn point_inside_or_on_boundary(point: Point, polygon: &[Point]) -> bool {
    point_on_polygon_edge(point, polygon) || point_in_polygon(point, polygon)
}

/// Check if a rectangle is valid (entirely inside the polygon) using geometric reasoning.
/// This is O(n) where n is the number of polygon edges, instead of O(area).
///
/// A rectangle is valid if:
/// 1. No polygon edge passes through the open interior of the rectangle
/// 2. The interior of the rectangle is inside the polygon
fn rectangle_is_valid_geometric(p1: Point, p2: Point, polygon: &[Point]) -> bool {
    let min_x = p1.x.min(p2.x);
    let max_x = p1.x.max(p2.x);
    let min_y = p1.y.min(p2.y);
    let max_y = p1.y.max(p2.y);

    let n = polygon.len();

    // Check if any polygon edge passes through the open interior of the rectangle
    for i in 0..n {
        let pi = polygon[i];
        let pj = polygon[(i + 1) % n];

        if pi.y == pj.y {
            // Horizontal edge at y = pi.y
            let edge_y = pi.y;
            let edge_x_min = pi.x.min(pj.x);
            let edge_x_max = pi.x.max(pj.x);

            // Does this edge pass through the open interior?
            // Interior y: min_y < y < max_y
            // Interior x: min_x < x < max_x
            if min_y < edge_y && edge_y < max_y {
                // Edge is at a y strictly inside the rectangle
                // Check if edge overlaps the x-interior
                if edge_x_min < max_x && edge_x_max > min_x {
                    // Edge passes through the interior
                    return false;
                }
            }
        } else {
            // Vertical edge at x = pi.x
            let edge_x = pi.x;
            let edge_y_min = pi.y.min(pj.y);
            let edge_y_max = pi.y.max(pj.y);

            // Does this edge pass through the open interior?
            if min_x < edge_x && edge_x < max_x {
                // Edge is at an x strictly inside the rectangle
                // Check if edge overlaps the y-interior
                if edge_y_min < max_y && edge_y_max > min_y {
                    // Edge passes through the interior
                    return false;
                }
            }
        }
    }

    // No edge passes through the interior
    // Now check if a point inside the rectangle is inside the polygon

    // Handle degenerate cases (line or point)
    if max_x == min_x && max_y == min_y {
        // Single point (corner) - corners are red tiles, always valid
        return true;
    }

    if max_x == min_x || max_y == min_y {
        // It's a line segment - check the midpoint
        let mid = Point {
            x: (min_x + max_x) / 2,
            y: (min_y + max_y) / 2,
        };
        return point_inside_or_on_boundary(mid, polygon);
    }

    // There are strict interior points - check one of them
    // For a rectangle with width > 1 and height > 1, check the center
    // We use a point that's definitely in the open interior
    let test_x = if max_x - min_x >= 2 {
        min_x + 1
    } else {
        // Width is exactly 2, so interior x is just min_x + 0.5, but we're on integers
        // All integer x values are on the boundary of the rectangle
        // In this case, check points on the vertical edges
        min_x
    };
    let test_y = if max_y - min_y >= 2 {
        min_y + 1
    } else {
        min_y
    };

    // If both dimensions are exactly 2, we need to check edge points
    if max_x - min_x < 2 || max_y - min_y < 2 {
        // Check multiple sample points on the rectangle edges
        // Sample points on each edge (not at corners)
        if max_x > min_x {
            let mid_x = (min_x + max_x) / 2;
            if !point_inside_or_on_boundary(Point { x: mid_x, y: min_y }, polygon) {
                return false;
            }
            if max_y > min_y
                && !point_inside_or_on_boundary(Point { x: mid_x, y: max_y }, polygon)
            {
                return false;
            }
        }
        if max_y > min_y {
            let mid_y = (min_y + max_y) / 2;
            if !point_inside_or_on_boundary(Point { x: min_x, y: mid_y }, polygon) {
                return false;
            }
            if max_x > min_x
                && !point_inside_or_on_boundary(Point { x: max_x, y: mid_y }, polygon)
            {
                return false;
            }
        }
        return true;
    }

    let test_point = Point {
        x: test_x,
        y: test_y,
    };
    point_inside_or_on_boundary(test_point, polygon)
}

/// Find the largest rectangle area using only red and green tiles.
/// Uses geometric reasoning instead of tile enumeration for efficiency.
fn largest_rectangle_area_with_green(red_tiles: &[Point]) -> i64 {
    let n = red_tiles.len();
    let mut max_area = 0;

    // Check all pairs of red tiles as opposite corners
    for i in 0..n {
        for j in (i + 1)..n {
            let p1 = red_tiles[i];
            let p2 = red_tiles[j];

            // Calculate potential area
            let width = (p2.x - p1.x).abs() + 1;
            let height = (p2.y - p1.y).abs() + 1;
            let area = width * height;

            // Early skip: if this can't beat the current max, skip
            if area <= max_area {
                continue;
            }

            // Check if this rectangle is entirely inside the polygon
            if rectangle_is_valid_geometric(p1, p2, red_tiles) {
                max_area = area;
            }
        }
    }

    max_area
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_test_input(input: &str) -> Vec<Point> {
        let lines: Vec<String> = input
            .lines()
            .filter(|line| !line.is_empty())
            .map(String::from)
            .collect();
        parse_red_tiles(&lines)
    }

    #[test]
    fn test_example() {
        let input = "\
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3";

        let red_tiles = parse_test_input(input);
        assert_eq!(red_tiles.len(), 8);

        let result = largest_rectangle_area(&red_tiles);
        assert_eq!(result, 50);
    }

    #[test]
    fn test_example_part2() {
        let input = "\
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3";

        let red_tiles = parse_test_input(input);
        // The largest rectangle using only red and green tiles should have area 24
        let result = largest_rectangle_area_with_green(&red_tiles);
        assert_eq!(result, 24);
    }
}
