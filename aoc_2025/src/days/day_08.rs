use crate::read_input;

pub fn run() {
    let input = read_input("day_08");
    let junctions = parse_junctions(&input);

    let res01 = product_of_three_largest_circuits(&junctions, 1000);
    let res02 = last_connection_x_product(&junctions);

    println!("Part 1 answer: {}", res01);
    println!("Part 2 answer: {}", res02);
}

/// A 3D point representing a junction box position.
#[derive(Debug, Clone, Copy)]
struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

impl Point3D {
    /// Calculate squared Euclidean distance to another point.
    /// Using squared distance avoids floating point and sqrt.
    fn distance_squared(&self, other: &Point3D) -> i64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        let dz = self.z - other.z;
        dx * dx + dy * dy + dz * dz
    }
}

/// Union-Find (Disjoint Set Union) data structure with path compression and union by rank.
struct UnionFind {
    parent: Vec<usize>,
    rank: Vec<usize>,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind {
            parent: (0..n).collect(),
            rank: vec![0; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]); // Path compression
        }
        self.parent[x]
    }

    fn union(&mut self, x: usize, y: usize) -> bool {
        let root_x = self.find(x);
        let root_y = self.find(y);

        if root_x == root_y {
            return false; // Already in the same set
        }

        // Union by rank
        if self.rank[root_x] < self.rank[root_y] {
            self.parent[root_x] = root_y;
        } else if self.rank[root_x] > self.rank[root_y] {
            self.parent[root_y] = root_x;
        } else {
            self.parent[root_y] = root_x;
            self.rank[root_x] += 1;
        }

        true
    }

    /// Get the size of each circuit (connected component).
    fn get_circuit_sizes(&mut self) -> Vec<usize> {
        let n = self.parent.len();
        let mut sizes = vec![0usize; n];

        for i in 0..n {
            let root = self.find(i);
            sizes[root] += 1;
        }

        sizes.into_iter().filter(|&s| s > 0).collect()
    }
}

/// Parse input lines into junction box positions.
fn parse_junctions(lines: &[String]) -> Vec<Point3D> {
    lines
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| {
            let parts: Vec<i64> = line.split(',').map(|s| s.trim().parse().unwrap()).collect();
            Point3D {
                x: parts[0],
                y: parts[1],
                z: parts[2],
            }
        })
        .collect()
}

/// Generate sorted pairs of junction indices by distance.
fn generate_sorted_pairs(junctions: &[Point3D]) -> Vec<(i64, usize, usize)> {
    let n = junctions.len();
    let mut pairs: Vec<(i64, usize, usize)> = Vec::new();
    for i in 0..n {
        for j in (i + 1)..n {
            let dist = junctions[i].distance_squared(&junctions[j]);
            pairs.push((dist, i, j));
        }
    }
    pairs.sort_by_key(|&(dist, _, _)| dist);
    pairs
}

/// Connect the k closest pairs of junction boxes and return the product of the three largest circuit sizes.
fn product_of_three_largest_circuits(junctions: &[Point3D], k: usize) -> u64 {
    let n = junctions.len();
    let pairs = generate_sorted_pairs(junctions);

    // Use Union-Find to connect the k closest pairs
    let mut uf = UnionFind::new(n);
    let mut connections_made = 0;

    for (_, i, j) in pairs {
        // Attempt to connect (even if already connected, it counts toward the 1000)
        uf.union(i, j);
        connections_made += 1;

        if connections_made >= k {
            break;
        }
    }

    // Get circuit sizes and find the three largest
    let mut sizes = uf.get_circuit_sizes();
    sizes.sort_by(|a, b| b.cmp(a)); // Sort descending

    // Multiply the three largest
    let top_three: u64 = sizes.iter().take(3).map(|&s| s as u64).product();
    top_three
}

/// Connect pairs until all junction boxes are in one circuit.
/// Return the product of X coordinates of the last two junction boxes connected.
fn last_connection_x_product(junctions: &[Point3D]) -> i64 {
    let n = junctions.len();
    if n <= 1 {
        return 0;
    }

    let pairs = generate_sorted_pairs(junctions);

    let mut uf = UnionFind::new(n);
    let mut successful_unions = 0;
    let mut last_i = 0;
    let mut last_j = 0;

    // We need n-1 successful unions to connect all nodes
    for (_, i, j) in pairs {
        if uf.union(i, j) {
            // This union actually merged two different circuits
            successful_unions += 1;
            last_i = i;
            last_j = j;

            if successful_unions == n - 1 {
                // All nodes are now connected
                break;
            }
        }
    }

    junctions[last_i].x * junctions[last_j].x
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_test_input(input: &str) -> Vec<Point3D> {
        let lines: Vec<String> = input
            .lines()
            .filter(|line| !line.is_empty())
            .map(String::from)
            .collect();
        parse_junctions(&lines)
    }

    #[test]
    fn test_example_part1() {
        let input = "\
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689";

        let junctions = parse_test_input(input);
        assert_eq!(junctions.len(), 20);

        // After 10 connections: circuits of size 5, 4, 2, 2, and seven of size 1
        // Product of three largest: 5 * 4 * 2 = 40
        assert_eq!(product_of_three_largest_circuits(&junctions, 10), 40);
    }

    #[test]
    fn test_example_part2() {
        let input = "\
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689";

        let junctions = parse_test_input(input);

        // Last connection is between 216,146,977 and 117,168,530
        // Product of X coordinates: 216 * 117 = 25272
        assert_eq!(last_connection_x_product(&junctions), 25272);
    }
}
