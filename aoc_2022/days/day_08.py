from aoc.input import read_input


def parse(lines: list[str]) -> list[list[int]]:
    return [[int(char) for char in line] for line in lines]


def is_visible(grid: list[list[int]], row: int, col: int) -> bool:
    """True when every tree toward at least one edge is shorter."""
    height = grid[row][col]
    rows = len(grid)
    cols = len(grid[0])
    return (
        all(grid[row][c] < height for c in range(col))
        or all(grid[row][c] < height for c in range(col + 1, cols))
        or all(grid[r][col] < height for r in range(row))
        or all(grid[r][col] < height for r in range(row + 1, rows))
    )


def part_one(lines: list[str]) -> int:
    """Count trees visible from outside the grid."""
    grid = parse(lines)
    return sum(
        is_visible(grid, row, col)
        for row in range(len(grid))
        for col in range(len(grid[0]))
    )


def viewing_distance(trees: list[int], height: int) -> int:
    """Trees visible along one direction, stopping at the first blocker."""
    seen = 0
    for tree in trees:
        seen += 1
        if tree >= height:
            break
    return seen


def scenic_score(grid: list[list[int]], row: int, col: int) -> int:
    """Product of viewing distances up, down, left, and right."""
    height = grid[row][col]
    up = [grid[r][col] for r in range(row - 1, -1, -1)]
    down = [grid[r][col] for r in range(row + 1, len(grid))]
    left = list(reversed(grid[row][:col]))
    right = grid[row][col + 1 :]
    return (
        viewing_distance(up, height)
        * viewing_distance(down, height)
        * viewing_distance(left, height)
        * viewing_distance(right, height)
    )


def part_two(lines: list[str]) -> int:
    """Highest scenic score of any tree."""
    grid = parse(lines)
    return max(
        scenic_score(grid, row, col)
        for row in range(len(grid))
        for col in range(len(grid[0]))
    )


def run() -> None:
    lines = read_input("day_08")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
