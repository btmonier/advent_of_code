from collections import deque

from aoc.input import read_input

DIRECTIONS = ((0, 1), (0, -1), (1, 0), (-1, 0))


def elevation(cell: str) -> int:
    if cell == "S":
        return ord("a")
    if cell == "E":
        return ord("z")
    return ord(cell)


def positions(lines: list[str], wanted: str) -> set[tuple[int, int]]:
    """Squares equal to ``wanted``. ``a`` also matches S, which has that elevation."""
    return {
        (row, column)
        for row, line in enumerate(lines)
        for column, cell in enumerate(line)
        if cell == wanted or (wanted == "a" and cell == "S")
    }


def fewest_steps(lines: list[str], starts: set[tuple[int, int]]) -> int:
    """Shortest climb from any start to E, rising at most one elevation per step."""
    height = len(lines)
    width = len(lines[0])
    queue: deque[tuple[tuple[int, int], int]] = deque((start, 0) for start in starts)
    seen = set(starts)
    while queue:
        (row, column), steps = queue.popleft()
        if lines[row][column] == "E":
            return steps
        current = elevation(lines[row][column])
        for drow, dcolumn in DIRECTIONS:
            next_row = row + drow
            next_column = column + dcolumn
            if not (0 <= next_row < height and 0 <= next_column < width):
                continue
            next_pos = (next_row, next_column)
            if next_pos in seen:
                continue
            if elevation(lines[next_row][next_column]) > current + 1:
                continue
            seen.add(next_pos)
            queue.append((next_pos, steps + 1))
    raise ValueError("no path to E")


def part_one(lines: list[str]) -> int:
    """Fewest steps from the current position to the best signal."""
    return fewest_steps(lines, positions(lines, "S"))


def part_two(lines: list[str]) -> int:
    """Fewest steps from any elevation-a square to the best signal."""
    return fewest_steps(lines, positions(lines, "a"))


def run() -> None:
    lines = read_input("day_12")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
