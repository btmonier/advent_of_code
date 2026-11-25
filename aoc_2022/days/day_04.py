from aoc.input import read_input


def parse_pair(line: str) -> tuple[tuple[int, int], tuple[int, int]]:
    left, right = line.split(",")
    a_start, a_end = left.split("-")
    b_start, b_end = right.split("-")
    return (int(a_start), int(a_end)), (int(b_start), int(b_end))


def fully_contains(a: tuple[int, int], b: tuple[int, int]) -> bool:
    """True when one assignment range covers the other completely."""
    return (a[0] <= b[0] and a[1] >= b[1]) or (b[0] <= a[0] and b[1] >= a[1])


def overlaps(a: tuple[int, int], b: tuple[int, int]) -> bool:
    """True when the two ranges share at least one section."""
    return a[0] <= b[1] and b[0] <= a[1]


def part_one(lines: list[str]) -> int:
    """Count pairs where one range fully contains the other."""
    return sum(fully_contains(*parse_pair(line)) for line in lines)


def part_two(lines: list[str]) -> int:
    """Count pairs whose ranges overlap at all."""
    return sum(overlaps(*parse_pair(line)) for line in lines)


def run() -> None:
    lines = read_input("day_04")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
