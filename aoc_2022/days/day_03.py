from aoc.input import read_input


def priority(item: str) -> int:
    """a-z are 1-26, A-Z are 27-52."""
    if item.islower():
        return ord(item) - ord("a") + 1
    return ord(item) - ord("A") + 27


def shared_item(rucksack: str) -> str:
    """The one item type packed into both compartments."""
    half = len(rucksack) // 2
    return (set(rucksack[:half]) & set(rucksack[half:])).pop()


def part_one(lines: list[str]) -> int:
    """Sum of priorities of the item shared by both compartments."""
    return sum(priority(shared_item(line)) for line in lines)


def part_two(lines: list[str]) -> int:
    """Sum of priorities of the badge shared by each group of three Elves."""
    total = 0
    for i in range(0, len(lines), 3):
        badge = set(lines[i]) & set(lines[i + 1]) & set(lines[i + 2])
        total += priority(badge.pop())
    return total


def run() -> None:
    lines = read_input("day_03")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
