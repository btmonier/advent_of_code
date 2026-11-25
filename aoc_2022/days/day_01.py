from aoc.input import read_input


def elf_totals(lines: list[str]) -> list[int]:
    """Sum the Calories carried by each Elf. Blank lines separate inventories."""
    totals: list[int] = []
    current = 0
    for line in lines:
        if line == "":
            totals.append(current)
            current = 0
        else:
            current += int(line)
    totals.append(current)
    return totals


def part_one(lines: list[str]) -> int:
    """Calories carried by the Elf with the most food."""
    return max(elf_totals(lines))


def part_two(lines: list[str]) -> int:
    """Total Calories carried by the three Elves with the most food."""
    return sum(sorted(elf_totals(lines), reverse=True)[:3])


def run() -> None:
    lines = read_input("day_01")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
