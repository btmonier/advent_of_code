from collections.abc import Iterator

from aoc.input import read_input

SCREEN_WIDTH = 40


def register_values(lines: list[str]) -> Iterator[tuple[int, int]]:
    """Yield (cycle, X) for every cycle, with X as it is during that cycle."""
    x = 1
    cycle = 0
    for line in lines:
        parts = line.split()
        if parts[0] == "noop":
            cycle += 1
            yield cycle, x
        else:
            for _ in range(2):
                cycle += 1
                yield cycle, x
            x += int(parts[1])


def part_one(lines: list[str]) -> int:
    """Sum of signal strengths during the 20th cycle and every 40 cycles after."""
    return sum(cycle * x for cycle, x in register_values(lines) if cycle % 40 == 20)


def part_two(lines: list[str]) -> str:
    """Image drawn on the CRT."""
    pixels = []
    for cycle, x in register_values(lines):
        column = (cycle - 1) % SCREEN_WIDTH
        pixels.append("#" if abs(column - x) <= 1 else ".")
    rows = [
        "".join(pixels[start : start + SCREEN_WIDTH])
        for start in range(0, len(pixels), SCREEN_WIDTH)
    ]
    return "\n".join(rows)


def run() -> None:
    lines = read_input("day_10")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer:\n{part_two(lines)}")
