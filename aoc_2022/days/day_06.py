from aoc.input import read_input


def marker(signal: str, size: int) -> int:
    """Characters processed before the first run of `size` distinct characters."""
    for end in range(size, len(signal) + 1):
        if len(set(signal[end - size : end])) == size:
            return end
    raise ValueError(f"no {size}-character marker")


def part_one(signal: str) -> int:
    """End of the first start-of-packet marker (four distinct characters)."""
    return marker(signal, 4)


def part_two(signal: str) -> int:
    """End of the first start-of-message marker (fourteen distinct characters)."""
    return marker(signal, 14)


def run() -> None:
    signal = read_input("day_06")[0]
    print(f"Part 1 answer: {part_one(signal)}")
    print(f"Part 2 answer: {part_two(signal)}")
