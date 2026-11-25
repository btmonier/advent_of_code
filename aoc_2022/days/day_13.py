import json
from functools import cmp_to_key

from aoc.input import read_input

type Packet = int | list["Packet"]

DIVIDERS: list[Packet] = [[[2]], [[6]]]


def compare(left: Packet, right: Packet) -> int:
    """Negative when left belongs before right, positive when it belongs after."""
    if isinstance(left, int) and isinstance(right, int):
        return (left > right) - (left < right)
    if isinstance(left, int):
        return compare([left], right)
    if isinstance(right, int):
        return compare(left, [right])
    for left_item, right_item in zip(left, right):
        order = compare(left_item, right_item)
        if order != 0:
            return order
    return (len(left) > len(right)) - (len(left) < len(right))


def packets(lines: list[str]) -> list[Packet]:
    return [json.loads(line) for line in lines if line]


def part_one(lines: list[str]) -> int:
    """Sum of 1-based indices of packet pairs that are already in order."""
    received = packets(lines)
    pairs = zip(received[::2], received[1::2])
    return sum(index for index, (left, right) in enumerate(pairs, start=1) if compare(left, right) < 0)


def part_two(lines: list[str]) -> int:
    """Product of the 1-based positions of the divider packets [[2]] and [[6]]."""
    ordered = sorted([*packets(lines), *DIVIDERS], key=cmp_to_key(compare))
    return (ordered.index(DIVIDERS[0]) + 1) * (ordered.index(DIVIDERS[1]) + 1)


def run() -> None:
    lines = read_input("day_13")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
