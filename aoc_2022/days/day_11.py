import math
from collections import deque
from collections.abc import Callable
from dataclasses import dataclass, field

from aoc.input import read_input


@dataclass
class Monkey:
    items: deque[int]
    operation: Callable[[int], int]
    divisor: int
    if_true: int
    if_false: int
    inspections: int = field(default=0)


def operation_for(expression: str) -> Callable[[int], int]:
    """Worry update from an operation such as ``old * 19`` or ``old * old``."""
    _, operator, operand = expression.split()
    if operand == "old":
        if operator == "+":
            return lambda old: old + old
        return lambda old: old * old
    amount = int(operand)
    if operator == "+":
        return lambda old, amount=amount: old + amount
    return lambda old, amount=amount: old * amount


def parse_monkeys(lines: list[str]) -> list[Monkey]:
    """Monkeys described by the puzzle notes, in index order."""
    monkeys = []
    index = 0
    while index < len(lines):
        if not lines[index].startswith("Monkey"):
            index += 1
            continue
        items = deque(int(item) for item in lines[index + 1].split(": ", 1)[1].split(", "))
        expression = lines[index + 2].split("= ", 1)[1]
        divisor = int(lines[index + 3].rsplit(" ", 1)[1])
        if_true = int(lines[index + 4].rsplit(" ", 1)[1])
        if_false = int(lines[index + 5].rsplit(" ", 1)[1])
        monkeys.append(
            Monkey(
                items=items,
                operation=operation_for(expression),
                divisor=divisor,
                if_true=if_true,
                if_false=if_false,
            )
        )
        index += 6
    return monkeys


def monkey_business(monkeys: list[Monkey], rounds: int, *, relief: bool) -> int:
    """Product of the two highest inspection counts after the given rounds.

    With relief, worry is divided by 3 after each inspection. Without it, worry
    is reduced modulo the product of every test divisor. Each test only cares
    whether the worry is divisible by that monkey's divisor, and the product is
    a multiple of every divisor, so the tests stay the same.
    """
    modulus = math.prod(monkey.divisor for monkey in monkeys)
    for _ in range(rounds):
        for monkey in monkeys:
            while monkey.items:
                worry = monkey.items.popleft()
                monkey.inspections += 1
                worry = monkey.operation(worry)
                worry = worry // 3 if relief else worry % modulus
                target = monkey.if_true if worry % monkey.divisor == 0 else monkey.if_false
                monkeys[target].items.append(worry)
    busiest = sorted(monkey.inspections for monkey in monkeys)
    return busiest[-1] * busiest[-2]


def part_one(lines: list[str]) -> int:
    """Level of monkey business after 20 rounds, with worry divided by 3."""
    return monkey_business(parse_monkeys(lines), 20, relief=True)


def part_two(lines: list[str]) -> int:
    """Level of monkey business after 10000 rounds, with no relief division."""
    return monkey_business(parse_monkeys(lines), 10_000, relief=False)


def run() -> None:
    lines = read_input("day_11")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
