from aoc.input import read_input


def parse(lines: list[str]) -> tuple[list[list[str]], list[tuple[int, int, int]]]:
    """Split the drawing into stacks (bottom to top) and move instructions."""
    split_at = lines.index("")
    diagram = lines[:split_at]
    moves = []
    for line in lines[split_at + 1 :]:
        count, src, dst = line.split()[1::2]
        moves.append((int(count), int(src), int(dst)))

    stacks: list[list[str]] = []
    for index, label in enumerate(diagram[-1]):
        if not label.isdigit():
            continue
        stack = [
            row[index]
            for row in reversed(diagram[:-1])
            if index < len(row) and row[index].isalpha()
        ]
        stacks.append(stack)
    return stacks, moves


def part_one(lines: list[str]) -> str:
    """Crates on top after moving one crate at a time."""
    stacks, moves = parse(lines)
    for count, src, dst in moves:
        for _ in range(count):
            stacks[dst - 1].append(stacks[src - 1].pop())
    return "".join(stack[-1] for stack in stacks)


def part_two(lines: list[str]) -> str:
    """Crates on top after moving each group together, order preserved."""
    stacks, moves = parse(lines)
    for count, src, dst in moves:
        moved = stacks[src - 1][-count:]
        del stacks[src - 1][-count:]
        stacks[dst - 1].extend(moved)
    return "".join(stack[-1] for stack in stacks)


def run() -> None:
    lines = read_input("day_05")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
