from aoc.input import read_input

DIRECTIONS = {"U": (0, 1), "D": (0, -1), "L": (-1, 0), "R": (1, 0)}


def sign(value: int) -> int:
    return (value > 0) - (value < 0)


def positions_visited(lines: list[str], knots: int) -> int:
    """Unique positions visited by the last knot as the head follows the motions."""
    rope = [(0, 0)] * knots
    visited = {(0, 0)}
    for line in lines:
        direction, count = line.split()
        step_x, step_y = DIRECTIONS[direction]
        for _ in range(int(count)):
            head_x, head_y = rope[0]
            rope[0] = (head_x + step_x, head_y + step_y)
            for index in range(1, knots):
                ahead_x, ahead_y = rope[index - 1]
                knot_x, knot_y = rope[index]
                delta_x = ahead_x - knot_x
                delta_y = ahead_y - knot_y
                if abs(delta_x) > 1 or abs(delta_y) > 1:
                    rope[index] = (knot_x + sign(delta_x), knot_y + sign(delta_y))
            visited.add(rope[-1])
    return len(visited)


def part_one(lines: list[str]) -> int:
    """Positions visited by the tail of a two-knot rope."""
    return positions_visited(lines, 2)



def part_two(lines: list[str]) -> int:
    """Positions visited by the tail of a ten-knot rope."""
    return positions_visited(lines, 10)


def run() -> None:
    lines = read_input("day_09")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
