from aoc.input import read_input

SOURCE = (500, 0)
DROPS = ((0, 1), (-1, 1), (1, 1))


def sign(value: int) -> int:
    return (value > 0) - (value < 0)


def rocks(lines: list[str]) -> set[tuple[int, int]]:
    """Every tile occupied by a rock path."""
    blocked: set[tuple[int, int]] = set()
    for line in lines:
        points = [tuple(int(part) for part in point.split(",")) for point in line.split(" -> ")]
        for (start_x, start_y), (end_x, end_y) in zip(points, points[1:]):
            step_x = sign(end_x - start_x)
            step_y = sign(end_y - start_y)
            x, y = start_x, start_y
            blocked.add((x, y))
            while (x, y) != (end_x, end_y):
                x += step_x
                y += step_y
                blocked.add((x, y))
    return blocked


def units_at_rest(lines: list[str], *, floor: bool) -> int:
    """Sand grains that settle before the flow stops.

    Without a floor, the flow stops when a grain falls past the lowest rock.
    With a floor, an infinite line two units below that rock blocks the grain,
    and the flow stops when sand plugs the source.
    """
    blocked = rocks(lines)
    depth = max(y for _, y in blocked) + (2 if floor else 0)
    settled = 0
    while SOURCE not in blocked:
        x, y = SOURCE
        while True:
            for step_x, step_y in DROPS:
                next_x = x + step_x
                next_y = y + step_y
                if next_y > depth:
                    return settled
                if floor and next_y == depth:
                    continue
                if (next_x, next_y) not in blocked:
                    x, y = next_x, next_y
                    break
            else:
                blocked.add((x, y))
                settled += 1
                break
    return settled


def part_one(lines: list[str]) -> int:
    """Units of sand that come to rest before any grain falls into the abyss."""
    return units_at_rest(lines, floor=False)


def part_two(lines: list[str]) -> int:
    """Units of sand that come to rest before the source is blocked."""
    return units_at_rest(lines, floor=True)


def run() -> None:
    lines = read_input("day_14")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
