from aoc.input import read_input

# Rock, Paper, Scissors. Each shape beats the one before it.
SHAPES = {"A": 0, "B": 1, "C": 2, "X": 0, "Y": 1, "Z": 2}


def round_score(opponent: str, response: str) -> int:
    """Score one round: shape points plus 0, 3, or 6 for the outcome."""
    them = SHAPES[opponent]
    me = SHAPES[response]
    shape_points = me + 1
    # Stepping one shape ahead is a win (6), staying put is a draw (3).
    outcome_points = ((me - them + 1) % 3) * 3
    return shape_points + outcome_points


def part_one(lines: list[str]) -> int:
    """Total score if the second column is the shape you play."""
    return sum(round_score(*line.split()) for line in lines)


def part_two(lines: list[str]) -> int:
    """Total score if the second column is the required outcome."""
    total = 0
    for line in lines:
        opponent, outcome = line.split()
        them = SHAPES[opponent]
        # X lose, Y draw, Z win. Play the shape that produces that result.
        needed = SHAPES[outcome]
        me = (them + needed - 1) % 3
        total += (me + 1) + needed * 3
    return total


def run() -> None:
    lines = read_input("day_02")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
