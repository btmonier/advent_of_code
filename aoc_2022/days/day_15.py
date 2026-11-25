import re

from aoc.input import read_input

READING = re.compile(
    r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
)
ROW = 2_000_000
TUNING = 4_000_000


def readings(lines: list[str]) -> list[tuple[int, int, int, int, int]]:
    """Each sensor as (x, y, beacon x, beacon y, Manhattan distance)."""
    parsed = []
    for line in lines:
        match = READING.fullmatch(line)
        if match is None:
            raise ValueError(f"unrecognized sensor report: {line}")
        sensor_x, sensor_y, beacon_x, beacon_y = (int(value) for value in match.groups())
        distance = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y)
        parsed.append((sensor_x, sensor_y, beacon_x, beacon_y, distance))
    return parsed


def covered_ranges(reports: list[tuple[int, int, int, int, int]], row: int) -> list[tuple[int, int]]:
    """Merged inclusive x-intervals on ``row`` where no undetected beacon can sit."""
    intervals = []
    for sensor_x, sensor_y, _, _, distance in reports:
        reach = distance - abs(sensor_y - row)
        if reach >= 0:
            intervals.append((sensor_x - reach, sensor_x + reach))
    intervals.sort()
    merged: list[tuple[int, int]] = []
    for start, end in intervals:
        if merged and start <= merged[-1][1] + 1:
            merged[-1] = (merged[-1][0], max(merged[-1][1], end))
        else:
            merged.append((start, end))
    return merged


def part_one(lines: list[str], row: int = ROW) -> int:
    """Positions on ``row`` that cannot contain a beacon."""
    reports = readings(lines)
    ranges = covered_ranges(reports, row)
    covered = sum(end - start + 1 for start, end in ranges)
    beacons = {beacon_x for _, _, beacon_x, beacon_y, _ in reports if beacon_y == row}
    occupied = sum(any(start <= beacon_x <= end for start, end in ranges) for beacon_x in beacons)
    return covered - occupied


def part_two(lines: list[str], limit: int = TUNING) -> int:
    """Tuning frequency of the one position in the search square no sensor covers.

    That cell sits just outside at least two sensor diamonds, on the crossing of
    an ``x + y`` edge and an ``x - y`` edge.
    """
    reports = readings(lines)
    rising: list[int] = []
    falling: list[int] = []
    for sensor_x, sensor_y, _, _, distance in reports:
        border = distance + 1
        rising.append(sensor_x + sensor_y + border)
        rising.append(sensor_x + sensor_y - border)
        falling.append(sensor_x - sensor_y + border)
        falling.append(sensor_x - sensor_y - border)

    for up in rising:
        for down in falling:
            if (up + down) % 2:
                continue
            x = (up + down) // 2
            y = up - x
            if not (0 <= x <= limit and 0 <= y <= limit):
                continue
            if all(
                abs(x - sensor_x) + abs(y - sensor_y) > distance
                for sensor_x, sensor_y, _, _, distance in reports
            ):
                return x * TUNING + y
    raise ValueError("no uncovered position in the search square")


def run() -> None:
    lines = read_input("day_15")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
