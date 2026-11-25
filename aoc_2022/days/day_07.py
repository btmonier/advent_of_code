from aoc.input import read_input


def directory_sizes(lines: list[str]) -> dict[tuple[str, ...], int]:
    """Total size of every directory, including files in subdirectories."""
    sizes: dict[tuple[str, ...], int] = {}
    seen: set[tuple[tuple[str, ...], str]] = set()
    path: list[str] = []

    for line in lines:
        if line.startswith("$ cd "):
            target = line.removeprefix("$ cd ")
            if target == "/":
                path = []
            elif target == "..":
                path.pop()
            else:
                path.append(target)
            continue
        if line.startswith("$") or line.startswith("dir "):
            continue

        size_text, name = line.split()
        location = tuple(path)
        if (location, name) in seen:
            continue
        seen.add((location, name))
        size = int(size_text)
        for depth in range(len(location) + 1):
            directory = location[:depth]
            sizes[directory] = sizes.get(directory, 0) + size

    return sizes


def part_one(lines: list[str]) -> int:
    """Sum of directory totals that are at most 100000."""
    return sum(size for size in directory_sizes(lines).values() if size <= 100_000)


def part_two(lines: list[str]) -> int:
    """Smallest directory that frees enough space for the update."""
    sizes = directory_sizes(lines)
    used = sizes[()]
    needed = 30_000_000 - (70_000_000 - used)
    return min(size for size in sizes.values() if size >= needed)


def run() -> None:
    lines = read_input("day_07")
    print(f"Part 1 answer: {part_one(lines)}")
    print(f"Part 2 answer: {part_two(lines)}")
