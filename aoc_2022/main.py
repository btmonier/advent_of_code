#!/usr/bin/env python3

import sys

from days import run


def main() -> None:
    day = 1
    if len(sys.argv) > 1:
        try:
            day = int(sys.argv[1])
        except ValueError:
            pass

    run(day)


if __name__ == "__main__":
    main()
