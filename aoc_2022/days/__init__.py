import sys

from days.day_01 import run as run_day_01
from days.day_02 import run as run_day_02
from days.day_03 import run as run_day_03
from days.day_04 import run as run_day_04
from days.day_05 import run as run_day_05
from days.day_06 import run as run_day_06
from days.day_07 import run as run_day_07
from days.day_08 import run as run_day_08
from days.day_09 import run as run_day_09
from days.day_10 import run as run_day_10
from days.day_11 import run as run_day_11
from days.day_12 import run as run_day_12
from days.day_13 import run as run_day_13
from days.day_14 import run as run_day_14
from days.day_15 import run as run_day_15


def run(day: int) -> None:
    """Execute the solution for the given day."""
    match day:
        case 1:
            run_day_01()
        case 2:
            run_day_02()
        case 3:
            run_day_03()
        case 4:
            run_day_04()
        case 5:
            run_day_05()
        case 6:
            run_day_06()
        case 7:
            run_day_07()
        case 8:
            run_day_08()
        case 9:
            run_day_09()
        case 10:
            run_day_10()
        case 11:
            run_day_11()
        case 12:
            run_day_12()
        case 13:
            run_day_13()
        case 14:
            run_day_14()
        case 15:
            run_day_15()
        case _:
            print(f"Day {day} not implemented yet!", file=sys.stderr)
