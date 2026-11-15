from pathlib import Path


def read_input(name: str) -> list[str]:
    """Read lines from the given input txt file."""
    raw = read_input_raw(name)
    if raw == "":
        return []
    return raw.split("\n")


def read_input_raw(name: str) -> str:
    """Read the entire input file as a single string."""
    path = Path("../input/2022") / f"{name}.txt"
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as err:
        raise OSError(f"Could not read file: {path}") from err
    return text.replace("\r\n", "\n").strip()
