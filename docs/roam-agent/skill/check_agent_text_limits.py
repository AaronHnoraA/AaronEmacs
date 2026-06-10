#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
LIMIT = 500
FILES = [
    ROOT / "agent" / "project-overview.md",
    ROOT / "agent" / "growth-log.md",
]


def count_body_chars(path: Path) -> int:
    body_lines = [
        line
        for line in path.read_text(encoding="utf-8").splitlines()
        if not line.lstrip().startswith("#")
    ]
    return sum(1 for char in "\n".join(body_lines) if not char.isspace())


def main() -> None:
    failures: list[str] = []
    for path in FILES:
        count = count_body_chars(path)
        rel_path = path.relative_to(ROOT)
        print(f"{rel_path}: {count}/{LIMIT}")
        if count > LIMIT:
            failures.append(f"{rel_path} is {count} chars, limit is {LIMIT}")

    if failures:
        raise SystemExit("\n".join(failures))


if __name__ == "__main__":
    main()
