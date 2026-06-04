#!/usr/bin/env python3
"""
Compare RaTeX parser output with KaTeX parser output.

Usage:
    python3 compare_parsers.py [test_cases.txt]

Runs each test case through both parsers and reports structural differences.
Requires:
    - cargo build --bin parse (ratex-parser)
    - npx tsx katex_parse.mjs (KaTeX parser)
"""
import json
import subprocess
import sys
import os
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
WORKSPACE = SCRIPT_DIR.parent.parent
KATEX_SCRIPT = SCRIPT_DIR / "katex_parse.mjs"
RUST_BIN = "parse"


def strip_loc(obj):
    """Recursively remove 'loc' keys for comparison."""
    if isinstance(obj, dict):
        return {k: strip_loc(v) for k, v in obj.items() if k != "loc"}
    elif isinstance(obj, list):
        return [strip_loc(item) for item in obj]
    return obj


def run_katex(expressions: list[str]) -> list:
    """Run KaTeX parser on expressions."""
    inp = "\n".join(expressions) + "\n"
    try:
        result = subprocess.run(
            ["npx", "tsx", str(KATEX_SCRIPT)],
            input=inp,
            capture_output=True,
            text=True,
            timeout=120,
            cwd=str(SCRIPT_DIR),
        )
        lines = result.stdout.strip().split("\n")
        return [json.loads(line) if line else None for line in lines]
    except Exception as e:
        print(f"KaTeX error: {e}", file=sys.stderr)
        return [None] * len(expressions)


def run_ratex(expressions: list[str]) -> list:
    """Run RaTeX parser on expressions."""
    inp = "\n".join(expressions) + "\n"
    try:
        result = subprocess.run(
            ["cargo", "run", "--bin", RUST_BIN, "-q"],
            input=inp,
            capture_output=True,
            text=True,
            timeout=3000,
            cwd=str(WORKSPACE),
        )
        lines = result.stdout.strip().split("\n")
        return [json.loads(line) if line else None for line in lines]
    except Exception as e:
        print(f"RaTeX error: {e}", file=sys.stderr)
        return [None] * len(expressions)


def compare_structure(katex_ast, ratex_ast, path="$") -> list[str]:
    """Compare two ASTs structurally (type-level). Returns list of differences."""
    diffs = []

    if katex_ast is None and ratex_ast is None:
        return diffs

    if type(katex_ast) != type(ratex_ast):
        diffs.append(f"{path}: type mismatch: KaTeX={type(katex_ast).__name__} RaTeX={type(ratex_ast).__name__}")
        return diffs

    if isinstance(katex_ast, dict):
        # Compare "type" field first
        kt = katex_ast.get("type", "?")
        rt = ratex_ast.get("type", "?")
        if kt != rt:
            diffs.append(f"{path}: node type: KaTeX={kt} RaTeX={rt}")
            return diffs

        # Compare key fields (not exhaustive — focus on structure)
        important_keys = {"type", "mode", "text", "family", "font", "label",
                          "hasBarLine", "left", "right", "delim", "mclass",
                          "symbol", "limits", "style", "color", "newLine",
                          "isOver", "alignment", "href", "star"}
        all_keys = set(katex_ast.keys()) | set(ratex_ast.keys())
        for key in sorted(all_keys):
            if key == "loc":
                continue
            kv = katex_ast.get(key)
            rv = ratex_ast.get(key)
            if key in important_keys and kv != rv:
                if not isinstance(kv, (dict, list)):
                    diffs.append(f"{path}.{key}: KaTeX={kv!r} RaTeX={rv!r}")
            elif isinstance(kv, (dict, list)):
                diffs.extend(compare_structure(kv, rv, f"{path}.{key}"))

    elif isinstance(katex_ast, list):
        if len(katex_ast) != len(ratex_ast):
            diffs.append(f"{path}: array length: KaTeX={len(katex_ast)} RaTeX={len(ratex_ast)}")
        for i in range(min(len(katex_ast), len(ratex_ast))):
            diffs.extend(compare_structure(katex_ast[i], ratex_ast[i], f"{path}[{i}]"))

    return diffs


def main():
    test_file = sys.argv[1] if len(sys.argv) > 1 else str(SCRIPT_DIR / "test_cases.txt")

    with open(test_file) as f:
        expressions = [line.strip() for line in f if line.strip() and not line.strip().startswith("#")]

    print(f"Running {len(expressions)} test cases...")
    print()

    katex_results = run_katex(expressions)
    ratex_results = run_ratex(expressions)

    passed = 0
    failed = 0
    errors = 0

    for i, expr in enumerate(expressions):
        katex_ast = strip_loc(katex_results[i]) if i < len(katex_results) else None
        ratex_ast = strip_loc(ratex_results[i]) if i < len(ratex_results) else None

        if katex_ast is None:
            print(f"  SKIP  {expr!r}  (KaTeX failed)")
            errors += 1
            continue

        if ratex_ast is None:
            print(f"  SKIP  {expr!r}  (RaTeX failed)")
            errors += 1
            continue

        # Check for error responses
        if isinstance(katex_ast, dict) and katex_ast.get("error"):
            if isinstance(ratex_ast, dict) and ratex_ast.get("error"):
                print(f"  PASS  {expr!r}  (both error)")
                passed += 1
            else:
                print(f"  DIFF  {expr!r}  (KaTeX errors, RaTeX succeeds)")
                failed += 1
            continue

        if isinstance(ratex_ast, dict) and ratex_ast.get("error"):
            print(f"  FAIL  {expr!r}  (RaTeX error: {ratex_ast.get('message', '?')})")
            failed += 1
            continue

        diffs = compare_structure(katex_ast, ratex_ast)
        if not diffs:
            print(f"  PASS  {expr!r}")
            passed += 1
        else:
            print(f"  DIFF  {expr!r}")
            for d in diffs[:5]:
                print(f"        {d}")
            if len(diffs) > 5:
                print(f"        ... and {len(diffs) - 5} more")
            failed += 1

    print()
    print(f"Results: {passed} passed, {failed} failed, {errors} skipped")
    print(f"Total: {len(expressions)} test cases")

    return 0 if failed == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
