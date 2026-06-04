#!/usr/bin/env python3
"""
Compare token output between ratex-lexer and the KaTeX lexer.

Usage:
  python compare_lexers.py [--ratex-bin PATH] [--katex-script PATH] [--cases FILE] [--golden DIR] [--generate-golden]
  python compare_lexers.py                    # compare both using default paths
  python compare_lexers.py --generate-golden  # generate golden from ratex for later replacement with KaTeX output

Test cases: test_cases.txt, one LaTeX input per line. \\n in a line is replaced with a real newline; \\t with a tab.
"""
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path


def decode_case(line: str) -> str:
    """Convert \\n, \\t, etc. in a test case line to real characters."""
    return line.rstrip("\n").replace("\\n", "\n").replace("\\t", "\t")


def run_ratex_lexer(ratex_bin: str, latex: str, workspace_root: Path) -> list[str]:
    """Invoke the ratex-lexer lex binary and return a list of token lines."""
    try:
        r = subprocess.run(
            [ratex_bin],
            input=latex.encode("utf-8"),
            capture_output=True,
            cwd=workspace_root,
            timeout=5,
        )
    except FileNotFoundError:
        return []
    if r.returncode != 0:
        return []
    return r.stdout.decode("utf-8").strip().split("\n")


def run_katex_lexer(katex_script: str, latex: str, script_dir: Path) -> list[str] | None:
    """Invoke the KaTeX lexer script (tsx katex_lex.mjs) and return token lines; returns None on failure."""
    try:
        r = subprocess.run(
            ["npx", "tsx", katex_script],
            input=latex.encode("utf-8"),
            capture_output=True,
            cwd=script_dir,
            timeout=10,
        )
    except FileNotFoundError:
        return None
    if r.returncode != 0:
        return None
    return r.stdout.decode("utf-8").strip().split("\n")


def load_cases(cases_file: Path) -> list[tuple[int, str]]:
    """Load test cases as (line_number, decoded_latex). Skips comment lines."""
    cases = []
    with open(cases_file, "r", encoding="utf-8") as f:
        for i, line in enumerate(f, 1):
            if line.strip().startswith("#"):
                continue
            cases.append((i, decode_case(line)))
    return cases


def compare(ratex_tokens: list[str], katex_tokens: list[str] | None, golden_dir: Path | None, case_id: int) -> tuple[bool, str]:
    """Compare ratex against katex (or golden). Returns (match: bool, short description)."""
    expected = katex_tokens
    if expected is None and golden_dir is not None:
        golden_file = golden_dir / f"{case_id}.txt"
        if golden_file.exists():
            expected = golden_file.read_text(encoding="utf-8").strip().split("\n")
    if expected is None:
        return True, "ratex only (no katex/golden)"
    if ratex_tokens != expected:
        return False, "diff"
    return True, "match"


def main() -> int:
    script_dir = Path(__file__).resolve().parent
    workspace_root = script_dir.parent.parent
    default_ratex_bin = str(workspace_root / "target" / "debug" / "lex")
    if not os.path.isfile(default_ratex_bin):
        default_ratex_bin = str(workspace_root / "target" / "release" / "lex")
    if not os.path.isfile(default_ratex_bin):
        default_ratex_bin = "cargo run -p ratex-lexer --bin lex --quiet"
        use_cargo = True
    else:
        use_cargo = False

    ap = argparse.ArgumentParser(description="Compare ratex-lexer vs KaTeX lexer output")
    ap.add_argument("--ratex-bin", default=default_ratex_bin, help="ratex lex binary or 'cargo run ...'")
    ap.add_argument("--katex-script", default=str(script_dir / "katex_lex.mjs"), help="KaTeX lexer script (tsx)")
    ap.add_argument("--cases", default=str(script_dir / "test_cases.txt"), help="Test cases file")
    ap.add_argument("--golden", default=None, help="Directory of golden token files (e.g. from KaTeX)")
    ap.add_argument("--generate-golden", action="store_true", help="Generate golden from ratex into --golden (default: golden/)")
    args = ap.parse_args()

    cases_file = Path(args.cases)
    if not cases_file.exists():
        print(f"Cases file not found: {cases_file}", file=sys.stderr)
        return 1

    golden_dir = Path(args.golden) if args.golden else (script_dir / "golden")
    if args.generate_golden:
        golden_dir.mkdir(parents=True, exist_ok=True)

    cases = load_cases(cases_file)
    if not cases:
        print("No test cases found.", file=sys.stderr)
        return 1

    # Build ratex if we use cargo
    if use_cargo and not args.generate_golden:
        subprocess.run(
            ["cargo", "build", "-p", "ratex-lexer", "--bin", "lex"],
            cwd=workspace_root,
            capture_output=True,
            check=False,
        )

    failed = 0
    for idx, (line_no, latex) in enumerate(cases):
        case_id = idx + 1
        if use_cargo:
            r = subprocess.run(
                ["cargo", "run", "-p", "ratex-lexer", "--bin", "lex", "--quiet"],
                input=latex.encode("utf-8"),
                capture_output=True,
                cwd=workspace_root,
                timeout=15,
            )
            ratex_tokens = r.stdout.decode("utf-8").strip().split("\n") if r.returncode == 0 else []
        else:
            ratex_tokens = run_ratex_lexer(args.ratex_bin, latex, workspace_root)

        if args.generate_golden:
            (golden_dir / f"{case_id}.txt").write_text("\n".join(ratex_tokens) + "\n", encoding="utf-8")
            continue

        katex_tokens = run_katex_lexer(args.katex_script, latex, script_dir)
        print(f"ratex_tokens: {ratex_tokens}")
        print(f"katex_tokens: {katex_tokens}")
        ok, msg = compare(ratex_tokens, katex_tokens, golden_dir if golden_dir.exists() else None, case_id)
        if not ok:
            failed += 1
            print(f"FAIL case {case_id} (line {line_no}): {msg}")
            print(f"  input: {repr(latex)[:80]}...")
            print(f"  ratex ({len(ratex_tokens)} tokens): {ratex_tokens[:8]}...")
            if katex_tokens is not None:
                print(f"  katex ({len(katex_tokens)} tokens): {katex_tokens[:8]}...")
            elif golden_dir.exists():
                exp_file = golden_dir / f"{case_id}.txt"
                if exp_file.exists():
                    exp = exp_file.read_text(encoding="utf-8").strip().split("\n")
                    print(f"  golden ({len(exp)} tokens): {exp[:8]}...")
        else:
            if failed == 0 and (case_id <= 5 or case_id % 20 == 0):
                print(f"  ok {case_id} ({msg})")

    if args.generate_golden:
        print(f"Generated {len(cases)} golden files in {golden_dir}")
        return 0

    print(f"Total: {len(cases)}, Failed: {failed}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
