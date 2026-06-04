#!/usr/bin/env python3
"""
Compare ratex-layout vs KaTeX layout dimensions.

Reads test cases from stdin or test_cases.txt, runs both engines,
and reports dimension differences.
"""
import subprocess
import json
import sys
import os

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.join(SCRIPT_DIR, "..", "..")

def run_katex(expressions: list[str]) -> list[dict]:
    """Run KaTeX layout extraction."""
    input_text = "\n".join(expressions) + "\n"
    result = subprocess.run(
        ["node", os.path.join(SCRIPT_DIR, "katex_layout.mjs")],
        input=input_text, capture_output=True, text=True, cwd=SCRIPT_DIR,
    )
    results = []
    for line in result.stdout.strip().split("\n"):
        if line.strip():
            results.append(json.loads(line))
    return results


def run_ratex(expressions: list[str]) -> list[dict]:
    """Run ratex-layout."""
    input_text = "\n".join(expressions) + "\n"
    result = subprocess.run(
        ["cargo", "run", "--bin", "ratex-layout", "-q"],
        input=input_text, capture_output=True, text=True, cwd=PROJECT_ROOT,
    )
    results = []
    for line in result.stdout.strip().split("\n"):
        if line.strip():
            results.append(json.loads(line))
    return results


def compare(katex_results, ratex_results, tolerance=0.02):
    """Compare dimensions between KaTeX and RaTeX."""
    total = 0
    passed = 0
    failed = []

    for kr, rr in zip(katex_results, ratex_results):
        if "error" in kr or "error" in rr:
            continue

        expr = kr.get("input", "?")
        total += 1

        # KaTeX dimensions: use max across all struts (KaTeX may split at operators)
        struts = kr.get("struts", [])
        if not struts:
            continue

        katex_h = max(s["ascent"] for s in struts)
        katex_d = max(s["depth"] for s in struts)

        # RaTeX dimensions
        rbox = rr.get("box", {})
        ratex_h = rbox.get("height", 0)
        ratex_d = rbox.get("depth", 0)

        h_diff = abs(katex_h - ratex_h)
        d_diff = abs(katex_d - ratex_d)

        if h_diff <= tolerance and d_diff <= tolerance:
            passed += 1
            status = "✓"
        else:
            status = "✗"
            failed.append(expr)

        print(f"  {status} {expr:30s}  "
              f"h: KaTeX={katex_h:7.4f} RaTeX={ratex_h:7.4f} Δ={h_diff:6.4f}  "
              f"d: KaTeX={katex_d:7.4f} RaTeX={ratex_d:7.4f} Δ={d_diff:6.4f}")

    print(f"\n  Result: {passed}/{total} passed (tolerance={tolerance}em)")
    if failed:
        print(f"  Failed: {', '.join(failed)}")
    return passed, total


def main():
    test_cases = [
        # Basic symbols
        "x",
        "a",
        "A",
        "1",
        # Binary/relational operations with spacing
        "a+b",
        "a+b=c",
        "a+b+c",
        "a-b",
        "a \\cdot b",
        # Superscripts and subscripts
        "x^2",
        "x_i",
        "x^2_i",
        "a^{bc}",
        "x_{ij}",
        "e^{i\\pi}",
        # Fractions
        "\\frac{a}{b}",
        "\\frac{1}{2}",
        "\\frac{x+y}{z}",
        "\\frac{a^2}{b^2}",
        "\\dfrac{a}{b}",
        # Square roots
        "\\sqrt{x}",
        "\\sqrt{2}",
        "\\sqrt{a+b}",
        # Nested structures
        "\\frac{\\sqrt{a}}{b}",
        "\\sqrt{\\frac{a}{b}}",
        "x^{x^2}",
        "(a+b)^2",
        # Core formulas from implementation plan
        "\\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}",
    ]

    print("Running KaTeX layout extraction...")
    katex_results = run_katex(test_cases)

    print("Running RaTeX layout...")
    ratex_results = run_ratex(test_cases)

    print(f"\nComparing {len(test_cases)} test cases:\n")
    compare(katex_results, ratex_results)


if __name__ == "__main__":
    main()
