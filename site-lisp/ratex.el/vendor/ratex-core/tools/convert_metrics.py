#!/usr/bin/env python3
"""Convert KaTeX fontMetricsData.js into a Rust source file."""

import json
import os
import re
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
INPUT_FILE = os.path.join(SCRIPT_DIR, "katex_fontMetricsData.js")
OUTPUT_FILE = os.path.join(
    SCRIPT_DIR, "..", "crates", "ratex-font", "src", "data", "metrics_data.rs"
)


def parse_js_metrics(path: str) -> dict:
    with open(path, "r") as f:
        text = f.read()

    # Strip the JS module wrapper to get a JSON object
    text = re.sub(r"^.*?export\s+default\s*", "", text, count=1, flags=re.DOTALL)
    text = text.rstrip().rstrip(";")

    # Remove trailing commas before } or ] (not valid JSON but common in JS)
    text = re.sub(r",\s*([}\]])", r"\1", text)

    return json.loads(text)


def font_name_to_rust(name: str) -> str:
    return name.replace("-", "_").upper()


def format_f64(v: float) -> str:
    s = f"{v:.5f}"
    # Strip unnecessary trailing zeros but keep at least one decimal
    s = s.rstrip("0").rstrip(".")
    if "." not in s:
        s += ".0"
    return s


def generate_rust(data: dict) -> str:
    lines = [
        "// Auto-generated from KaTeX fontMetricsData.js — DO NOT EDIT",
        "",
        "/// Each entry is (char_code, depth, height, italic, skew, width)",
        "pub type MetricsEntry = (u32, f64, f64, f64, f64, f64);",
        "",
    ]

    for font_name in sorted(data.keys()):
        entries = data[font_name]
        rust_name = font_name_to_rust(font_name)
        lines.append(f"pub static {rust_name}: &[MetricsEntry] = &[")
        for code in sorted(entries.keys(), key=int):
            vals = entries[code]
            parts = ", ".join(format_f64(v) for v in vals)
            lines.append(f"    ({code}, {parts}),")
        lines.append("];")
        lines.append("")

    return "\n".join(lines)


def main():
    data = parse_js_metrics(INPUT_FILE)
    rust_src = generate_rust(data)

    os.makedirs(os.path.dirname(OUTPUT_FILE), exist_ok=True)
    with open(OUTPUT_FILE, "w") as f:
        f.write(rust_src)

    font_count = len(data)
    entry_count = sum(len(v) for v in data.values())
    print(f"Generated {OUTPUT_FILE}")
    print(f"  {font_count} fonts, {entry_count} total entries")


if __name__ == "__main__":
    main()
