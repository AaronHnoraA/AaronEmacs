#!/usr/bin/env python3
"""Convert KaTeX symbols.ts into a Rust source file with static symbol data."""

import os
import re

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
INPUT_FILE = os.path.join(SCRIPT_DIR, "katex_symbols.ts")
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "..", "crates", "ratex-font", "src", "data")
OUTPUT_FILE = os.path.join(OUTPUT_DIR, "symbols_data.rs")

GROUP_VAR_MAP = {
    "accent": "accent-token",
    "bin": "bin",
    "close": "close",
    "inner": "inner",
    "mathord": "mathord",
    "op": "op-token",
    "open": "open",
    "punct": "punct",
    "rel": "rel",
    "spacing": "spacing",
    "textord": "textord",
}

MODE_VAL = {"math": 0, "text": 1}
FONT_VAL = {"main": 0, "ams": 1}

DEFINE_SYMBOL_RE = re.compile(
    r"defineSymbol\(\s*"
    r"(math|text)\s*,\s*"
    r"(main|ams)\s*,\s*"
    r"(accent|bin|close|inner|mathord|op|open|punct|rel|spacing|textord)\s*,\s*"
    r"""(null|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*')\s*,\s*"""
    r"""("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*')"""
    r"(?:\s*,\s*(true))?\s*\)"
)


def parse_js_string(s):
    if s == "null":
        return None
    inner = s[1:-1]
    result = []
    i = 0
    while i < len(inner):
        if inner[i] == "\\" and i + 1 < len(inner):
            c = inner[i + 1]
            if c == "u":
                hex_str = inner[i + 2 : i + 6]
                result.append(chr(int(hex_str, 16)))
                i += 6
            elif c == "n":
                result.append("\n")
                i += 2
            elif c == "t":
                result.append("\t")
                i += 2
            else:
                result.append(c)
                i += 2
        else:
            result.append(inner[i])
            i += 1
    return "".join(result)


def rust_str(s):
    """Escape a Python string for use inside a Rust double-quoted string literal."""
    parts = []
    for c in s:
        cp = ord(c)
        if c == "\\":
            parts.append("\\\\")
        elif c == '"':
            parts.append('\\"')
        elif c == "\n":
            parts.append("\\n")
        elif c == "\t":
            parts.append("\\t")
        elif 0x20 <= cp <= 0x7E:
            parts.append(c)
        else:
            parts.append(f"\\u{{{cp:X}}}")
    return "".join(parts)


def rust_char(c):
    """Format a single character as a Rust char literal (without wrapping Option)."""
    cp = ord(c)
    if c == "\\":
        return "'\\\\'"
    elif c == "'":
        return "'\\''"
    elif 0x20 <= cp <= 0x7E:
        return f"'{c}'"
    else:
        return f"'\\u{{{cp:X}}}'"


def surrogate_to_codepoint(high, low):
    return (high - 0xD800) * 0x400 + (low - 0xDC00) + 0x10000


def main():
    with open(INPUT_FILE, "r", encoding="utf-8") as f:
        content = f.read()

    symbols = []

    for m in DEFINE_SYMBOL_RE.finditer(content):
        mode = MODE_VAL[m.group(1)]
        font = FONT_VAL[m.group(2)]
        group = GROUP_VAR_MAP[m.group(3)]
        replace_val = parse_js_string(m.group(4))
        name = parse_js_string(m.group(5))

        if name.startswith("\\@"):
            continue

        if replace_val is not None and len(replace_val) == 1:
            codepoint = f"Some({rust_char(replace_val)})"
        else:
            codepoint = "None"

        symbols.append((name, mode, font, group, codepoint))

    # --- Loop-generated symbols ---

    math_text_symbols = '0123456789/@."'
    for ch in math_text_symbols:
        symbols.append((ch, 0, 0, "textord", f"Some({rust_char(ch)})"))

    text_symbols = '0123456789!@*()-=+";:?/.,'
    for ch in text_symbols:
        symbols.append((ch, 1, 0, "textord", f"Some({rust_char(ch)})"))

    letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    for ch in letters:
        symbols.append((ch, 0, 0, "mathord", f"Some({rust_char(ch)})"))
        symbols.append((ch, 1, 0, "textord", f"Some({rust_char(ch)})"))

    # Wide characters: Mathematical Alphanumeric Symbols (U+1D400–U+1D7FF)
    # Each entry is (base_codepoint, math_group, text_group)
    wide_letter_bases = [
        (0x1D400, "mathord", "textord"),  # bold
        (0x1D434, "mathord", "textord"),  # italic
        (0x1D468, "mathord", "textord"),  # bold italic
        (0x1D504, "mathord", "textord"),  # Fraktur
        (0x1D56C, "mathord", "textord"),  # bold Fraktur
        (0x1D5A0, "mathord", "textord"),  # sans-serif
        (0x1D5D4, "mathord", "textord"),  # sans bold
        (0x1D608, "mathord", "textord"),  # sans italic
        (0x1D670, "mathord", "textord"),  # monospace
    ]
    for base, mg, tg in wide_letter_bases:
        for i, ch in enumerate(letters):
            wc = chr(base + i)
            cp_lit = f"Some({rust_char(ch)})"
            symbols.append((wc, 0, 0, mg, cp_lit))
            symbols.append((wc, 1, 0, tg, cp_lit))

    # Double-struck and script (A-Z only)
    for i in range(26):
        ch = letters[i]
        cp_lit = f"Some({rust_char(ch)})"
        for base in [0x1D538, 0x1D49C]:  # double-struck, script
            wc = chr(base + i)
            symbols.append((wc, 0, 0, "mathord", cp_lit))
            symbols.append((wc, 1, 0, "textord", cp_lit))

    # k double-struck
    wc = chr(0x1D55C)
    symbols.append((wc, 0, 0, "mathord", "Some('k')"))
    symbols.append((wc, 1, 0, "textord", "Some('k')"))

    # Wide numerals
    wide_num_bases = [0x1D7CE, 0x1D7E2, 0x1D7EC, 0x1D7F6]
    for base in wide_num_bases:
        for i in range(10):
            ch = str(i)
            wc = chr(base + i)
            cp_lit = f"Some({rust_char(ch)})"
            symbols.append((wc, 0, 0, "mathord", cp_lit))
            symbols.append((wc, 1, 0, "textord", cp_lit))

    # Extra Latin: Ð Þ þ
    for ch in "\u00D0\u00DE\u00FE":
        cp_lit = f"Some({rust_char(ch)})"
        symbols.append((ch, 0, 0, "mathord", cp_lit))
        symbols.append((ch, 1, 0, "textord", cp_lit))

    # --- Generate Rust output ---

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
        f.write("// Auto-generated from KaTeX symbols.ts - DO NOT EDIT\n\n")
        f.write("/// Symbol definition: (name, mode, font, group, codepoint)\n")
        f.write("/// mode: 0 = math, 1 = text\n")
        f.write("/// font: 0 = main, 1 = ams\n")
        f.write(
            "pub type SymbolEntry = (&'static str, u8, u8, &'static str, Option<char>);\n\n"
        )
        f.write("pub static SYMBOLS: &[SymbolEntry] = &[\n")
        for name, mode, font, group, codepoint in symbols:
            f.write(f'    ("{rust_str(name)}", {mode}, {font}, "{group}", {codepoint}),\n')
        f.write("];\n")

    print(f"Generated {len(symbols)} symbols to {os.path.abspath(OUTPUT_FILE)}")


if __name__ == "__main__":
    main()
