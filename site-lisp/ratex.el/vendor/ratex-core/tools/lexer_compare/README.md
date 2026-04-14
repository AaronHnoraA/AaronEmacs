# ratex-lexer vs KaTeX Lexer Comparison Tool

Compares token output between [ratex-lexer](../../crates/ratex-lexer) and the [KaTeX](https://katex.org) Lexer across a large set of test cases.
Test cases are sourced from ratex-lexer unit tests, common formulas, and Unicode-related inputs from [KaTeX test/unicode-spec.ts](https://github.com/KaTeX/KaTeX/blob/main/test/unicode-spec.ts).

## Dependencies

- **Rust**: a built `ratex-lexer` with the `lex` binary (or invoked via `cargo run`)
- **Node**: used to run the KaTeX source Lexer (`npx tsx katex_lex.mjs`)
  - Run `npm install` under `tools/lexer_compare` (installs `katex` and `tsx`)

## Usage

```bash
# From the repo root
cd /path/to/RaTeX

# Build the lex binary (optional — the script will fall back to cargo run)
cargo build -p ratex-lexer --bin lex

# Install Node dependencies (once)
cd tools/lexer_compare && npm install && cd ../..

# Run comparison (runs both ratex and KaTeX, compares token sequences case by case)
python3 tools/lexer_compare/compare_lexers.py
```

## Options

- `--ratex-bin PATH`: path to the ratex `lex` binary (default: `target/debug/lex` or `target/release/lex`)
- `--katex-script PATH`: KaTeX lexer script (default: `tools/lexer_compare/katex_lex.mjs`)
- `--cases FILE`: test case file (default: `tools/lexer_compare/test_cases.txt`)
- `--golden DIR`: golden directory (when output differs from KaTeX, you can generate golden from ratex then replace with KaTeX output as a baseline)
- `--generate-golden`: write current ratex output as golden files to `--golden` (default: `golden/`)

## Test Case Format

`test_cases.txt`: one LaTeX input per line.

- Lines starting with `#` are comments and are skipped.
- `\n` within a line represents a real newline; `\t` represents a tab (backslash is literal `\`).
- An empty line represents the "empty input" test case.

## Component Overview

| File | Description |
|------|-------------|
| `compare_lexers.py` | Main script: reads test cases, calls ratex/KaTeX, compares or generates golden |
| `test_cases.txt` | Test case list (corresponds to ratex-lexer unit tests, plus KaTeX unicode-spec extensions) |
| `katex_lex.mjs` | Reads LaTeX from stdin, outputs one token per line via KaTeX source Lexer (including EOF) |
| `golden/` | Optional; when "expected token sequence" files are present, used for comparison against ratex |

## Using KaTeX as the Golden Baseline

To use KaTeX output as the reference and only run ratex against it:

1. Generate the current golden from ratex:
   `python3 tools/lexer_compare/compare_lexers.py --generate-golden`
2. Overwrite `golden/*.txt` with KaTeX output (e.g. run `katex_lex.mjs` for each case and write to the corresponding numbered `.txt` file)
3. Run:
   `python3 tools/lexer_compare/compare_lexers.py --golden tools/lexer_compare/golden`
   to see the diff between ratex and KaTeX.

## Running One Side Only

- **ratex only** (print tokens from stdin, one per line):
  ```bash
  printf '%s' '\frac{a}{b}' | cargo run -p ratex-lexer --bin lex --quiet
  # or
  printf '%s' '\frac{a}{b}' | ./target/debug/lex
  ```
- **KaTeX only**:
  ```bash
  cd tools/lexer_compare && printf '%s' '\frac{a}{b}' | npx tsx katex_lex.mjs
  ```

## Known Gaps and Differences

The following scenarios are currently **not covered** by the unified comparison, or have **known behavioral differences** from KaTeX.

### Known Behavioral Differences (not in test_cases)

- **Unterminated `\verb`** (e.g. `\verb|x` with no closing delimiter): ratex treats the entire segment as one token; KaTeX does not match the `\verb` regex and splits it into `\verb`, `|`, `x`, etc.

### Content Not Tested at the Lexer Layer

- **Dynamic catcode changes**: set via `MacroExpander.set_catcode`; the lexer only exposes the interface.
- **Token attributes**: `noexpand`, `treat_as_relax`, etc. are set by upper layers; comparison only checks token text.
- **Parser / strict mode**: belongs to the parsing layer, outside the scope of lexer comparison.

### Optional Additions (not yet in test cases)

- **BMP Private Use Area** (U+E000–U+F8FF): explicitly excluded by KaTeX.
- **`\verb` content with newlines**: may differ from KaTeX's `.*?` regex; not covered.
- **More KaTeX tests**: LaTeX fragments from katex-spec, parser tests, etc. can be added to `test_cases.txt`.
