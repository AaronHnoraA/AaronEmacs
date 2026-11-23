"""Which Noema source files no test and no document mentions.

Run with -I; paths are passed in, nothing is imported from the trees read.
usage: python3 -I noema_coverage.py EMACS_ROOT OUT_TSV [OUT_MD]
"""
import os, re, sys
from collections import defaultdict

root = sys.argv[1]
out = sys.argv[2]
noema = os.path.join(root, "site-lisp/noema")

def walk(base, exts, skip=()):
    for dirpath, dirnames, files in os.walk(base):
        dirnames[:] = [d for d in dirnames if d not in ("node_modules", "dist", "build", "testdata", ".git") and not d.startswith(".")]
        if any(s in dirpath for s in skip):
            continue
        for f in files:
            if f.endswith(exts):
                yield os.path.join(dirpath, f)

groups = {
    "src": (os.path.join(noema, "src"), (".ts", ".mjs"), ()),
    "pages": (os.path.join(noema, "pages"), (".ts",), ()),
    "shared": (os.path.join(noema, "shared"), (".mjs",), ()),
    "server": (os.path.join(noema, "server"), (".mjs",), ()),
    "kernel/noema": (os.path.join(noema, "kernel/noema"), (".go",), ()),
    "lisp": (os.path.join(noema, "lisp"), (".el",), ()),
    "emacs lisp/noema": (os.path.join(root, "lisp/noema"), (".el",), ()),
}

def read_all(paths):
    text = []
    for p in paths:
        try:
            with open(p, encoding="utf-8", errors="ignore") as fh:
                text.append(fh.read())
        except OSError:
            pass
    return "\n".join(text)

test_files = list(walk(os.path.join(noema, "tests"), (".ts", ".mjs", ".el", ".py", ".json")))
test_files += list(walk(os.path.join(root, "test"), (".el",)))
tests_text = read_all(test_files)
doc_files = [p for p in walk(os.path.join(root, "docs"), (".md",), skip=("/works/nocodb/upstream", "/upstream"))
             # This audit's own workbench names the files it found; it is not documentation.
             if "/works/nocodb/" not in p]
doc_files += [os.path.join(root, "AGENT.md"), os.path.join(noema, "CLAUDE.md"), os.path.join(noema, "AGENTS.md")]
docs_text = read_all(doc_files)

rows = []
for group, (base, exts, skip) in groups.items():
    for path in walk(base, exts, skip):
        name = os.path.basename(path)
        if name.endswith(("_test.go", ".test.ts", ".d.ts", ".d.mts")) or name.endswith("~"):
            continue
        stem = re.sub(r"\.(ts|mjs|go|el)$", "", name)
        lines = sum(1 for _ in open(path, encoding="utf-8", errors="ignore"))
        if group == "kernel/noema":
            # A Go file is covered when its package has tests.
            pkg = os.path.dirname(path)
            tested = any(f.endswith("_test.go") for f in os.listdir(pkg))
            documented = os.path.relpath(pkg, noema) in docs_text or os.path.basename(pkg) in docs_text
        else:
            pattern = re.compile(r"(?<![\w-])" + re.escape(stem) + r"(?![\w-])")
            tested = bool(pattern.search(tests_text))
            if group == "emacs lisp/noema":
                rel = os.path.relpath(path, os.path.join(root, "lisp"))
                feature = re.sub(r"\.el$", "", rel)
                tested = tested or feature in tests_text
            documented = bool(pattern.search(docs_text)) or os.path.relpath(path, noema) in docs_text \
                or os.path.relpath(path, root) in docs_text
        rows.append((group, os.path.relpath(path, root), lines, tested, documented))

with open(out, "w") as fh:
    for r in rows:
        fh.write("\t".join(map(str, r)) + "\n")

summary = defaultdict(lambda: [0, 0, 0, 0, 0, 0])
for group, path, lines, tested, documented in rows:
    s = summary[group]
    s[0] += 1; s[1] += lines
    s[2] += tested; s[3] += documented
    if not tested and not documented:
        s[4] += 1; s[5] += lines
print("group\tfiles\tlines\ttested\tdocumented\tneither(files/lines)")
for group, s in summary.items():
    print(f"{group}\t{s[0]}\t{s[1]}\t{s[2]}\t{s[3]}\t{s[4]}/{s[5]}")

if len(sys.argv) > 3:
    import subprocess
    head = subprocess.run(["git", "-C", noema, "rev-parse", "--short", "HEAD"],
                          capture_output=True, text=True).stdout.strip()
    md = ["# Noema 源文件覆盖表", "",
          f"由同目录的 `noema_coverage.py` 生成（`python3 -I noema_coverage.py ~/.config/emacs OUT.tsv noema-coverage.md`，Noema `{head}`）。"
          "“测试”指 Noema `tests/`、Emacs `test/` 中出现该文件名（Go 文件看所在包是否有 `_test.go`）；"
          "“文档”指 `docs/`（本审计的 `works/nocodb/` 除外）、`AGENT.md`、Noema `CLAUDE.md`/`AGENTS.md` 中出现该文件名或路径。"
          "只按名字匹配，间接覆盖（经由引用它的模块被测试）不算在内；结论见 [README](README.md) 的“Noema 审计”。",
          "", "## 汇总", "",
          "| 层 | 文件 | 行数 | 有测试引用 | 有文档提到 | 两者都没有（文件/行） |",
          "| --- | ---: | ---: | ---: | ---: | ---: |"]
    tot = [0, 0, 0, 0, 0, 0]
    for group, v in summary.items():
        md.append(f"| {group} | {v[0]} | {v[1]} | {v[2]} | {v[3]} | {v[4]}/{v[5]} |")
        tot = [a + b for a, b in zip(tot, v)]
    md.append(f"| 合计 | {tot[0]} | {tot[1]} | {tot[2]} | {tot[3]} | {tot[4]}/{tot[5]} |")
    md += ["", "## 逐文件"]
    current = None
    for group, path, lines, tested, documented in rows:
        if group != current:
            current = group
            md += ["", f"### {group}", "", "| 文件 | 行数 | 测试 | 文档 |", "| --- | ---: | :---: | :---: |"]
        md.append(f"| `{path.replace('site-lisp/noema/', '')}` | {lines} | {'✓' if tested else '—'} | {'✓' if documented else '—'} |")
    with open(sys.argv[3], "w") as fh:
        fh.write("\n".join(md) + "\n")
