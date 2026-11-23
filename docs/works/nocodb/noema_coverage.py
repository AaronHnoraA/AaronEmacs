"""Which Noema source files no test and no document mentions.

Run with -I; paths are passed in, nothing is imported from the trees read.
usage: python3 -I noema_coverage.py EMACS_ROOT OUT_TSV
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
doc_files = list(walk(os.path.join(root, "docs"), (".md",), skip=("/works/nocodb/upstream", "/upstream")))
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
