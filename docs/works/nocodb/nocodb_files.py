"""Account for every NocoDB source file: size, exports, header, CE stub.

usage: python3 -I nocodb_files.py UPSTREAM_DIR OUT_TSV OUT_MD
Reads only; nothing from the tree is imported or executed.
"""
import os, re, sys
from collections import defaultdict

up, out_tsv, out_md = sys.argv[1], sys.argv[2], sys.argv[3]
pk = os.path.join(up, "packages")

SKIP_DIRS = {"node_modules", "dist", "build", ".nuxt", ".output", "coverage", "__tests__", "tests", "test", "e2e", "playwright", "cypress"}
STUB_PATTERNS = [
    re.compile(r"static async \w+\(\s*\.\.\._?args"),            # models: static async get(..._args)
    re.compile(r"\bCE stub\b|\bCE has no\b|returns no-op", re.I),  # declared stubs
    re.compile(r"=\s*\(\)\s*=>\s*\{\s*\}"),                       # () => {}
]
EXPORT = re.compile(r"^export\s+(?:default\s+)?(?:async\s+)?(?:class|function|const|interface|type|enum)\s+(\w+)", re.M)
DECOR = re.compile(r"^@(Controller|Injectable|Module)\b", re.M)
TOPIC = {
    "ai": re.compile(r"\b(ai|llm|prompt|embedding|AiUseCase|generateText)\b", re.I),
    "markdown": re.compile(r"markdown|tiptap|prosemirror|richtext|rich-text", re.I),
    "workflow": re.compile(r"workflow", re.I),
    "mcp": re.compile(r"\bmcp\b", re.I),
    "agent/chat": re.compile(r"\b(agent|chat)\w*", re.I),
    "view/grid": re.compile(r"\b(grid|kanban|gallery|calendar|view)\w*", re.I),
}

def header(text):
    for line in text.splitlines()[:40]:
        s = line.strip()
        if s.startswith(("/**", "*", "//")):
            s = s.lstrip("/*").strip()
            if s and not s.startswith(("@", "eslint", "Copyright", "SPDX")):
                return s[:140]
    return ""

rows = []
for dirpath, dirnames, files in os.walk(pk):
    dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS and not d.startswith(".")]
    for f in files:
        if not f.endswith((".ts", ".vue", ".mjs", ".js")) or f.endswith((".d.ts", ".spec.ts", ".test.ts", ".config.ts", ".config.js")):
            continue
        path = os.path.join(dirpath, f)
        try:
            text = open(path, encoding="utf-8", errors="ignore").read()
        except OSError:
            continue
        rel = os.path.relpath(path, pk)
        parts = rel.split(os.sep)
        group = "/".join(parts[:3]) if len(parts) > 3 else "/".join(parts[:-1])
        lines = text.count("\n") + 1
        stub = sum(bool(p.search(text)) for p in STUB_PATTERNS)
        # A file is a stub when it is small and every method body is a stub shape.
        is_stub = stub > 0 and lines < 260 and len(re.findall(r"return (null|\[\]|undefined|\{\})", text)) + text.count("=> {}") >= 2
        topics = [k for k, p in TOPIC.items() if p.search(rel)]
        exports = EXPORT.findall(text)[:6]
        rows.append((parts[0], group, rel, lines, is_stub, ",".join(topics), ",".join(exports), header(text).replace("|", "/")))

rows.sort(key=lambda r: r[2])
with open(out_tsv, "w") as fh:
    for r in rows:
        fh.write("\t".join(map(str, r)) + "\n")

by_pkg = defaultdict(lambda: [0, 0, 0])
by_group = defaultdict(lambda: [0, 0, 0])
for pkg, group, rel, lines, stub, *_ in rows:
    for d, key in ((by_pkg, pkg), (by_group, group)):
        d[key][0] += 1; d[key][1] += lines; d[key][2] += stub

md = ["# NocoDB 逐文件清点", "",
      "由同目录的 `nocodb_files.py` 生成（`python3 -I nocodb_files.py upstream OUT.tsv nocodb-files.md`），"
      "覆盖 `packages/` 下全部 ts/vue/js 源文件（测试、类型声明、配置与构建产物除外）。每个文件都被读入并检查："
      "行数、导出名、头部注释、是否为社区版空壳（小文件且方法体都是 `..._args`/`return null`/`() => {}` 一类）。"
      "人工结论见 [inventory.md](inventory.md) 与 [README](README.md)。", "",
      "## 按包", "", "| 包 | 文件 | 行数 | 空壳 |", "| --- | ---: | ---: | ---: |"]
for k, v in sorted(by_pkg.items(), key=lambda x: -x[1][1]):
    md.append(f"| `{k}` | {v[0]} | {v[1]} | {v[2]} |")
tot = [sum(v[i] for v in by_pkg.values()) for i in range(3)]
md.append(f"| 合计 | {tot[0]} | {tot[1]} | {tot[2]} |")
md += ["", "## 按目录（前三级）", "", "| 目录 | 文件 | 行数 | 空壳 |", "| --- | ---: | ---: | ---: |"]
for k, v in sorted(by_group.items()):
    md.append(f"| `{k}` | {v[0]} | {v[1]} | {v[2]} |")
md += ["", "## 社区版空壳（全部）", "", "| 文件 | 行数 | 头注释 |", "| --- | ---: | --- |"]
for pkg, group, rel, lines, stub, topics, exports, head in rows:
    if stub:
        md.append(f"| `{rel}` | {lines} | {head} |")
md += ["", "## 与 Noema 相关主题的文件（AI、Markdown、工作流、MCP、Agent/Chat）", "",
       "| 文件 | 行数 | 主题 | 空壳 | 导出 | 头注释 |", "| --- | ---: | --- | :---: | --- | --- |"]
for pkg, group, rel, lines, stub, topics, exports, head in rows:
    t = set(topics.split(",")) - {""} - {"view/grid"}
    if t:
        md.append(f"| `{rel}` | {lines} | {','.join(sorted(t))} | {'✓' if stub else ''} | {exports} | {head} |")
with open(out_md, "w") as fh:
    fh.write("\n".join(md) + "\n")
print(f"files {tot[0]} lines {tot[1]} stubs {tot[2]}")
