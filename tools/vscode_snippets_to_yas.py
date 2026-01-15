#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import json
import re
import sys
from pathlib import Path

# VSCode scope / language id -> Emacs major-mode folder name
SCOPE_TO_MODE = {
    "c": "c-mode",
    "cpp": "c++-mode",
    "c++": "c++-mode",
    "lua": "lua-mode",
    "python": "python-mode",
    "javascript": "js-mode",
    "typescript": "typescript-mode",
    "json": "json-mode",
    "sh": "sh-mode",
    "bash": "sh-mode",
    "zsh": "sh-mode",
    "go": "go-mode",
    "rust": "rust-mode",
    "java": "java-mode",
    "latex": "latex-mode",
    "tex": "tex-mode",
    "markdown": "markdown-mode",
    "md": "markdown-mode",
    "yaml": "yaml-mode",
    "yml": "yaml-mode",
}

def ensure_dir(p: Path) -> None:
    p.mkdir(parents=True, exist_ok=True)

def safe_filename(s: str) -> str:
    s = s.strip()
    s = re.sub(r"[^\w.\-+]+", "_", s, flags=re.UNICODE)
    s = re.sub(r"_+", "_", s)
    return s[:120] if s else "snippet"

def as_list(x):
    if x is None:
        return []
    if isinstance(x, list):
        return x
    return [x]

def normalize_body(body) -> str:
    # VSCode: body can be string or list of strings
    if isinstance(body, list):
        return "\n".join(str(line) for line in body)
    return str(body) if body is not None else ""

def parse_scope(scope_field):
    """
    VSCode: "scope": "lua,markdown" or "lua" etc.
    Return list of scopes.
    """
    if not scope_field:
        return []
    if isinstance(scope_field, list):
        scopes = []
        for x in scope_field:
            scopes.extend([s.strip() for s in str(x).split(",") if s.strip()])
        return scopes
    return [s.strip() for s in str(scope_field).split(",") if s.strip()]

def mode_from_scopes(scopes):
    """
    Choose the first scope we can map; else None.
    """
    for sc in scopes:
        if sc in SCOPE_TO_MODE:
            return SCOPE_TO_MODE[sc]
    return None

def mode_from_filename(stem: str):
    """
    If file is named like c.code-snippets / cpp.code-snippets / lua.json etc.
    use that to infer.
    """
    key = stem.lower()
    return SCOPE_TO_MODE.get(key)

def write_yas_snippet(out_dir: Path, key: str, name: str, body: str, description: str = ""):
    ensure_dir(out_dir)

    # Prefer key + name to avoid collisions
    fname = safe_filename(f"{key}__{name}" if name else key)
    path = out_dir / fname

    lines = [
        "# -*- mode: snippet -*-",
        f"# name: {name or key}",
        f"# key: {key}",
    ]
    if description:
        lines.append(f"# -- {description}")
    lines.append("# --")
    lines.append(body)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")

def convert_file(src: Path, out_root: Path):
    data = json.loads(src.read_text(encoding="utf-8"))

    # Default mode: try from filename; fallback fundamental-mode
    default_mode = mode_from_filename(src.stem) or "fundamental-mode"

    # VSCode file format: { "SnippetName": { "prefix":..., "body":..., "scope":..., ... }, ... }
    for snippet_name, spec in data.items():
        if not isinstance(spec, dict):
            continue

        prefixes = as_list(spec.get("prefix"))
        body = normalize_body(spec.get("body"))
        desc = spec.get("description", "")

        if not prefixes or not body.strip():
            continue

        scopes = parse_scope(spec.get("scope"))
        mode = mode_from_scopes(scopes) or default_mode
        out_dir = out_root / mode

        for pfx in prefixes:
            pfx = str(pfx).strip()
            if not pfx:
                continue
            write_yas_snippet(out_dir, pfx, str(snippet_name), body, str(desc))

def main():
    if len(sys.argv) != 3:
        print("用法: vscode_snippets_to_yas.py <vscode_snippets_dir> <yas_output_root>")
        print("例如: vscode_snippets_to_yas.py ~/.config/emacs/vscode-snippets ~/.config/emacs/snippets")
        sys.exit(1)

    src_dir = Path(sys.argv[1]).expanduser()
    out_root = Path(sys.argv[2]).expanduser()
    ensure_dir(out_root)

    files = list(src_dir.rglob("*.code-snippets")) + list(src_dir.rglob("*.json"))
    # 只处理包含 snippets 的 json：你也可以把目录里其他 json 移走，避免误伤
    # 这里做一个温和过滤：文件过大/不符合结构的会在解析时报错，你能看到错误
    if not files:
        print(f"没找到 snippets 文件: {src_dir}")
        sys.exit(2)

    ok = 0
    for f in files:
        try:
            # 尝试解析并转换
            convert_file(f, out_root)
            ok += 1
        except json.JSONDecodeError:
            # 不是 snippet json，跳过
            continue
        except Exception as e:
            print(f"转换失败: {f}\n  错误: {e}")

    print(f"完成：成功处理 {ok} 个文件。输出目录：{out_root}")

if __name__ == "__main__":
    main()
