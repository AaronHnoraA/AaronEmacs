#!/usr/bin/env python3
from __future__ import annotations

from dataclasses import dataclass, field
import json
from pathlib import Path
import os
import re
import random
import subprocess
from urllib.parse import unquote


ROOT = Path(__file__).resolve().parents[2]
AGENT_DIR = ROOT / "agent"
ROAM_DIR = ROOT / "roam"
INDEX_DIR = AGENT_DIR / "index"
WIKI_DIR = AGENT_DIR / "wiki"
WIKI_NOTES_DIR = WIKI_DIR / "notes"
STATE_DIR = AGENT_DIR / ".state"
STATE_FILE = STATE_DIR / "maintain-state.json"
DEFAULT_SAMPLE_RATIO = 0.15
DEFAULT_MIN_SAMPLE = 1
SKIP_NOTE_DIR_NAMES = {
    ".git",
    ".git-rewrite",
    ".cache",
    ".direnv",
    ".venv",
    "node_modules",
    "public",
    "release",
    "ltximg",
    "_typst",
    "__pycache__",
}

HEADING_RE = re.compile(r"^(#{1,6})\s+(.+)$")
FRONT_MATTER_RE = re.compile(r"\A---\s*\n(.*?)\n---\s*\n?", re.S)
ORG_META_RE = re.compile(r"\A#\+begin(?:_|\s+)meta\s*\n(.*?)^#\+end(?:_|\s+)meta\s*\n?", re.S | re.M)
MD_LINK_RE = re.compile(r"(?<!!)\[[^\]]+\]\(([^)]+)\)")


@dataclass
class Note:
    path: Path
    rel_path: str
    wiki_path: Path
    id: str = ""
    title: str = ""
    date: str = ""
    tags: list[str] = field(default_factory=list)
    headings: list[tuple[int, str]] = field(default_factory=list)
    summary: str = ""
    outgoing_refs: list[str] = field(default_factory=list)
    outgoing_ids: list[str] = field(default_factory=list)

    def to_state(self) -> dict[str, object]:
        return {
            "path": self.path.as_posix(),
            "rel_path": self.rel_path,
            "wiki_path": self.wiki_path.as_posix(),
            "id": self.id,
            "title": self.title,
            "date": self.date,
            "tags": self.tags,
            "headings": self.headings,
            "summary": self.summary,
            "outgoing_refs": self.outgoing_refs,
            "outgoing_ids": self.outgoing_ids,
        }

    @classmethod
    def from_state(cls, data: dict[str, object]) -> "Note":
        path = Path(str(data["path"]))
        return cls(
            path=path,
            rel_path=str(data["rel_path"]),
            wiki_path=wiki_path_for(path),
            id=str(data.get("id", "")),
            title=str(data.get("title", "")),
            date=str(data.get("date", "")),
            tags=[str(tag) for tag in data.get("tags", [])],
            headings=[(int(level), str(title)) for level, title in data.get("headings", [])],
            summary=str(data.get("summary", "")),
            outgoing_refs=[str(item) for item in data.get("outgoing_refs", [])],
            outgoing_ids=[str(item) for item in data.get("outgoing_ids", [])],
        )


def clean_markdown_markup(text: str) -> str:
    text = re.sub(r"!\[[^\]]*\]\([^)]+\)", " ", text)
    text = re.sub(r"\[([^\]]+)\]\([^)]+\)", r"\1", text)
    text = re.sub(r"</?[^>]+>", " ", text)
    text = text.replace("\\_", "_")
    text = re.sub(r"[#>*_`~$|]", " ", text)
    text = re.sub(r"^\s*[-+]\s+", "", text)
    return " ".join(text.split())


def unique(values: list[str]) -> list[str]:
    seen: set[str] = set()
    result: list[str] = []
    for value in values:
        if value and value not in seen:
            seen.add(value)
            result.append(value)
    return result


def wiki_path_for(note_path: Path) -> Path:
    return WIKI_NOTES_DIR / note_path.relative_to(ROAM_DIR).with_suffix(".md")


def indexable_note_path(path: Path) -> bool:
    directories = path.relative_to(ROAM_DIR).parts[:-1]
    return not any(part.startswith(".") or part in SKIP_NOTE_DIR_NAMES for part in directories)


def parse_metadata_block(text: str) -> tuple[dict[str, object], str]:
    match = ORG_META_RE.match(text)
    if match:
        return parse_meta_lines(match.group(1), org_style=True), text[match.end():]
    match = FRONT_MATTER_RE.match(text)
    if match:
        return parse_meta_lines(match.group(1), org_style=False), text[match.end():]
    return {}, text


def parse_meta_lines(raw: str, org_style: bool) -> dict[str, object]:
    metadata: dict[str, object] = {}
    current_list: str | None = None
    for raw_line in raw.splitlines():
        line = raw_line.rstrip()
        item = re.match(r"^\s*-\s*(.+?)\s*$", line)
        if item and current_list:
            metadata.setdefault(current_list, [])
            assert isinstance(metadata[current_list], list)
            metadata[current_list].append(parse_yaml_scalar(item.group(1)))
            continue
        pair = re.match(r"^([A-Za-z0-9_-]+):(?:\s*(.*))?$", line)
        if not pair:
            continue
        key, value = pair.group(1), (pair.group(2) or "").strip()
        if value == "":
            metadata[key] = []
            current_list = key
        elif org_style and key in {"tags", "aliases", "refs"}:
            metadata[key] = [normalize_meta_string(part) for part in value.split(",") if part.strip()]
            current_list = None
        else:
            metadata[key] = parse_yaml_scalar(value)
            current_list = None
    return metadata


def normalize_meta_string(value: object) -> str:
    return str(value).strip().strip('"').replace("\\_", "_")


def parse_yaml_scalar(value: str) -> object:
    value = value.strip()
    if value in {"true", "false"}:
        return value == "true"
    if value.startswith('"') and value.endswith('"'):
        return value[1:-1].replace('\\"', '"').replace("\\\\", "\\").replace("\\_", "_")
    return value.replace("\\_", "_")


def metadata_list(metadata: dict[str, object], key: str) -> list[str]:
    value = metadata.get(key, [])
    if isinstance(value, list):
        return [normalize_meta_string(item) for item in value if normalize_meta_string(item)]
    if isinstance(value, str):
        return [normalize_meta_string(part) for part in value.split(",") if part.strip()]
    return []


def parse_note(path: Path) -> Note:
    rel_path = path.relative_to(ROOT).as_posix()
    text = path.read_text(encoding="utf-8")
    metadata, body = parse_metadata_block(text)
    lines = body.splitlines()
    note = Note(path=path, rel_path=rel_path, wiki_path=wiki_path_for(path))
    note.id = str(metadata.get("id", ""))
    note.title = str(metadata.get("title", ""))
    note.date = str(metadata.get("date", ""))
    note.tags = metadata_list(metadata, "tags")

    summary_done = False
    summary_parts: list[str] = []
    outgoing_refs = metadata_list(metadata, "refs") + [ref.strip() for ref in MD_LINK_RE.findall(body)]
    in_code_fence = False

    for line in lines:
        stripped = line.strip()

        if stripped.startswith("```") or stripped.startswith("~~~"):
            in_code_fence = not in_code_fence
            continue
        if in_code_fence:
            continue

        heading_match = HEADING_RE.match(line)
        if heading_match:
            level = len(heading_match.group(1))
            title = clean_markdown_markup(heading_match.group(2))
            note.headings.append((level, title))

        if summary_done:
            continue
        if not stripped:
            continue
        if stripped.startswith("#+begin") or stripped.startswith("#+end"):
            continue
        if heading_match:
            cleaned_heading = clean_markdown_markup(heading_match.group(2))
            if cleaned_heading:
                summary_parts.append(cleaned_heading)
            continue

        cleaned = clean_markdown_markup(stripped)
        if cleaned:
            summary_parts.append(cleaned)
        if len(" ".join(summary_parts)) >= 420:
            summary_done = True

    note.title = note.title or path.stem.replace("_", " ").title()
    note.summary = truncate(" ".join(summary_parts), 420)
    note.outgoing_refs = unique(outgoing_refs)
    return note


def truncate(text: str, limit: int) -> str:
    if len(text) <= limit:
        return text
    return text[: limit - 1].rstrip() + "..."


def md_escape(text: str) -> str:
    return text.replace("|", "\\|").replace("\n", " ")


def rel_link(from_file: Path, to_file: Path) -> str:
    return os.path.relpath(to_file, start=from_file.parent).replace(os.sep, "/")


def note_link(from_file: Path, note: Note) -> str:
    return f"[{note.title}]({rel_link(from_file, note.wiki_path)})"


def source_link(from_file: Path, note: Note) -> str:
    return f"[{note.rel_path}]({rel_link(from_file, note.path)})"


def split_markdown_ref(raw_ref: str) -> str:
    ref = raw_ref.strip()
    if ref.startswith("<") and ref.endswith(">"):
        ref = ref[1:-1]
    return unquote(ref.split("#", 1)[0].strip())


def external_ref_p(ref: str) -> bool:
    return bool(re.match(r"^[a-z][a-z0-9+.-]*:", ref, flags=re.I))


def resolve_markdown_links(notes: list[Note]) -> None:
    by_id = {note.id: note for note in notes if note.id}
    by_rel_path = {note.rel_path: note for note in notes}
    for note in notes:
        outgoing_ids: list[str] = []
        for raw_ref in note.outgoing_refs:
            if raw_ref in by_id:
                outgoing_ids.append(raw_ref)
                continue
            ref = split_markdown_ref(raw_ref)
            if not ref or external_ref_p(ref):
                continue
            ref_path = (note.path.parent / ref).resolve()
            try:
                rel_path = ref_path.relative_to(ROOT).as_posix()
            except ValueError:
                continue
            target = by_rel_path.get(rel_path)
            if target and target.id != note.id:
                outgoing_ids.append(target.id)
        note.outgoing_ids = unique(outgoing_ids)


def build_backlinks(notes: list[Note]) -> dict[str, list[Note]]:
    by_id = {note.id: note for note in notes if note.id}
    backlinks: dict[str, list[Note]] = {note.id: [] for note in notes if note.id}
    for note in notes:
        for target_id in note.outgoing_ids:
            if target_id in by_id and note not in backlinks[target_id]:
                backlinks[target_id].append(note)
    return backlinks


def write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content.rstrip() + "\n", encoding="utf-8")


def write_if_changed(path: Path, content: str) -> bool:
    normalized = content.rstrip() + "\n"
    if path.exists() and path.read_text(encoding="utf-8") == normalized:
        return False
    write(path, normalized)
    return True


def current_head() -> str:
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return ""
    return result.stdout.strip()


def load_state() -> dict[str, object]:
    if not STATE_FILE.exists():
        return {}
    data = json.loads(STATE_FILE.read_text(encoding="utf-8"))
    return data if isinstance(data, dict) else {}


def load_note_cache() -> dict[str, dict[str, object]]:
    state = load_state()
    notes = state.get("notes", {})
    return notes if isinstance(notes, dict) else {}


def save_state(notes: list[Note], fingerprints: dict[str, dict[str, int]], last_head: str) -> None:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    payload = {
        "last_head": last_head,
        "notes": {
            note.rel_path: {
                "fingerprint": fingerprints[note.rel_path],
                "note": note.to_state(),
            }
            for note in notes
        }
    }
    write(STATE_FILE, json.dumps(payload, indent=2, sort_keys=True))


def fingerprint(path: Path) -> dict[str, int]:
    stat = path.stat()
    return {"mtime_ns": stat.st_mtime_ns, "size": stat.st_size}


def sample_ratio() -> float:
    raw = os.environ.get("AGENT_MAINTAIN_SAMPLE_RATIO", "").strip()
    if not raw:
        return DEFAULT_SAMPLE_RATIO
    try:
        value = float(raw)
    except ValueError:
        return DEFAULT_SAMPLE_RATIO
    return max(0.0, min(1.0, value))


def min_sample() -> int:
    raw = os.environ.get("AGENT_MAINTAIN_MIN_SAMPLE", "").strip()
    if not raw:
        return DEFAULT_MIN_SAMPLE
    try:
        value = int(raw)
    except ValueError:
        return DEFAULT_MIN_SAMPLE
    return max(0, value)


def choose_sample(paths: list[Path]) -> set[Path]:
    if not paths:
        return set()
    count = max(min_sample(), int(len(paths) * sample_ratio()))
    count = min(len(paths), count)
    if count == 0:
        return set()
    return set(random.sample(paths, count))


def parse_changed_paths(output: str) -> set[str]:
    changed: set[str] = set()
    for raw_line in output.splitlines():
        line = raw_line.rstrip()
        if not line:
            continue
        path_text = line[3:] if len(line) > 3 and line[1] == " " else line
        if " -> " in path_text:
            path_text = path_text.split(" -> ", 1)[1]
        if not path_text.endswith(".md"):
            continue
        if not path_text.startswith("roam/"):
            continue
        changed.add(path_text)
    return changed


def git_changed_note_paths(last_head: str) -> set[str]:
    changed: set[str] = set()

    if last_head:
        try:
            committed = subprocess.run(
                ["git", "diff", "--name-only", f"{last_head}..HEAD", "--", "roam"],
                cwd=ROOT,
                check=True,
                capture_output=True,
                text=True,
            )
            changed.update(parse_changed_paths(committed.stdout))
        except (OSError, subprocess.CalledProcessError):
            pass

    try:
        working = subprocess.run(
            ["git", "status", "--short", "--untracked-files=all", "--", "roam"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return changed

    changed.update(parse_changed_paths(working.stdout))
    return changed


def load_notes(paths: list[Path]) -> tuple[list[Note], list[str], int]:
    state = load_state()
    previous = load_note_cache()
    path_by_rel = {path.relative_to(ROOT).as_posix(): path for path in paths}
    unchanged_candidates: list[Path] = []
    notes_by_rel: dict[str, Note] = {}
    fingerprints: dict[str, dict[str, int]] = {}
    reparsed = 0
    last_head = str(state.get("last_head", ""))
    git_changed = git_changed_note_paths(last_head)
    head = current_head()

    for rel_path, path in path_by_rel.items():
        current_fp = fingerprint(path)
        fingerprints[rel_path] = current_fp
        previous_entry = previous.get(rel_path)
        changed_in_git = rel_path in git_changed
        if (
            not changed_in_git
            and previous_entry
            and isinstance(previous_entry, dict)
            and previous_entry.get("fingerprint") == current_fp
            and "note" in previous_entry
        ):
            unchanged_candidates.append(path)
        else:
            notes_by_rel[rel_path] = parse_note(path)
            reparsed += 1

    sampled = choose_sample(unchanged_candidates)
    for path in unchanged_candidates:
        rel_path = path.relative_to(ROOT).as_posix()
        previous_entry = previous[rel_path]
        if path in sampled:
            notes_by_rel[rel_path] = parse_note(path)
            reparsed += 1
        else:
            notes_by_rel[rel_path] = Note.from_state(previous_entry["note"])

    deleted = sorted(set(previous) - set(path_by_rel))
    notes = sorted(notes_by_rel.values(), key=lambda note: note.rel_path.lower())
    resolve_markdown_links(notes)
    save_state(notes, fingerprints, head)
    return notes, deleted, reparsed


def render_index_readme(notes: list[Note]) -> str:
    return "\n".join(
        [
            "# Agent Index",
            "",
            "Generated by `python3 agent/skill/maintain.py`.",
            "",
            f"- Notes indexed: {len(notes)}",
            "- Main index: `markdown-note-index.md`",
            "- Title index: `titles.md`",
            "- Path index: `paths.md`",
            "- Tag index: `tags.md`",
            "- Link graph: `graph.md`",
            "- Backlink index: `backlinks.md`",
            "- Sources covered: `roam/`",
        ]
    )


def render_markdown_note_index(notes: list[Note], backlinks: dict[str, list[Note]]) -> str:
    lines = [
        "# Markdown Note Index",
        "",
        "| Title | Path | Tags | Summary |",
        "| --- | --- | --- | --- |",
    ]
    index_file = INDEX_DIR / "markdown-note-index.md"
    for note in notes:
        tags = ", ".join(note.tags)
        lines.append(
            f"| {md_escape(note.title)} | {source_link(index_file, note)} | {md_escape(tags)} | {md_escape(note.summary)} |"
        )

    lines.extend(["", "## Note Links", ""])
    by_id = {note.id: note for note in notes if note.id}
    for note in notes:
        outgoing = [by_id[target_id] for target_id in note.outgoing_ids if target_id in by_id]
        incoming = backlinks.get(note.id, []) if note.id else []
        lines.append(f"### {note.title}")
        lines.append(f"- Source: {source_link(index_file, note)}")
        lines.append(f"- Wiki: {note_link(index_file, note)}")
        lines.append(
            "- Outgoing: "
            + (", ".join(note_link(index_file, target) for target in outgoing) if outgoing else "None")
        )
        lines.append(
            "- Backlinks: "
            + (", ".join(note_link(index_file, source) for source in incoming) if incoming else "None")
        )
        lines.append("")
    return "\n".join(lines)


def render_tags(notes: list[Note]) -> str:
    tags: dict[str, list[Note]] = {}
    for note in notes:
        for tag in note.tags:
            tags.setdefault(tag, []).append(note)

    tag_file = INDEX_DIR / "tags.md"
    lines = ["# Tags", ""]
    for tag in sorted(tags, key=str.lower):
        lines.append(f"## {tag}")
        for note in sorted(tags[tag], key=lambda item: item.title.lower()):
            lines.append(f"- {note_link(tag_file, note)}")
        lines.append("")
    return "\n".join(lines)


def render_titles(notes: list[Note]) -> str:
    title_file = INDEX_DIR / "titles.md"
    lines = [
        "# Title Index",
        "",
        "| Title | Path | Wiki |",
        "| --- | --- | --- |",
    ]
    for note in sorted(notes, key=lambda item: (item.title.lower(), item.rel_path.lower())):
        lines.append(
            f"| {md_escape(note.title)} | {source_link(title_file, note)} | {note_link(title_file, note)} |"
        )
    return "\n".join(lines)


def render_paths(notes: list[Note]) -> str:
    path_file = INDEX_DIR / "paths.md"
    lines = [
        "# Path Index",
        "",
        "| Path | Title | Wiki |",
        "| --- | --- | --- |",
    ]
    for note in sorted(notes, key=lambda item: item.rel_path.lower()):
        lines.append(
            f"| {md_escape(note.rel_path)} | {md_escape(note.title)} | {note_link(path_file, note)} |"
        )
    return "\n".join(lines)


def render_graph(notes: list[Note], backlinks: dict[str, list[Note]]) -> str:
    graph_file = INDEX_DIR / "graph.md"
    by_id = {note.id: note for note in notes if note.id}
    lines = ["# Link Graph", "", "## Edges", ""]
    edge_count = 0
    for note in notes:
        for target_id in note.outgoing_ids:
            target = by_id.get(target_id)
            if not target:
                continue
            lines.append(f"- {note_link(graph_file, note)} -> {note_link(graph_file, target)}")
            edge_count += 1
    if edge_count == 0:
        lines.append("- None")

    lines.extend(["", "## Backlink Counts", ""])
    for note in notes:
        count = len(backlinks.get(note.id, [])) if note.id else 0
        lines.append(f"- {note_link(graph_file, note)}: {count}")
    return "\n".join(lines)


def render_backlinks(notes: list[Note], backlinks: dict[str, list[Note]]) -> str:
    backlink_file = INDEX_DIR / "backlinks.md"
    lines = [
        "# Backlink Index",
        "",
        "| Title | Backlinks | Sources |",
        "| --- | --- | --- |",
    ]
    ranked_notes = sorted(
        notes,
        key=lambda item: (-len(backlinks.get(item.id, [])) if item.id else 0, item.title.lower(), item.rel_path.lower()),
    )
    for note in ranked_notes:
        incoming = backlinks.get(note.id, []) if note.id else []
        linked_sources = ", ".join(note_link(backlink_file, source) for source in incoming) if incoming else "None"
        lines.append(
            f"| {md_escape(note.title)} | {len(incoming)} | {linked_sources} |"
        )
    return "\n".join(lines)


def render_wiki_readme(notes: list[Note]) -> str:
    wiki_file = WIKI_DIR / "README.md"
    lines = [
        "# Agent Wiki",
        "",
        "Condensed Markdown views generated from `roam/**/*.md`. These files are for fast AI lookup; edit the source Markdown files instead.",
        "",
        "## Notes",
        "",
    ]
    for note in notes:
        lines.append(f"- {note_link(wiki_file, note)}")
    return "\n".join(lines)


def render_wiki_note(note: Note, notes_by_id: dict[str, Note], backlinks: dict[str, list[Note]]) -> str:
    lines = [
        f"# {note.title}",
        "",
        f"- Source: {source_link(note.wiki_path, note)}",
        f"- ID: `{note.id or 'missing'}`",
        f"- Date: {note.date or 'unknown'}",
        f"- Tags: {', '.join(note.tags) if note.tags else 'None'}",
        "",
        "## Summary",
        "",
        note.summary or "No body summary found.",
        "",
        "## Structure",
        "",
    ]

    if note.headings:
        for level, heading in note.headings[:24]:
            indent = "  " * max(level - 1, 0)
            lines.append(f"{indent}- {heading}")
    else:
        lines.append("- No headings found.")

    lines.extend(["", "## Links", ""])
    outgoing = [notes_by_id[target_id] for target_id in note.outgoing_ids if target_id in notes_by_id]
    if outgoing:
        for target in outgoing:
            lines.append(f"- {note_link(note.wiki_path, target)}")
    else:
        lines.append("- None")

    lines.extend(["", "## Backlinks", ""])
    incoming = backlinks.get(note.id, []) if note.id else []
    if incoming:
        for source in incoming:
            lines.append(f"- {note_link(note.wiki_path, source)}")
    else:
        lines.append("- None")

    return "\n".join(lines)


def main() -> None:
    INDEX_DIR.mkdir(parents=True, exist_ok=True)
    WIKI_DIR.mkdir(parents=True, exist_ok=True)
    WIKI_NOTES_DIR.mkdir(parents=True, exist_ok=True)

    note_paths = sorted(path for path in ROAM_DIR.rglob("*.md") if indexable_note_path(path))
    notes, deleted, reparsed = load_notes(note_paths)
    notes_by_id = {note.id: note for note in notes if note.id}
    backlinks = build_backlinks(notes)

    for rel_path in deleted:
        rel_note_path = Path(rel_path)
        if rel_note_path.is_relative_to(Path("roam")):
            stale_wiki = WIKI_NOTES_DIR / rel_note_path.relative_to("roam").with_suffix(".md")
        else:
            stale_wiki = WIKI_NOTES_DIR / rel_note_path.with_suffix(".md")
        if stale_wiki.exists():
            stale_wiki.unlink()

    write_if_changed(INDEX_DIR / "README.md", render_index_readme(notes))
    legacy_index = INDEX_DIR / "typst-note-index.md"
    if legacy_index.exists():
        legacy_index.unlink()
    write_if_changed(INDEX_DIR / "markdown-note-index.md", render_markdown_note_index(notes, backlinks))
    write_if_changed(INDEX_DIR / "titles.md", render_titles(notes))
    write_if_changed(INDEX_DIR / "paths.md", render_paths(notes))
    write_if_changed(INDEX_DIR / "tags.md", render_tags(notes))
    write_if_changed(INDEX_DIR / "graph.md", render_graph(notes, backlinks))
    write_if_changed(INDEX_DIR / "backlinks.md", render_backlinks(notes, backlinks))
    write_if_changed(WIKI_DIR / "README.md", render_wiki_readme(notes))

    for note in notes:
        write_if_changed(note.wiki_path, render_wiki_note(note, notes_by_id, backlinks))

    expected_wiki_paths = {note.wiki_path.resolve() for note in notes}
    for path in WIKI_NOTES_DIR.rglob("*.md"):
        if path.resolve() not in expected_wiki_paths:
            path.unlink()

    print(
        "Indexed "
        f"{len(notes)} Markdown notes into {INDEX_DIR.relative_to(ROOT)} and {WIKI_DIR.relative_to(ROOT)}. "
        f"Reparsed {reparsed} notes; reused cache for {len(notes) - reparsed}; removed {len(deleted)} deleted notes."
    )


if __name__ == "__main__":
    main()
