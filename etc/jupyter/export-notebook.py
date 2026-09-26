"""Convert a notebook snapshot without launching or executing its kernel."""
import base64
import json
import mimetypes
from pathlib import Path
import re
import sys
import tempfile
from urllib.parse import unquote


def rewrite_images(source, images):
    """Embed only assets explicitly supplied by the owning filesystem."""
    def markdown(match):
        original = match.group(2)
        name = unquote(original.strip("<>"))
        return match.group(1) + images.get(name, original)

    def html(match):
        return match.group(1) + images.get(unquote(match.group(2)), match.group(2))

    source = re.sub(r'(!\[[^\]\n]*\]\()[ \t]*(<[^>\n]+>|[^\s)\n]+)', markdown, source)
    return re.sub(r'(<img\b[^>]*\bsrc=["\'])([^"\']+)', html, source, flags=re.I)


def convert(request, format_name, output):
    import nbformat
    from nbconvert.exporters import HTMLExporter, PDFExporter, ScriptExporter

    classes = {"html": HTMLExporter, "pdf": PDFExporter, "script": ScriptExporter}
    if format_name not in classes:
        raise ValueError("Unsupported notebook export format")
    notebook = nbformat.from_dict(request["notebook"])
    # Do not let file metadata select arbitrary installed exporter plugins.
    language = notebook.metadata.get("language_info", {})
    if language.get("nbconvert_exporter") not in (None, "python", "script"):
        language.pop("nbconvert_exporter", None)
    with tempfile.TemporaryDirectory(prefix="notebook-export-assets-") as directory:
        root = Path(directory)
        for name, value in request.get("assets", {}).items():
            relative = Path(name)
            if relative.is_absolute() or ".." in relative.parts:
                raise ValueError("Export asset must stay inside the notebook directory")
            file = root / relative
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_bytes(base64.b64decode(value, validate=True))
        exporter = classes[format_name]()
        if format_name == "html":
            # nbconvert's generic embed_images may open absolute client paths
            # from Markdown. Use only the explicit Remote-resolved manifest.
            images = {name: f"data:{mimetypes.guess_type(name)[0] or 'application/octet-stream'};base64,{value}"
                      for name, value in request.get("assets", {}).items()}
            for cell in notebook.cells:
                if cell.cell_type == "markdown":
                    cell.source = rewrite_images(cell.source, images)
        if format_name == "pdf":
            exporter.latex_command = ["xelatex", "{filename}", "-no-shell-escape", "-quiet"]
        body, resources = exporter.from_notebook_node(
            notebook, resources={"metadata": {"path": str(root), "name": request.get("name", "notebook")}})
        Path(output).write_bytes(body if isinstance(body, bytes) else body.encode("utf-8"))
        return {"extension": resources.get("output_extension", exporter.file_extension)}


if __name__ == "__main__":
    try:
        snapshot = json.loads(Path(sys.argv[2]).read_text(encoding="utf-8"))
        print(json.dumps(convert(snapshot, sys.argv[1], sys.argv[3])))
    except Exception as error:
        print(f"Notebook export failed: {error}", file=sys.stderr)
        sys.exit(1)
