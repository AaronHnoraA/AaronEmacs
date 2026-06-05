import { StateField, type EditorState, type Extension } from "@codemirror/state";

type LeanBlockRange = {
  bodyFrom: number;
  bodyTo: number;
};

const OPEN_RE = /^[ \t]*#\+begin[ \t]+lean4(?:[ \t]+[^\n]*)?[ \t]*$/i;
const CLOSE_RE = /^[ \t]*#\+end[ \t]+lean4[ \t]*$/i;

function scanLean4OrgEnvBlocks(state: EditorState): LeanBlockRange[] {
  const doc = state.doc;
  const blocks: LeanBlockRange[] = [];

  for (let lineNumber = 1; lineNumber <= doc.lines; lineNumber++) {
    const open = doc.line(lineNumber);
    if (!OPEN_RE.test(open.text)) continue;

    let depth = 1;
    let closeLineNumber = lineNumber + 1;
    for (; closeLineNumber <= doc.lines; closeLineNumber++) {
      const text = doc.line(closeLineNumber).text;
      if (OPEN_RE.test(text)) depth++;
      if (CLOSE_RE.test(text) && --depth === 0) break;
    }
    if (depth !== 0) continue;

    blocks.push({
      bodyFrom: Math.min(open.to + 1, doc.length),
      bodyTo: doc.line(closeLineNumber).from,
    });
    lineNumber = closeLineNumber;
  }

  return blocks;
}

const lean4OrgEnvBlocksField = StateField.define<LeanBlockRange[]>({
  create: scanLean4OrgEnvBlocks,
  update(value, transaction) {
    return transaction.docChanged ? scanLean4OrgEnvBlocks(transaction.state) : value;
  },
});

export function getLean4OrgEnvBodyRanges(
  state: EditorState,
): { from: number; to: number }[] {
  const blocks = state.field(lean4OrgEnvBlocksField, false)
    ?? scanLean4OrgEnvBlocks(state);
  return blocks.map(({ bodyFrom, bodyTo }) => ({ from: bodyFrom, to: bodyTo }));
}

export const leanExtension: Extension = lean4OrgEnvBlocksField;
