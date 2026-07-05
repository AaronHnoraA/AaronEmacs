export type MarkdownCellFactory<TOptions, TCell> = {
  createMarkdownCell(options: TOptions): TCell;
};

const patchedFactories = new WeakSet<object>();

export function replaceMarkdownCellFactory<TOptions, TCell>(
  factory: MarkdownCellFactory<TOptions, TCell>,
  createCell: (options: TOptions) => TCell,
): boolean {
  if (patchedFactories.has(factory)) return false;
  factory.createMarkdownCell = createCell;
  patchedFactories.add(factory);
  return true;
}
