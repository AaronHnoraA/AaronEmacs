import "katex/dist/katex.min.css";
import katex from "katex";

type KatexRenderOptions = {
  displayMode?: boolean;
  throwOnError?: boolean;
  strict?: boolean | "error" | "ignore" | "warn";
  trust?: boolean;
  output?: "html" | "mathml" | "htmlAndMathml";
};

export function renderMathLazy(
  tex: string,
  element: HTMLElement,
  options: KatexRenderOptions,
  onError: () => void,
): void {
  const key = `${options.displayMode ? "display" : "inline"}\n${tex}`;
  element.setAttribute("data-math-render-key", key);
  try {
    katex.render(tex, element, options);
  } catch {
    if (element.getAttribute("data-math-render-key") !== key) return;
    onError();
  }
}
