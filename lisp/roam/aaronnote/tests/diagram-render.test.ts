import { describe, expect, test } from "@voidzero-dev/vite-plus-test";

import { enableDiagramInteraction, normalizeMermaidSource, staticAaronMindmap } from "../src/diagram-render.ts";

describe("diagram render helpers", () => {
  test("keeps full Mermaid source unchanged for marmind fences", () => {
    expect(normalizeMermaidSource("graph LR\nA --- B", "marmind"))
      .toBe("graph LR\nA --- B");
  });

  test("adds mindmap header for plain marmind trees", () => {
    expect(normalizeMermaidSource("Root\n  Branch\n    Detail", "marmind"))
      .toBe("mindmap\n  Root\n    Branch\n      Detail");
  });

  test("keeps empty marmind fences empty", () => {
    expect(normalizeMermaidSource("   \n", "marmind")).toBe("");
  });

  test("accepts Markdown-ish list trees in marmind fences", () => {
    expect(normalizeMermaidSource("- Root\n  - Branch\n    - Detail", "marmind"))
      .toBe("mindmap\n  Root\n    Branch\n      Detail");
  });

  test("keeps ordered list markers in marmind labels", () => {
    expect(normalizeMermaidSource("1. Root\n  2) Branch", "markmind"))
      .toBe("mindmap\n  1. Root\n    2) Branch");
  });

  test("keeps Aaron mindmap fences static while Mermaid mindmaps stay generic", () => {
    expect(staticAaronMindmap("marmind")).toBe(true);
    expect(staticAaronMindmap("markmind")).toBe(true);
    expect(staticAaronMindmap("mindmap")).toBe(false);
    expect(staticAaronMindmap("mermaid")).toBe(false);
  });

  test("enables diagram interaction without toolbar chrome and lets nodes be selected", () => {
    const div = document.createElement("div");
    div.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg"><g id="node-a"><text>Root</text></g></svg>';

    enableDiagramInteraction(div);
    div.querySelector("text")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));

    expect(div.classList.contains("cm-diagram-interactive")).toBe(true);
    expect(div.querySelector(".cm-diagram-toolbar")).toBeNull();
    expect(div.querySelector("#node-a")?.classList.contains("cm-diagram-selected")).toBe(true);
  });

  test("drags diagrams by translating the svg", () => {
    const div = document.createElement("div");
    div.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg"><g id="node-a"><text>Root</text></g></svg>';
    const svg = div.querySelector<SVGSVGElement>("svg")!;

    enableDiagramInteraction(div);
    svg.dispatchEvent(new MouseEvent("pointerdown", { bubbles: true, cancelable: true, button: 0, clientX: 10, clientY: 20 }));
    div.dispatchEvent(new MouseEvent("pointermove", { bubbles: true, cancelable: true, button: 0, clientX: 28, clientY: 15 }));
    div.dispatchEvent(new MouseEvent("pointerup", { bubbles: true, cancelable: true, button: 0, clientX: 28, clientY: 15 }));

    expect(svg.style.transform).toContain("translate(18px, -5px)");
  });

  test("sanitizes SVG diagram links and dispatches safe links", () => {
    const div = document.createElement("div");
    div.innerHTML = [
      '<svg xmlns="http://www.w3.org/2000/svg">',
      '<a id="ok" href="https://example.com"><text>ok</text></a>',
      '<a id="bad" href="javascript:alert(1)"><text>bad</text></a>',
      "</svg>",
    ].join("");
    let opened = "";
    div.addEventListener("aaronnote:open-url", (event) => {
      event.preventDefault();
      opened = (event as CustomEvent<{ href: string }>).detail.href;
    });

    enableDiagramInteraction(div);
    div.querySelector("#ok text")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));

    expect(div.querySelector("#ok")?.getAttribute("target")).toBe("_blank");
    expect(div.querySelector("#bad")?.hasAttribute("href")).toBe(false);
    expect(opened).toBe("https://example.com");
  });
});
