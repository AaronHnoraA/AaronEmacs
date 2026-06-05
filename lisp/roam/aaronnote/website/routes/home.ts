// "/" — the editor route. Loads README.md into a single live editor.
// Uses the public `createEditor` API end-to-end (dogfooding the npm
// surface from our own home page).

import { createEditor } from "../../src/lib.ts";
import { mountNav } from "../components/nav.ts";
import readme from "../../README.md?raw";

export function homeRoute(root: HTMLElement): () => void {
  mountNav(root, "/");

  const main = document.createElement("main");
  main.className = "page page-home";
  main.innerHTML = `
    <section class="hero-editor"></section>
    <p class="route-footer">
      The text above is editable through the CM6 editor surface.
    </p>
  `;
  root.append(main);

  const host = main.querySelector(".hero-editor") as HTMLElement;
  const editor = createEditor(host, { initialContent: readme });

  return () => {
    editor.destroy();
  };
}
