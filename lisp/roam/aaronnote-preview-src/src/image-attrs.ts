import {
  applyLayoutAttrs,
  layoutClasses,
  layoutFromAttrs,
  layoutStyle,
  type LayoutAlign,
  type LayoutAttrs,
  readLayoutTrailingAttrs,
} from "./layout-attrs.ts";
import { type AttrMap, type TrailingAttrs } from "./attrs-syntax.ts";

export type ImageAlign = LayoutAlign;

export type ImageLayoutAttrs = LayoutAttrs;

export function readImageTrailingAttrs(text: string, from: number): TrailingAttrs | null {
  return readLayoutTrailingAttrs(text, from);
}

export function imageLayoutFromAttrs(attrs: AttrMap): ImageLayoutAttrs {
  return layoutFromAttrs(attrs);
}

export function imageLayoutClasses(layout: ImageLayoutAttrs): string {
  return layoutClasses("image", layout);
}

export function imageLayoutStyle(layout: ImageLayoutAttrs): string {
  return layoutStyle("image", layout);
}

export function applyImageLayout(el: HTMLElement, layout: ImageLayoutAttrs): void {
  applyLayoutAttrs(el, "image", layout);
  el.dataset.aaronnoteImageAlign = layout.align;
  el.dataset.aaronnoteImageWrap = layout.wrap ? "true" : "false";
}
