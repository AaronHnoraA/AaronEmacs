---
name: rendered-construct
description: Use before adding or changing anything the Noema visual editor draws over Markdown source — a CM6 widget, a Decoration.replace, a block environment, an inline chip, an embedded editor (table cell, MathLive), or a Vim motion that crosses rendered content. Not for Noema work outside the CM6 editor.
---

# Rendered constructs

Code is in `~/.config/emacs/site-lisp/noema/`; the rules are in
`~/.config/emacs/docs/noema/design/rendered-constructs.md`, and its guard
tests reject code that breaks them. Read only the section your change needs:

| Step | Section of rendered-constructs.md |
| --- | --- |
| Choose `object` or `structure`, and a reveal policy | “词汇”, “Reveal policy” |
| Find an existing family to copy | “组件一览” |
| Build an inline, viewport or block family; what never to write | “新增一个渲染组件” |
| Caret, word motion and deletion beside a construct | “左右与词移动”, “删除：object 整体删除” |
| A control inside the widget that edits the document | “从组件写文档：一个入口” |
| When to rebuild, how to patch decorations | “何时重建”, “增量重绘” |
| Markdown inside a widget; embedded editors; tables | “组件内部的 Markdown”, “嵌入式编辑面”, “表格” |
| Up/down motion and Vim screen rows | “上下移动：一个行模型”, “Vim 与屏幕行” |
| Clicks must not move the view | “新增一个渲染组件” → 视口, then `scroll-ownership.md` |
| Tests to add and run | “守卫与测试” |

The syntax the construct draws is listed in `docs/noema/design/syntax.md`;
a new source form takes its row there in the same change.
