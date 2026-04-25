# ai-workbench 实施计划

## 概述

`ai-workbench` 是一个放在 `site-lisp/` 下的统一 AI 工作台包。目标是在 Emacs 内把 Claude Code 和 Codex CLI 的调用方式收束成更一致的会话、上下文注入、diff 审阅和接受流程。

这个计划文件继续保留，但实现方向已经调整。当前主线不再是中转 frontend，而是：

- `ai-workbench` 作为统一入口
- 直接弹出后端交互 buffer
- popup 显示逻辑复用现有 `init-vterm-popup.el`
- Claude / Codex 都要走同一类“准备 session -> 自动工作目录 -> 自动 profile 注入 -> 直接交互”体验
- profile prompt 从 `etc/ai-workbench/profiles/*.txt` 读取，方便用户直接编辑
- 如果 session 被用户杀掉或失效，下次入口必须重新确认 backend，而不是盲目沿用旧值

## 阶段 1：骨架与文档

- 创建 `site-lisp/ai-workbench/` 包根。
- 创建 workbench 自身模块骨架：
  - `ai-workbench.el`
  - `ai-workbench-session.el`
  - `ai-workbench-compose.el`
  - `ai-workbench-adapter-claude.el`
  - `ai-workbench-adapter-codex.el`
- 创建 vendor 目录占位：
  - `vendor/claude-code-ide/`
  - `vendor/codex-cli/`
- 保持现有 Claude/Codex 工作流不变。

## 阶段 2：vendor 迁移

- 把 `elpa/claude-code-ide` 和 `elpa/codex-cli` 的源码迁移到 `vendor/`。
- 删除 vendor 内的 `.git`、autoload、`.elc` 和其他不需要保留的生成物。
- 保留必要源码、许可证和说明文档。

当前状态：已把两边源码、测试和必要文档复制到 `vendor/`，并已经切换 `lisp/init-ai-ide.el` 从 `site-lisp/ai-workbench/vendor/` 加载。

## 阶段 3：统一 session 与 compose UI

- 引入 project-scoped session 模型。
- 引入普通 Emacs buffer 的 compose 界面，而不是默认依赖 terminal buffer。
- 支持用户在 compose buffer 中手动注入上下文。

当前状态：compose buffer、project-scoped session、output/result/diff buffer 已具备；中转 workbench panel 已降级，不再作为主要交互面。

## 阶段 4：Claude/Codex adapter 融合

- Claude 路径复用现有 MCP、Emacs tools、IDE diff 能力。
- Codex 路径最终目标是交互式体验优先，不以 `exec --json` 作为默认主路径。
- 抽出统一 adapter 接口，避免 UI 层直接依赖单一后端。

当前状态：

- Claude 继续复用现有 MCP/emacs tools。
- Codex 默认已切回交互 terminal session；`exec` 仅保留为可切换路径，不再是默认。
- Claude / Codex 打开 buffer 时都改为复用 `init-vterm-popup.el` 的 popup 窗口逻辑。
- session 启动时都会显式禁止全局 startup `cd` 注入，避免把 `cd ...` 误发给模型输入框。
- profile/workdir 注入已支持一次性自动发送。

## 阶段 5：入口切换与文档更新

- 调整 `lisp/init-ai-ide.el`，从新包加载统一入口。
- 保留兼容入口或旧快捷键映射，降低迁移成本。
- 更新 AI 相关文档，说明新的入口、结构和工作流。

当前状态：已完成入口切换，并新增以下统一入口：

- `C-c M-a`：文件内引用式工具入口
- `C-c A w`：workbench
- `C-c A .`：文件内引用式工具入口
- `C-c A k`：kill 当前 AI 会话
- `C-c A m`：compose
- `C-c A i r` / `C-c A i b` / `C-c A i f`：直接发送 region / buffer / file

## 下一步

- 文件内快捷入口：
  - 绑定一个高频快捷键，在文件里直接弹出工具选择框。
  - 不再经过独立 prompt 输入 buffer；生成后直接写入当前 Claude/Codex 交互 buffer。
  - 写入后不自动回车，用户可以在智能体 buffer 里继续编辑或手动提交。
  - 工具采用“引用式上下文”，例如 `@本文件`、`@本行`、`@所选`、`@函数`、`@块`、`@项目`、`@Git状态`、`@诊断`。
  - 生成 prompt 时不复制源码正文，而是插入结构化引用描述，交给 Claude/Codex 结合 Emacs 工作目录和工具能力理解。
- 变更审阅：
  - 不再内建 snapshot / manifest / diff 审阅流程。
  - 是否让 AI 先做计划、给 diff、等待确认，交给用户在 prompt 中明确要求。
- 会话控制：
  - 提供统一 kill 指令，能够杀掉当前项目的 Claude/Codex 会话并清掉选择状态。
- 稳定性：
  - 修正 Claude popup 中 `project-root` 空变量错误。
  - 修正 Codex vterm buffer 里错误切 major mode 的问题。
