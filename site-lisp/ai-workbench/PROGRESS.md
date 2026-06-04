# ai-workbench 进度

## 当前状态

- 已完成 vendor 接管：
  - `vendor/claude-code-ide/`
  - `vendor/codex-cli/`
- `lisp/init-ai-ide.el` 已从 `site-lisp/ai-workbench/vendor/` 加载 Claude/Codex
- 已有统一入口：
  - `M-x ai-workbench`
  - `C-c M-a`
  - `C-c A W`
  - `C-c A w`
  - `C-c A .`
  - `C-c A k`
  - `C-c A i r` / `C-c A i b` / `C-c A i f`

## 已完成

### Session 与上下文

- project-scoped session 状态表
- region / buffer / file 注入
- 文件内引用式工具入口
  - 直接把引用式 prompt 写入当前 Claude/Codex 交互 buffer
  - 代码上下文使用 `@file` / `@line file:line:column` / `@range file:start-line:start-column-end-line:end-column`，不复制源码正文
  - 写入后不自动回车，用户可以继续编辑或手动提交
  - `@本文件`
  - `@本行`
  - `@所选`
  - `@函数`
  - `@块`
  - `@项目`
  - `@Git状态`
  - `@诊断`
- last prompt / last status / last error / run state 记录
- 写作 prompt 入口：
  - `C-c A w` 从当前选区或 buffer 生成写作 prompt
  - 支持润色 / 改写 / 总结 / 翻译 / 提纲 / 续写 / 评论
  - 直接 draft 到当前 Claude/Codex TUI，由用户在后端 buffer 里继续编辑或回车提交

### Backend

- Claude session 入口适配
- Codex session 入口适配
- Codex adapter 已改成和 Claude 同型的实现方式：
  - project -> process 映射
  - 自己创建 terminal session
  - 自己发送 prompt
  - 自己清理 session
  - popup 显示统一复用 `init-vterm-popup.el`
- session 启动时禁止全局 startup `cd` 注入
- 自动注入一次 workdir/profile 提示
- profile 只描述项目工作目录和引用式 prompt 语义，不再注入 manifest 审计协议
- profile 文本已移到 `etc/ai-workbench/profiles/default.txt`
- profile / snippet / prompt template 文本已统一放到 `etc/ai-workbench/`
- Git 规则已调整为审计辅助：允许用 `git status` / `git diff` / Magit diff 做可回溯 review，但禁止未授权的破坏性或写历史操作
- Codex 默认不再走 `exec --json`

### UI / 窗口

- `ai-workbench` 不再停留在中转 frontend
- 当前统一入口直接弹出后端交互 buffer
- popup 显示逻辑改为复用 `init-vterm-popup.el`
- popup header 已加入轻量 tab strip，用于切换当前 popup 池里的 terminal / Claude / Codex buffer
- popup header 已加入 `+term` 按钮，可直接新建 popup vterm

### Diff

- 已从包内移除。
- 是否让 AI 输出 diff、分步确认或停在计划阶段，改由用户在 prompt 中明确要求。

### 会话控制

- 统一 `kill ai-ide` 指令

## 正在推进

- 写作体验继续围绕 Org/Markdown 场景优化
- profile 作为写作 persona 继续增强

## 已知问题

- status buffer 只做工作台概览；写作 prompt 应从 Org/Markdown/text buffer 调用。
- Claude / Codex 的交互 UI 还可以继续压一轮细节。
