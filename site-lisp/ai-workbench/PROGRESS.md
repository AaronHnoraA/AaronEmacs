# ai-workbench 进度

## 当前状态

- 已完成 vendor 接管：
  - `vendor/claude-code-ide/`
  - `vendor/codex-cli/`
- `lisp/init-ai-ide.el` 已从 `site-lisp/ai-workbench/vendor/` 加载 Claude/Codex
- 已有统一入口：
  - `M-x ai-workbench`
  - `M-x ai-workbench-compose-buffer`
  - `C-c M-a`
  - `C-c A w`
  - `C-c A .`
  - `C-c A k`
  - `C-c A m`
  - `C-c A i r` / `C-c A i b` / `C-c A i f`

## 已完成

### Session 与上下文

- project-scoped session 状态表
- compose buffer
- region / buffer / file 注入
- 文件内引用式工具入口
  - 直接把引用式 prompt 写入当前 Claude/Codex 交互 buffer
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
- Codex 默认不再走 `exec --json`

### UI / 窗口

- `ai-workbench` 不再停留在中转 frontend
- 当前统一入口直接弹出后端交互 buffer
- popup 显示逻辑改为复用 `init-vterm-popup.el`
- popup header 已加入轻量 tab strip，用于切换当前 popup 池里的 terminal / Claude / Codex buffer
- popup header 已加入 `+term` 按钮，可直接新建 popup vterm
- compose buffer 保留为备用手写 prompt 工具；主路径不再依赖 compose buffer

### Diff

- 已从包内移除。
- 是否让 AI 输出 diff、分步确认或停在计划阶段，改由用户在 prompt 中明确要求。

### 会话控制

- 统一 `kill ai-ide` 指令

## 正在推进

- Codex 交互细节继续向 Claude 当前体验靠拢
- profile 从固定 `default` 变成真正可选

## 已知问题

- profile 默认仍是 `default`，但文本已经可在 `etc/ai-workbench/profiles/default.txt` 里改；selector 还没做。
- Claude / Codex 的交互 UI 还可以继续压一轮细节。
