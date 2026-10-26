# Doom modeline 状态

## 范围

本文定义 Doom modeline 左端的 Evil 状态显示、主题取色和窗口生命周期。
Portal 原生视图的键盘归属按 [窗口生命周期](window-lifecycle.md) 处理。

## 模型

普通 Emacs buffer 在活动窗口显示 Evil 状态色块与 powerline 箭头，其余
Doom modeline 段和点击入口保持原样。Portal Web（含 Noema Markdown）和
Portal 终端用 `[app]` / `[emacs]` 表示谁接收键盘，不显示 Evil 状态色块。
状态色从当前主题的 face 前景色中按感知色差选取。

## 契约

- **C1 状态含义**：活动窗口的普通 Evil buffer 显示状态文字和箭头；
  Portal 原生视图即使内部 Evil 状态为 `normal`，也只显示键盘归属标记。
- **C2 主题配色**：状态色优先拉开与 mode line 底色和其他状态色的距离；
  换主题后重选并刷新箭头缓存。TTY 或无 XPM 时保留状态文字，不绘制图像。
- **C3 布局生命周期**：启用 Doom 时，将已打开且仍用原 `main` 布局的
  buffer 更新为新布局；专用布局保留。关闭 Doom 时由 Doom 恢复原布局。

## 实现位置

- `lisp/ui/modeline/config.el`：状态段、箭头、配色和布局更新。
- `lisp/init-modules.el`：模块声明。
- `test/init-ui-modeline-tests.el`：C1 与 C3 的窗口和状态测试。

## 已知差距

暂无。

## 待定设计

暂无。
