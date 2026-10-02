# Aaron Elegant UI

Aaron UI 是 Emacs 与 Noema 共用的设计系统。深色 Kanagawa Wave 是 Emacs
的默认基调；`site-lisp/aaron-ui/aaron-ui.el` 提供语义角色、间距和形状 token。
Noema 继续由自己的 `aaronnote/style.css` 独占现有配色和 Markdown 宽度，
共享层不会覆盖这些变量或布局规则。

在 Emacs 中用 `M-x aaron-ui-load-theme` 选择 Wave、Dragon 或浅色 Lotus。
启动时使用 `aaron-ui-default-variant`；三种主题的界面语义色都跟随各自的
底色。标签栏、mode line、Org 标题和补全列表使用同一套主题色，减少
与正文竞争注意力的高饱和色块。Noema 的已提交 CSS token 仍导出 Wave。

## 语义角色

- `role-critical`：错误和不可逆操作
- `role-popout`：当前焦点和最需要注意的操作
- `role-strong`：正文主信息
- `role-salient`：链接、项目和二级强调
- `role-faded`：元数据与辅助信息
- `role-subtle`：边界、分隔和最低层级信息

Emacs 组件可通过 `aaron-ui-token`、`aaron-ui-color` 使用这些角色。Noema
导入角色 token 供新组件显式选用，但 Elegant adapter 只应用间距、圆角和交互
形态，不改变既有色值。

## Noema token 同步

Noema 使用仓库内已提交的纯 CSS 导出物，因此桌面 App 和发布站点可以独立于
Emacs 构建和运行。

```sh
make ui-tokens
make audit-ui-tokens
make ui-test
```

修改 Aaron UI token 后先重新导出，再运行审计。`make health` 已包含 token
测试与漂移检查。

## 窗口与文件界面

- 无原生顶栏的 GUI frame 不额外预留内部边距。窗口分隔线视觉上保持 1px，
  原生拖拽命中区为 7px，并沿用当前主题自己的背景与分隔色。
- Doom modeline 保留在底部；`C-x w d` 切换窗口 dedicated 状态，
  `C-u C-x w d` 设置强 dedicated，状态栏分别显示 `d` / `D`。
- Dired/Dirvish 和 Ibuffer 使用 Material SVG 文件图标。
- Treemacs 保留远程、Perspective、Projectile、Magit、LSP 行为和现有配色，
  Elegant 层只调整行距。
- TTY 或没有 SVG 支持时自动退回无 SVG 图标界面。
