# 文件与目录本地变量的信任

## 范围

本文规定 Emacs 如何确认并记住文件本地变量和 `.dir-locals.el`，以及用户明确
信任的目录如何生效。变量的具体含义由所属模块定义。

## 模型

Emacs 原生 `trusted-content` 按文件或目录前缀信任内容；
`safe-local-variable-directories` 按 `.dir-locals.el` 的来源目录批准目录变量。
两个默认信任根在 `aaron-base.el` 中设定；额外批准通过 Customize 持久化到
`etc/custom.el`。对未知目录，Emacs 原生确认提示的
`+` 批准并记住该目录，`!` 只记住当时列出的变量和值；一次性的 `y` 不存储决定。

`~/Documents/Noema/` 与 Emacs 配置目录默认完整信任：文件内容使用
`trusted-content`，目录变量使用 `safe-local-variable-directories`。没有启用
全局 `:all`；调用方显式禁用本地变量或 `eval` 时仍遵守禁用设置。
`.dir-locals.el` 位于项目根时，批准其来源目录也覆盖使用它的项目文件。

## 契约

- **C1** 两个默认目录中的文件与目录变量可直接应用，其他目录的未知变量先询问。
  测试：`local-trust-c1-default-roots-are-trusted`、
  `local-trust-c1-trusted-directory-enables-local-variables`；Noema 根目录的
  `*scratch*` 已做启动后的目录变量应用验证。
- **C2** 对未知项目按 `+` 后，该 `.dir-locals.el` 的来源目录被保存，下次访问
  不重复询问；按 `!` 只持久化具体变量值。测试：
  `local-trust-c2-directory-approval-is-reused`；持久化使用 Emacs 原生
  `customize-push-and-save`，已在重新启动的 Emacs 进程中验证。
- **C3** 旧项目设置名迁移到当前 `+project-local-settings` 与 `:noema-jupyter`，
  避免批准后仍把设置写入无人读取的变量。

## 实现位置

| 部分 | 位置 |
| --- | --- |
| 本地变量的默认策略与可信内容判断 | `lisp/core/aaron-base.el` |
| 默认信任根 | `lisp/core/aaron-base.el` |
| 用户额外保存的批准记录 | `etc/custom.el` |
| 当前项目 `.dir-locals.el` 的管理入口 | `lisp/emacs/dir-locals/config.el` |
| Noema 项目配置 | `~/Documents/Noema/.dir-locals.el` |

## 已知差距

无。

## 待定设计

无。
