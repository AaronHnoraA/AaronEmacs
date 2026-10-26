# neo-emacs 结构化升级

> 第二阶段。先完成第一阶段 [以 Doom 为模板升级配置](../doom-template/README.md)，再在那个骨架上吸收本项目。

研究 [sincebyte/neo-emacs](https://github.com/sincebyte/neo-emacs)，借它的模块框架把本配置的 `lisp/` 重新组织成可按模块开关、自带说明的结构。

- 上游代码：`upstream/`（本地 clone，被 `.gitignore` 忽略，不进本仓库）。更新用 `git -C docs/works/neo-emacs/upstream pull`。
- [structure.md](structure.md)：两边框架的对照，以及文件结构和项目组织怎么融合。
- 本文件是这个项目的工作台：目标、发现、决定和下一步都记在这里。结论定下来以后写进对应的 `design/` 文档和 `AGENT.md`，项目结束后本文件移到 `records/`。

## 目标

1. 读懂 neo-emacs 的框架：模块怎么声明、怎么开关、怎么分目录、文档怎么跟着模块走。
2. 全量吸收 neo-emacs 的配置和细节，骨架以本配置为准：每一条都要有去处（直接吸收 / 合并到已有实现 / 换实现以满足本配置的约束），不收的必须写明理由。
3. 吸收它的框架组织方式（分类、模块目录、开关和 flag、模块索引），重组本配置，过程中不破坏现有入口、Remote 框架约束和测试。

## 上游概览（clone 于 2026-10-26，`ad8d42b`）

neo-emacs 不是独立框架，而是一套 **Doom Emacs 私有配置**：

- `init.el` 是 Doom 的 `(doom! :category module (module +flag) ...)` 声明，按 `:completion :ui :editor :emacs :term :checkers :tools :os :lang :app :neo-emacs :config` 分类开关模块，`+flag` 选模块内的可选特性。
- `modules/neo-emacs/<module>/` 是作者自己的模块，每个目录一份 `config.el`（行为），有依赖时一份 `packages.el`（`package!` 声明，Doom 底层是 straight.el），多数还有 `readme.org`（及导出的 md/html）。共 20 个：common、frontend、eglot、java、org、shell、sql、git、agentshell、rime、translate、find、modeline、modelinexp、windows、alpha、addspace、eredis、elfeed、telega。
- `neoemacs/` 放二进制和资源（rime 动态库、字体、tramp-rpc 二进制、ultra-scroll 等），约 54M。
- `patches/emacs30|31/` 是构建 Emacs 的补丁，`snippets/` 是 yasnippet 片段。

## 和本配置的边界

- 本配置用 `package.el` + `use-package`，`AGENT.md` 明确不引入 straight.el 等第二套包管理；所以 **不迁到 Doom**，只借它的组织方式（模块目录、开关声明、flag、模块自带 readme）。
- 模块顺序目前以 `lisp/init-modules.el` 为准；任何“模块声明”方案都要保留单一权威顺序，并能在不加载整个 `init.el` 时做包恢复（`bootstrap.el` / `package-lock.el`）。
- Remote-first、`/fs` 身份、config 注册表、general.el 键位层这些约束不因重组改变。

## 已完成的研究

- [x] Doom 模块解析、flag 和加载顺序：见第一阶段的 [module-system.md](../../design/module-system.md)。
- [x] neo-emacs 各模块逐项去处：见 [inventory.md](inventory.md)。
- [x] 文档组织：代码树保留分类 `README.md`，详细规范留在 `docs/`；不复制 Doom 的逐模块 readme。
- [x] `lisp/` 分类、目录、开关、迁移和验证：见 [structure.md](structure.md) 与第一阶段的 [README.md](../doom-template/README.md)。

## 决定

- **键位以本配置现有的为准**（2026-10-26）：他的键只在本配置的空位上考虑，不覆盖现有键和 Evil 原生键，冲突的逐个另找位置或不收。
- **一个模块一个模块审计**：每次给出一个模块的条目和建议，由用户批准是否合并、如何合并，再动手；结果记在 [inventory.md](inventory.md)。
