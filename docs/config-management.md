# 统一配置管理（`config` 包）

`site-lisp/config/` 下的 `config` 包是本配置中**所有可配置项的统一入口**。

## 核心原则：不在 Lisp 代码里硬编码值

**所有可配置的值都存在 `etc/config-store.el` 或其他 `etc/config-*.el` 配置文件里，
不要写成散落在 `init-*.el` 里的 `setq`。**

这样做的原因：
- 防止**变量污染**——多处 `setq` 同一个变量，难以追踪谁是最终生效的值
- 在 `config-board` 里可以直接看到、修改、持久化每个配置项
- 启动时 `config.el` 自动加载 `etc/config-store.el` 和同目录下的 `config-*.el`，
  每次 `config-register`
  立即应用已存储的值，无需等待 `after-init-hook`

### 迁移新配置项的正确姿势

```elisp
;; 1. 在对应模块里注册
(config-register 'my-new-setting
  :group 'mymodule :type 'boolean
  :doc "什么时候启用这个功能")

;; 2. 把初始值写入 etc/config-store.el（通过 config-set 或直接编辑）
M-x config-set RET my-new-setting RET t
;; 或者直接在 etc/config-store.el 里的 alist 里加一行：
;;   (my-new-setting . t)

;; 3. 不要再写 (setq my-new-setting t)
```

如果是复杂配置（非简单变量），用 `config-register-file` 注册专属的
`etc/` 文件，在那里管理内容。

如果想把简单变量放进专门的 store 文件，直接把该 key 写进对应
`etc/config-*.el` 即可。`config` 加载时会记录 key 来自哪个文件，之后
`config-set` 会写回原文件；不需要在核心代码里维护 group→文件映射。
store 文件只解析受支持的 `(config-store-set '...)` 数据表单，不执行任意 Lisp。

## 文件结构

| 文件 | 职责 |
| --- | --- |
| `site-lisp/config/config.el` | 注册表核心 API (`(provide 'config)`) |
| `site-lisp/config/config-tools.el` | 管理面板 `config-board` + transient `config-dispatch` |
| `etc/config-store.el` | **随仓库提交的配置值**，既是初始值也是用户自定义值 |
| `etc/config-*.el` | 自动发现的专题 store；已存在的 key 会写回原文件 |
| `lisp/init-base.el`（尾部） | Emacs 原生变量的注册声明（不含 setq，值在 store 里） |

加载顺序：

1. `init.el` 在模块图之前 `(require 'config)` → 立即加载 `etc/config-store.el`
   和同目录下的 `etc/config-*.el` → `config--overrides` 填充完毕，同时记录
   每个 key 的来源文件
2. 每个模块的 `(config-register ...)` 调用时，如果 store 里有该项的值，
   **立即写入变量**（不再等 after-init-hook），保证模块加载期读到正确的值；
   但此时**不跑 `:on-change` 回调**——回调统一推迟到第 3 步，避免被多个 key
   共用的回调（如 20 个字体项共用 `my/font-reset-all`）在注册期反复触发
3. `config-apply-store` 挂在 `after-init-hook` 做最后一遍：先写入所有值，
   再跑 `:on-change`，且**每个回调每次启动只跑一次**——无参回调（如
   `my/font-reset-all`）按函数去重只调用一次，带参回调仍按 (NAME VALUE)
   逐项调用。这样一次启动只重建一次字体/face，而不是几十次

> 注意：注册期推迟 `:on-change` 只针对**启动窗口**（`after-init-time` 为 nil）。
> 启动完成后才加载的模块（延迟 `use-package` 等）注册时，`config-apply-store`
> 不会再跑，因此此时会立即应用值**并**触发 `:on-change`，保证延迟模块的回调不丢。

如果运行时新增、删除或**直接编辑**了 `etc/config-*.el`，执行
`M-x config-refresh-store-files`（dispatch 里 `R`）。它会**以磁盘为准全量重建**：
先丢弃内存中的覆盖，再重新发现并读入所有 store 文件，因此磁盘上的修改、新增的 key
以及删除的 key 都会被反映出来（运行时严格对齐磁盘）。所有通过 `config-set`/面板
改过的值都已落盘，不会因重建而丢失。

## 日常用法

### 管理面板

- `M-x config-board`，或 leader `SPC h c` 打开 transient 调度菜单 `config-dispatch`

面板按组列出 `Group | Name | Type | Value | Doc | Source`。

| 键 | 作用 |
| --- | --- |
| `RET` / `e` | 编辑当前项（按类型分派） |
| `t` | 切换布尔变量 / hook 成员 |
| `d` | 删除该项的存储覆盖（下次启动回到 Emacs 内置默认） |
| `r` | 重新加载文件项（运行 loader + 更新脚本） |
| `o` | 打开文件项 |
| `f` | 按组过滤（留空清除） |
| `s` | 强制写入 store |
| `!` | 自检完整性（`config-check`）：校验索引与 store，发现索引漂移就地修复 |
| `g` | 刷新 |
| `q` | 退出 |

**Doc 列**：该项注册时的 `:doc` 说明或 `:choices` 列表，静态不变，方便
修改时参考合法值格式。

transient `config-dispatch`：打开面板、保存 store、刷新 store（`R`，全量重建）、
自检完整性（`!`，`config-check`）、打开 store 文件、清空所有覆盖（`D`）。

`config-check`（面板 `!` / dispatch `!` / `M-x`）校验注册表的不变式：覆盖索引与
有序 alist 是否一一对应、每个覆盖是否能解析到 store 文件、各 store 文件是否还能
正确解析。只发现索引漂移时会就地 `config--reindex` 自愈，其余问题列在
`*Config Check*` 缓冲区。`config-store-set` / 覆盖写入只走 `config--clear-overrides`
等集中入口维护索引，确保「清空全部覆盖」等操作不会留下悬挂的 cons。

### Lisp API

```elisp
(config-get 'my/font-code)            ; => 当前值
(config-set 'my/font-code "Iosevka")  ; 即时生效 + 写入 store + 跑 :on-change
(config-reset 'my/font-code)          ; 从 store 里删除该项（变量值保持当前）
(config-list 'font)                   ; 内省：按组列出注册项
```

## 注册语法

```elisp
;; 1) 普通变量，带即时更新脚本
(config-register 'my/font-code
  :group 'font :type 'string :doc "代码字体族"
  :on-change #'my/font-reset-all)        ; :var 默认就是 'my/font-code

;; 2) buffer-local 全局默认值：用 :get/:set 操作 default-value
(config-register 'tab-width
  :group 'editing :type 'integer
  :get (lambda (_n) (default-value 'tab-width))
  :set (lambda (_n v) (setq-default tab-width v)))

;; 3) 函数 / 策略槽
(config-register 'browse-url-browser-function
  :group 'browser :type 'function
  :choices '(xwidget-webkit-browse-url eww-browse-url browse-url-default-browser))

;; 4) hook 成员管理
(config-register-hook 'prog-mode-hook
  :group 'editing
  :candidates '((display-line-numbers-mode . "行号")
                (hl-line-mode               . "高亮当前行")))

;; 5) etc 配置文件 + 修改后更新脚本
(config-register-file 'clutch
  :group 'database
  :path    (expand-file-name "etc/clutch-config.el" user-emacs-directory)
  :example (expand-file-name "etc/clutch-config.el.example" user-emacs-directory)
  :loader  (lambda () (my/clutch--load-config)))
```

`config-register` 关键字：`:type`（`boolean`/`integer`/`number`/`string`/
`function`/`face`/`sexp` 或 `(choice ...)`）、`:group`、`:var`（后端变量，默认 = NAME；
传 `:var nil` 表示纯注册表值）、`:doc`、`:choices`、`:on-change`、`:set`/`:get`、
`:source`、`:store-file`。

**没有 `:default`。** 初始值在 `etc/config-store.el` 或专题 `etc/config-*.el` 里。

通常不需要写 `:store-file`：已有 key 会按加载来源写回，`etc/config-*.el` 会自动发现。
只有在新增 key 第一次保存前就必须强制写到某个非默认文件时，才给单项加
`:store-file`。

写回时会先比较生成内容；内容不变不会刷新文件 mtime。实际写入使用同目录临时文件
加 rename，避免 store 被半写。

## `etc/config-store.el` 格式

```elisp
(config-store-set
 '((tab-width . 4)
   (scroll-margin . 2)
   (user-full-name . "Chang He (Aaron)")
   (completion-styles . (basic partial-completion substring flex))
   ((:hook prog-mode-hook hl-line-mode) . t)))
```

- 变量项：`(SYMBOL . VALUE)`
- Hook 成员：`((:hook HOOK FN) . t/nil)`

该文件随仓库提交，是配置的真正来源。`config-set` / 面板写操作会更新这个文件。
其他 `etc/config-*.el` 使用同样格式，并由 `config` 自动发现。

## 增量迁移工作流

1. 找到散落的 `(setq some-var value)` 硬编码
2. 在对应模块底部 `(config-register 'some-var :group 'g :type 't :doc "...")`
3. 把值加到 `etc/config-store.el` 或专题 `etc/config-*.el`（`config-set` 或手动编辑）
4. 删除原来的 `(setq some-var value)`

没迁移的项保持原样，正常工作。

种子注册见 `lisp/init-base.el` 尾部「Config registry」一节。
