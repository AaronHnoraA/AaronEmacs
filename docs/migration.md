# Migration Notes

这份文档只讲一件事：怎么把这套配置稳定迁到新机器，并解释这次真实迁移里踩到的坑。

## 推荐入口

新机器 / 新 clone：

```sh
make up
```

如果你带着旧机器导出的状态快照：

```sh
make up SNAPSHOT=/path/to/emacs-state-YYYYMMDD-HHMMSS.tar.gz
```

只恢复依赖：

```sh
make install
```

维护者更新锁文件：

```sh
make lock
```

检查当前环境和锁文件是否漂移：

```sh
make audit-lock
```

`make up` 现在是默认入口，因为它把“恢复”和“验收”合并成了一条链路：

- 可选先恢复状态快照
- 按锁文件恢复 Emacs 包依赖
- 跑 startup / byte / native health
- 跑 critical doctor
- 跑 lock drift audit

其中恢复和锁文件审计这两步现在都故意不依赖 `init.el`：

- `make install`
- `make audit-lock`

只走 `bootstrap.el` + `package-lock.el`。

原因很直接：

- 恢复阶段本来就可能还缺主题、ligature、TRAMP 扩展、终端包等依赖
- 如果这时先要求配置文件自己完整加载，恢复链路就会形成循环依赖

## 为什么这样分

之前最大的问题是把“当前目录里已经有少量包”误判成“这是旧环境，应该导出锁文件”。

实际后果是：

- 新机器不会按锁文件安装完整依赖
- 启动时才在 `ligature`、`multiple-cursors`、`treesit-fold` 之类的地方中途爆炸
- `make install` 失去“确定恢复环境”的意义

现在改成了显式模式：

- `install` 只做恢复
- `lock` 只做导出
- `setup` = 恢复 + 启动检查
- `up` = 可选恢复状态 + 完整拉起验收

这样迁移链路是确定的，不需要靠启发式猜测。

## 这次迁移里真正踩到的坑

### 1. VC 包不能在启动时反复尝试安装

像 `treesit-fold`、`org-modern-indent`、`org-appear` 这种 VC 包，如果目录已经存在，再跑 `package-vc-install` 很容易卡在：

- 是否覆盖已有 checkout

结论：

- 启动阶段只应该“能加载就加载”
- 安装和升级要交给显式命令

### 2. 带本地构建的包要优先复用现成产物

这次卡过两个典型：

- `pdf-tools` 的 `epdfinfo`
- `vterm` 的 `vterm-module`

结论：

- 如果已有可执行/可加载产物，优先直接复用
- 只在检测失败时重建
- 批处理检查里尽量避免交互式编译提示

### 3. 运行时状态必须持续收敛

如果顶层残留这些东西，迁移时会很混乱：

- `projects.eld`
- 顶层 `eln-cache`
- 顶层 `tree-sitter`
- 顶层 `*.elc`
- 本地 API 密钥或查询地址

现在约定是：

- `var/` 放运行时状态和缓存
- `etc/` 放本地配置和密钥
- `.gitignore` 忽略本机私有内容

### 4. 包依赖和系统依赖必须分层看

这次迁移已经把 Emacs 包依赖这层打通了：

- 普通包由 `package-lock.el` 约束
- VC 包由 recipe + 锁文件约束
- `make install` / `make up` 可以确定性恢复

但系统依赖仍然是另一层，例如：

- `rg`
- `git`
- `latexmk`
- `dvisvgm`
- `hunspell`
- `gls`
- 本地编译链

所以以后说“依赖都解决了吗”，要分清：

- Emacs package 依赖：是
- 系统外部依赖：未完全自动化

## 迁移后的最小验收

执行：

```sh
make up
```

或者最少也要：

```sh
make bootstrap-health
```

应至少确认：

- `INIT-OK`
- `Byte-compile finished`
- `NATIVE-OK`
- `package-lock` audit 为 `:ok t`

如果这里就失败，先修迁移链路，不要带着坏状态继续日常使用。
