# Emacs China「vibe package 2」→ Noema 审计（2026-10-02）

来源：[帖子](https://emacs-china.org/t/vibe-package-2/32095)。以下源码固定在审计时的提交，功能判断按源码与 README 对照；没有把 README 的能力当成 Noema 已经实现的能力。

| 项目 | 固定源码 | Noema 决定 |
|---|---|---|
| agent-shell-fork-tree | [README](https://github.com/roife/agent-shell-fork-tree/blob/c0a61ff6a3ceae53473a8533a06cb7648297d5eb/README.md)、[ACP 扫描](https://github.com/roife/agent-shell-fork-tree/blob/c0a61ff6a3ceae53473a8533a06cb7648297d5eb/agent-shell-fork-tree-acp.el)、[树与原生会话动作](https://github.com/roife/agent-shell-fork-tree/blob/c0a61ff6a3ceae53473a8533a06cb7648297d5eb/agent-shell-fork-tree.el)、[索引](https://github.com/roife/agent-shell-fork-tree/blob/c0a61ff6a3ceae53473a8533a06cb7648297d5eb/agent-shell-fork-tree-store.el) | **吸收**：锁定 package-vc 版本，经 `noema-agent-acp` 进入 Noema Agent 工作区。 |
| agent-shell-btw | [README](https://github.com/roife/agent-shell-btw/blob/6c74761ce95a882ac40a53e31ae9b05ca0fafc63/README.md)、[实现](https://github.com/roife/agent-shell-btw/blob/6c74761ce95a882ac40a53e31ae9b05ca0fafc63/agent-shell-btw.el) | 保留为下一步独立功能候选：继承上下文的临时 fork，区别于 Noema 已有的不带历史 Side chat。需要把 fork 保留/删除和 Noema 暂存会话生命周期统一后再接，不能把它直接绑定到现有 `s`/`C-c C-q`。 |
| org-typst-preview | [README](https://github.com/roife/org-typst-preview/blob/867ba67b8d0c52d7119b44f940a844bb1bf3f9d7/README.org)、[实现](https://github.com/roife/org-typst-preview/blob/867ba67b8d0c52d7119b44f940a844bb1bf3f9d7/org-typst-preview.el) | Org 的异步 Typst 数学预览；Noema `.noema` 是 nbformat 工作文档，数学预览由 JuText/RaTeX 路径负责。可供单独的 Org 配置评估，不进入 Noema Agent 工作流。 |
| jupyter.el | [README](https://github.com/roife/jupyter.el/blob/8bf5276acd083fae8318f4259c79374c036907fd/README.md)、[编辑与内核模块](https://github.com/roife/jupyter.el/blob/8bf5276acd083fae8318f4259c79374c036907fd/jupyter-notebook.el) | 提供原生 Emacs `.ipynb` 界面；Noema 已有独立 Jupyter 工作流，`.noema` 不走内核。另开编辑器产品路线会产生双重 notebook UI，此轮不接。 |

## 对话树的工作流落点

`agent-shell-fork-tree` 通过 ACP `session/list` 找候选，通过 `session/load` 读历史；支持 `session/fork` 和 `session/delete` 时会用临时 fork 并行读取，其他后端串行加载原会话。它把同一 agent、同一执行目录、共享非空文本前缀的历史呈现为树。**这个结构是从文本推断的可浏览视图，不能证明后端的真实 fork 亲缘，也不能替代 Noema 的 WorkNode 任务 DAG。**历史回合是预览；通用 ACP 只允许在原生会话末端继续或 fork。

Noema 的入口是 Agent 窗口 `C-c C-t`、Sessions 列表 `T`、`C-c A T`。Sessions 列表的冷会话先恢复，再打开树；原生 `RET`/`f` 回到 Noema 的 Agent 窗口，新分支登记为项目命名会话，后续可重命名或点名发送上下文。树只作会话导航，不改变 Run 的 WorkNode 所有权、`@@session` 的持久绑定或已有 `F` 的 handover 声明语义。

上游默认可把完整 prompt/reply 写入私有 JSON 缓存。Noema 已明确只以 agent 后端的原生历史为对话记录，因此设置 `agent-shell-fork-tree-cache-directory=nil`，只在当前 Emacs 进程内索引；重启后需按 `g` 重新发现。扫描并发上限设为 2，避免一个 Project 打开树时同时启动过多 ACP 客户端。后端必须同时宣告 `session/list` 和 `session/load`；缺少能力时在进入树前报错。

上游在把目录转成 ACP 目标原生路径后又调用 `file-truename`；远程目标的原生路径不能拿到本机文件系统解析。Noema 在调用树入口的同步范围内保留已经规范化的 ACP cwd 原样，确保本机和远程都查询实际 agent 的执行目录。树中复用已打开 buffer 时还按 agent 标识和执行目录限制候选，避免不同目标碰巧有相同原生 session ID 时切错会话。

原生分叉不会自动建立 Noema `parentName`：Noema `F` 的 `parentName` 是下一次 Run 使用的新命名会话的 handover 关系，原生 fork 是后端历史操作。若要把两者关联，需要单独定义产品语义，不能根据共享文本前缀悄悄写入研究 DAG 或 Session 声明。
