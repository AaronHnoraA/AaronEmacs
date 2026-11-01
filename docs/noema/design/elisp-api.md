# Noema Emacs Lisp API

## 范围

`lisp/noema-api.el` 提供项目身份、能力配置、Run 取消与显式文档模型查询。
Agent 的进程、Session、编辑器上下文和管理 UI 归 Emacs 的 `lisp/tools/agent/`。
Markdown 页面通过已有保存与上下文桥交互。

## 模型与契约

- **C1｜项目查询不创建状态。** `noema-current-project`、`noema-project-root`、
  `noema-project-scope`、`noema-project-workspace` 与 `noema-project-client-path`
  复用既有项目发现和 Remote 路径规则。需要创建项目时使用已有的
  `noema-project-ensure` 交互。
- **C2｜能力查询与写入分开。** `noema-capability-list`、`noema-skill-list`、
  `noema-mcp-list`、`noema-capability-config` 与 `noema-capability-resolve` 是异步
  查询，回调为 `(result error-object)`。`:scope 'global` 不发现项目；
  `:requested-skills`、`:requested-packs` 只表示本次选择，不污染持久选择或编辑器缓存。
- **C3｜能力写入有明确范围。** `noema-capability-set-enabled`、
  `noema-capability-set-patch`、`noema-mcp-register`、`noema-pack-define`、
  `noema-skill-install` 通过既有宿主接口原子修改选中的能力配置并重新解析。
  这些操作不重写链接的外部客户端配置；查询不表示授权写入或连接 MCP。
- **C4｜执行通过共享运行时。** `noema-agent-cancel` 取消已知 Run；原生 Agent
  操作复用共享 `tools/agent/` 命令。现有 `.prompt` 执行仍可经
  `noema-agent-worker-run-prompt-file` 调用冻结 Run 路径。
- **C5｜模型保留，专用编辑 API 移除。** `noema-node-id` 和显式传入 DOCUMENT 的
  `noema-node-parents`／`noema-node-children` 是纯模型查询。
  JuText 的 current-document/Cell/node、结构编辑、at-point Run、Graph Board
  导航与项目文件执行包装不再提供；不以伪造的当前 Cell 代替它们。

## 用法

```elisp
(noema-capability-list
 :scope 'global :requested-packs '("noema-project")
 :callback (lambda (resolution error-object)
             (if error-object
                 (message "Noema capability lookup failed")
               (message "%S" (noema-capability-active resolution)))))
```

`noema-capability-cached`、`noema-capability-refresh` 与
`noema-capability-active` 保持既有缓存与结果读取约定。Skill/MCP 查询与写入的
完整配置格式见 [capabilities.md](capabilities.md)。

## 实现位置

Pi 独立 Session 的入口是 `noema-pi-emacs-open`、`noema-pi-router-open` 和
`noema-pi-sessions`。程序协调通过按需披露的 `noema:api:emacs:pi`，不调用
Research 的 promote/bind；记录身份、恢复与归档见
[Pi 会话与项目助手](../../design/pi-sessions.md)。

- `site-lisp/noema/lisp/noema-api.el`：公开 API。
- `site-lisp/noema/lisp/noema-research.el`：项目发现、保留的文档模型与验证。
- `site-lisp/noema/lisp/noema-agent-worker.el`：现有冻结 Run 的执行与取消。
- `lisp/tools/agent/`：通用 Agent 运行时、上下文和管理。

随代码发布的 `noema-elisp-api` Skill 使用同一边界。
