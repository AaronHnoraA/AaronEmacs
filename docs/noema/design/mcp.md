# Noema MCP

Noema exposes its live headless kernel as a Streamable HTTP MCP server.  The
Emacs Noema integration starts and supervises that kernel, registers the
repository at `~/Documents/Noema` (or `NOEMA_ROOT`), and publishes the current
loopback endpoint only after the repository is ready.

On macOS, the endpoint descriptor is normally written to:

```text
~/Library/Application Support/Noema/state/runtime/mcp.json
```

The exact location follows Noema's host state directory. The
descriptor is mode `0600`, is replaced atomically when the kernel restarts on a
new port, and is removed when the host stops or loses kernel health. A typical
descriptor is:

```json
{
  "name": "Noema",
  "transport": "streamable-http",
  "url": "http://127.0.0.1:43128/mcp",
  "noteRoot": "/Users/example/Documents/Noema",
  "notebook": "20260826000000-example",
  "ownedKernel": true
}
```

Configure an MCP client with the descriptor's `url` and Streamable HTTP
transport while Noema is running. Do not save a random port in permanent
configuration: read the descriptor again after the Emacs/Noema host restarts.

The `document` tool accepts repository-native Markdown paths for the main
operations:

- `get`: `notebook`, `path`
- `create`: `notebook`, `path`, optional `title` and `markdown`
- `list`: `notebook`, optional directory `path`
- `rename`: `notebook`, `source_path`, `title`
- `move`: `notebook`, `source_path`, target `path`

These operations read and write the Markdown files in place and refresh the
same Noema index used by Emacs. The MCP endpoint remains
loopback-only and uses the kernel's normal authentication, administrator, and
read-only checks.

## 渐进式本地 API

`scripts/agent-mcp.mjs` 提供 stdio MCP，只注册 `noema_api` 和 `noema_call`。
前者分页搜索运行中 host 的所有已注册 channel 名字，或查看一个 channel 的参数
及实际处理器；后者调用相同的后端，保留既有检查与通知。目录不内嵌完整 schema。

Pi 默认使用这个入口。其他新会话可选全局 `noema-api` 能力；已经运行的会话用
`node ~/.config/emacs/site-lisp/noema/scripts/agent-api.mjs help`，无需重启原生对话。
客户端每次重新解析活跃 `host.json`，不固定随机端口；调用不自动重放。
参数通过标准输入以位置 JSON 数组提交，避免把文档正文插进 shell 命令。

Markdown 修改使用 `agent:document:read` 读取版本和范围，再用
`agent:document:propose` 创建或修订审阅提案。它调用已有 `notes:save`，保留 CAS、
索引和页面保存通知。`emacs:pi` 提供主／项目 Pi 的
`list/status/ensure/send/resume/new/archive`；会话所有权和原生恢复见
[Pi 会话与项目助手](../../design/pi-sessions.md)。
这些接口仅注册在本地模式，公共 Server 的只读边界保持不变。

## Public Server mode MCP

Server mode exposes a separate, unauthenticated Streamable HTTP endpoint at
`https://wiki.pwo101.top/mcp` (or `/mcp` on another Server mode host). It reads
the same published catalog as the public Wiki. It provides four read-only
tools: `search_notes`, `list_notes`, `read_note` and `list_tags`. A note path is
the public path returned by search or list, such as
`public/Public-README/example.md`. Hidden pages and private repositories are
absent from this catalog. Server mode does not start the Go kernel or expose
its writable `/mcp/research` and `/mcp/coordinator` surfaces.

From AaronEmacs, `make noema-server-deploy` builds, uploads, restarts and
checks the configured remote service. The ignored local files
`server-config/runtime.json` and `server-config/deploy.json` in the Noema tree
hold the site and SSH settings; initialize examples with
`make noema-server-config-init`.

## Project MCP selection

MarginNote 的原生执行接口是独立的 stdio MCP 能力 `noema-marginnote`，由全局
能力配置声明，入口为 `plugins/marginnote/mcp.mjs`。它不属于知识库、研究 DAG 或
发布 MCP 的工具集；通过 Noema host 中转到已连接的原生插件窗口。手写笔记的
增量数字化流程由 README 技能库中的 `marginnote-digitize` 负责。接口与安装见
[MarginNote agent 接口](marginnote-agent-bridge.md)。

The supervised Noema endpoint is also the built-in `noema` capability. Projects
may define additional stdio, HTTP or SSE MCP servers, enable/disable them and
patch their configuration in `noema-capabilities.json`. Run preparation copies
the resolved active definitions into the immutable RunSpec; the Emacs ACP worker
does not rediscover them. Persistent definition/configuration is separate from
live runtime state. See [Project Skills and MCP capabilities](capabilities.md).

Agent 的调用统一经过 `agent:call`，再调用原有处理器；权限审批、用户回答和
Proposal 审核接口仍可查询说明，调用会明确返回仅供人类决策，不让 Pi 自授权限。
