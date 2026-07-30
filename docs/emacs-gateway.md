# Emacs 通讯网关

`remote-gateway` 是 Emacs 对 Lean、Noema 等外部进程的统一控制面。它只监听
loopback，在同一个动态端口上提供：

- `POST /rpc`：一次性 JSON-RPC 2.0 请求；
- `GET /ws`：双向、可注册的 WebSocket JSON-RPC 2.0 通道；
- `GET /health`：轻量健康检查。

接口没有 `/v1` 前缀。启动后，连接信息写入
`var/emacs-gateway/<emacs-pid>.json`，Emacs 退出时自动清理。

## Lisp 入口

```elisp
(require 'remote-gateway)

;; 注册 Emacs 提供的方法。函数接收 params 与发起请求的 WebSocket client。
(remote-gateway-register-method
 "example.add"
 (lambda (params _client)
   (+ (alist-get "a" params nil nil #'string=)
      (alist-get "b" params nil nil #'string=))))

;; 获取 HTTP、WebSocket 和 health URL。
(remote-gateway-connection-info)

;; 启动外部 helper 前创建一次性注册绑定。
(remote-gateway-prepare-client
 "example-helper" context
 :placement 'client
 :provides '("example.render"))

;; 对已注册 helper 发请求或通知。
(remote-gateway-request-sync "example-helper" "example.render"
                             '((source . "hello")))
(remote-gateway-request-async
 "example-helper" "example.render" '((source . "hello"))
 (lambda (result error)
   (if error
       (message "render failed: %S" error)
     (message "rendered: %S" result))))
(remote-gateway-notify "example-helper" "example.changed"
                       '((path . "/tmp/a")))

;; helper 永久停止时撤销 binding，并可同时断开现有 peer。
(remote-gateway-release-binding binding t)
```

`placement` 有两个值：

- `client`：helper 在 Emacs 所在机器运行，直接连接 loopback；
- `target`：helper 在 Remote target 上运行。网关通过该 workspace 所有的稳定反向
  port forward 暴露给 target；SSH/TRAMP 的建立、恢复和关闭均由 Remote 管理。

consumer 不应自行判断本地或远程，也不应自行维护 SSH tunnel。

## HTTP JSON-RPC

```sh
curl -s http://127.0.0.1:PORT/rpc \
  -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"gateway.ping","params":{}}'
```

内建方法：

- `gateway.ping`
- `gateway.status`
- `emacs.eval`

`emacs.eval` 接受 `params.source`（`params.script` 也可），顺序读取并执行其中的所有
Lisp form，返回最后一个值的 JSON 表示与打印表示：

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "emacs.eval",
  "params": {
    "source": "(setq my-value 40) (+ my-value 2)"
  }
}
```

它具有当前 Emacs 进程的完整执行权限。网关因此只绑定 loopback；远端只能通过
Remote 管理的 SSH/TRAMP forwarding 使用，不应把端口暴露到不可信网络。

## WebSocket 注册

helper 使用 `remote-gateway-prepare-client` 返回的 `websocket-url` 和 `binding-id`
连接 `/ws`，然后首先发送：

```json
{
  "jsonrpc": "2.0",
  "id": "register",
  "method": "gateway.register",
  "params": {
    "bindingId": "binding-...",
    "instanceId": "helper-process-...",
    "provides": ["example.render"]
  }
}
```

绑定把 peer 固定到 `client-id + target-id + workspace-id`，Remote channel registry
负责 listener、peer 与 forwarding 的所有权和生命周期。断线重连可以继续使用同一
binding。同一逻辑 client 重启时旧 binding 会被替换回收；仍在线的 client 会复用
现有 binding。所有 pending request 都有超时，peer 断线或网关停止时会立即结算，
不会留到 Emacs 退出。

## 当前接入

- Lean：LSP 继续走 stdio；infoview endpoint 注册、cursor 通知走网关。
- Noema：命令、事件与 `/api` 控制调用走网关；页面、静态资源、SSE 等数据面
  保留在 Noema HTTP server。远程 Markdown 的逻辑 `/fs:` 路径不投影成
  Noema 所在机器的路径，而是通过 `aaronnote.file.read` 和
  `aaronnote.file.write` 回调 Emacs，由 Remote/TRAMP 完成读写。
- Copilot：Noema 通过网关调用 Emacs 的 `copilot.request`，不再维护独立
  bridge server。普通 Remote buffer 也复用 Emacs 客户端的同一个 Copilot
  language-server binary；进程通过 `remote-make-client-process` 固定为 client
  placement，不会在 target 上查找或安装 Copilot。

### Noema 远程 Markdown

Noema web-host 始终运行在 Emacs 客户端，不要求 Remote target 安装 Node、
Noema 或额外常驻服务。打开 `/fs:TARGET:/path/note.md` 时：

1. 浏览器与 Node 全程保留 `/fs:` 文件身份；
2. Node 通过双向网关请求 Emacs 读取或保存；
3. Emacs 使用普通文件 API，由 Remote 的 route/backend 转换为 TRAMP 或其他
   transport；
4. 文件监听作为 recoverable resource 归 Remote workspace 所有，Noema
   停止时一并释放。

保存使用同目录临时文件再重命名，并保留原权限。请求携带读取时的 mtime，文件在
远端被其他程序修改后会返回冲突，而不是静默覆盖；也会拒绝把非空文件意外保存成
空内容。某个 backend 不支持文件监听时，打开和保存仍可用，显式刷新及保存时的
mtime 冲突检查继续生效。

当前远程文件以 standalone note 打开：Markdown 编辑、预览、显式保存、外部修改
刷新可用；整个远程目录不会被隐式并入本地 Noema vault 索引。远程文件关闭
自动保存，编辑器会显示 `manual save` 状态；有未保存内容时切换文件或刷新会被
阻止。本地 Noema 笔记继续自动保存。
