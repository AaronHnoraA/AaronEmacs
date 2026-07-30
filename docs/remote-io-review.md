# emacs-io 对 Remote 的设计审查

本次审查参考 [Vibe Stream IO 讨论](https://emacs-china.org/t/vibe-stream-io/31791)
与 [gynamics/emacs-io](https://github.com/gynamics/emacs-io)。结论是吸收契约设计，
不引入该动态模块作为 Remote 依赖。

## 采纳

- 小而稳定的 stream/channel 操作面，以及 chunk/bulk 操作优先于逐字符调用；
- composite resource 必须原子建立、失败整体回滚、关闭幂等；
- benchmark、正确性测试和生命周期测试同时作为优化依据；
- native 快路径与 remote transport 共享同一消费者 API。

这些原则落实为 `remote-channel-group-*`：多端口协议共享 context、route intent 和
workspace owner，并以一个 recoverable resource 恢复。Jupyter 的五个 ZMQ channel
是首个消费者。

## 不采纳

- 原始 POSIX `open(2)` 会绕过 Emacs file-name handler，不能读取 `/fs:`；
- Common Lisp stream 命名空间不是本配置的公共 Remote 边界；
- 多个 user pointer 包装同一原始 stream、各自携带 finalizer，会造成所有权歧义；
- composite stream 保存未引用计数的子指针，不能满足 workspace 恢复和关闭顺序；
- 单对象 mutex 不能替代 session、transport 和 consumer callback 的整体并发契约。

Remote 继续使用 Lisp descriptor 管理所有权。若未来引入 C 加速器，它只能位于
native backend 内，必须保留 `/fs:`、workspace recovery 和纯 Elisp fallback，
并由等价测试及 benchmark 证明收益。

## 性能与生命周期验收

- channel group 只产生所需成员 forward，不额外创建 consumer tunnel；
- 任一成员失败后 registry、process 和 workspace resource 均无残留；
- gateway deferred request 在成功、错误、超时、断线和停止时 exactly-once 结算；
- sidecar 采用整文件原子替换；cell/widget 既有 payload 上限继续约束内存增长。
