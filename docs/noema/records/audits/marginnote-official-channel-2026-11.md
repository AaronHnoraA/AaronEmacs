# Noema 官方插件通道复核

日期：2026-11-05。接续[首次安全审计](marginnote-plugin-audit-2026-11.md)，当前契约见
[接口设计](../../design/marginnote-agent-bridge.md)。未修改 MN 应用本体、直接写资料库或绕过认证。

## 本次真机证据

Mac MN 5.0.1 build 50100 / Bridge 0.93，现有 Noema 1.0.3 身份 `noema.extension.agent`：

1. 从真实插件 owner 调用 `AddonWebPanel.panel()`，得到带 `loadHTML/loadPanel/onMessage/showFloatingIn/teardown` 的面板。
2. 用内联只读测试页与本插件 `onMessage` 打开面板，`floatingVisible:true`。
3. 官方 `POST /bridge/v1/addons/noema.extension.agent/panel/eval` 提交页面的 `fetch('/addon/ping')`。
   同步返回 accepted；第二次 eval 读异步结果，得到 `{native:true,echo:{proof:42},appVersion:"5.0.1",ok:true}`。
4. teardown 临时面板。随后无令牌/错误令牌均 401；有效令牌进入处理器，面板已关返回 409 `PANEL_NOT_OPEN`。

因此官方认证 → 同一插件面板 → 原生回调 → 官方结果的只读链路成立。它不是新正式包安装验收，
也不是精确批注已经通过这条新传输写入。此前真实批注仍是旧通道的证据。

## 正式源码与安全收敛

同一 Noema 升至 1.0.6，默认包仅有 `main.js/mnaddon.json/panel.html/icon.png`，无地址、令牌、
外部页面资源或新监听。明确 `--legacy` 才嵌入旧私有配置。host 下次启动默认不创建旧监听或令牌，
仅 `NOEMA_MN_LEGACY=1` 开启；发布 server 模式仍禁止。当前旧 host 尚未重载，不能把默认值当运行事实。

- 官方 Node 客户端读取明确设备的已有凭据，校验真实私有 IPv4（拒绝 `192.168.attacker.example` 一类域名），
  路径固定 `/bridge/v1`，不跟随重定向。旧通道只有显式选项，失败不自动切换。
- 页面只接受固定的 status/submit/result 信封；原生执行仍完整开放 JSB，未制造一个虚假的权限沙箱。
  页面 CSP 不加载外部内容；原稿内容仍是数据，不能提升为执行授权。
- submit/result 检查 native sessionId，避免换窗口后写错。忙时拒绝新任务，没有后台执行队列。
  同 ID 同内容复用结果，冲突拒绝；回调先返回，再调度用户代码。超时不重放、不强制终止副作用。
- 代码和 args 各 256 KiB，原生值 1 MiB；完成任务 payload 合计 8 MiB、最多 32 项/15 分钟，
  另保留 256 个淘汰 ID。页面回执最多 16 项/4 MiB，原生 HTTP 回调 10 秒后报告未知。
  缓存有界且易失，仍需业务层 plan/source 身份及写后回读。
- 空闲不发请求、无周期定时器；复用已核对的 sessionId 可省去每任务的两次状态 HTTP 请求。
  仅运行中的请求按短延迟逐步退避查询；status/exec 不偷偷打开 UI，open 是明确动作。

## 部署边界

已通过 MN 正常打开同 ID 安装包。原生 UI 实际读到 `UIAlertController`，提示“插件已存在，是否替换？”，
按钮为“是／否”。当前工具没有正常点击通道，已请用户完成该原生确认；没有调用私有 reload、
直接覆盖已安装文件或新建另一个插件身份来绕过。在线 1.0.3 不能代表源码 1.0.6 已生效。
安装后需同时读 manifest 和实际加载源码/运行回执，核对最新构建摘要，不能只看版本号。

仍待：正式包 exec 的真机全程、安装后的关闭重开/暂停行为、iPad 现场执行及同步、批注自动跟随移动。
手写/图片/卡片的局部读取和原生精确摆放既有能力不受这些未验证项代替。

## 检查结果

- 针对性 5 文件 42 项通过，含真实分层实现（客户端 + 页面 + 模拟 JSB）的异步闭环、重复请求、
  窗口身份变化、超时晚到结果、缓存淘汰、无凭据包和旧传输边界。
- Skill 59 项通过，skill validator 通过。眼睛特征、参考图来源、坐标逆变换及独立批注流程继续保留；
  新增官方/兼容通道选择、按需面板、版本生效核验及缓存预算。
- `make build install`、`go test -tags fts5 ./...` 通过。
- 最终全仓 3599 通过、1 失败、16 跳过；剩下 `image-layout` 是首次审计已有的 LaTeX 期望差异。
  早一轮的 4 个性能/时序相关文件单独复跑 75 项全部通过，最终全仓也通过。没有更改无关实现。
- Research 234 通过/1 失败，Jupyter 214 通过/1 失败，仍是首次审计记录的同两项。

本机回执：`var/noema/marginnote/optimization/`。临时面板测试源码、鉴权状态、构建和测试日志均保留，
不记录令牌。正常安装包最终摘要另存本机回执，版本相同也要核对实际内容。
