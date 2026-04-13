# Quick Start

这份文档解决三个问题：

1. 这套配置依赖什么。
2. 新机器怎么装起来。
3. 装好后哪些目录和文件最重要。

## 1. 环境定位

这套配置明显偏：

- macOS 图形界面 Emacs
- 重 UI、重功能、重集成
- 本地编码 + SSH/TRAMP + Org 笔记/学术写作

Linux 不是不能用，但部分体验默认按 macOS 配置。

macOS 图形界面下当前修饰键约定是：

- `Command = Meta (M-)`
- `Option = Hyper (H-)`

## 2. 核心目录

- [init.el](../init.el)
  主入口。
- [early-init.el](../early-init.el)
  提前做启动优化。
- [lisp/](../lisp/)
  主配置模块。
- [lisp/lang/](../lisp/lang/)
  语言专项配置。
- [bootstrap.el](../bootstrap.el)
  依赖导出/恢复入口。
- [package-lock.el](../package-lock.el)
  锁文件。
- [var/](../var/)
  运行时状态目录。备份、自动保存、eln-cache、transient、projectile、dirvish 等状态都集中放这里。
- [docs/](.)
  这套使用文档。

## 3. 首次安装

在配置目录执行：

```sh
make install
```

或者：

```sh
emacs --debug-init -q -l ./bootstrap.el
```

`bootstrap.el` 会自动判断：

- 机器上已有较多第三方包：导出新的 `package-lock.el`
- 新环境：按 `package-lock.el` 恢复依赖

直接启动 `init.el` 时，如果检测到本地几乎还没有第三方包，也会先按
`package-lock.el` 补齐依赖，避免在 theme、modeline 之类的首批模块上中途失败。
不过首装依然建议优先跑一次 `make install`。

## 4. 必装外部依赖

### 基础工具

- `git`
- `ripgrep`，用于 `consult-ripgrep`、`rg`
- `fzf`，用于 `fzf` 集成
- `hunspell`，用于拼写检查
- `terminal-notifier`，macOS 通知
- `gls`，推荐通过 coreutils 提供，供 `dired` / `dirvish` 使用

### 编程相关

- `clangd`
- `rust-analyzer`
- Python 语言服务器
  这套配置走 `eglot`，你需要自己装对应 server
- `vale`
  如果要用写作 lint
- `vscode-html-language-server`
  用于 HTML / Vue HTML

### Org / 学术写作

- `xelatex`
- `dvisvgm`
- [tools/org-xdvisvgm-hires](../tools/org-xdvisvgm-hires) 依赖链能正常执行

### 终端 / 远程

- `zsh`
- `ssh`
- `~/.ssh/config`
  `my/vterm-ssh` 会优先读这里的 Host

## 5. 字体依赖

当前配置直接引用这些字体：

- `Merriweather`
- `Fira Code`
- `Excalifont`
- `FZLiuGongQuanKaiShuJF`
- `JetBrainsMono Nerd Font`

缺字体不一定阻止启动，但界面观感会明显变化。

## 6. 路径约定

### Org

[lisp/init-org.el](../lisp/init-org.el) 里默认写死：

- `~/HC/Org/`
- `~/HC/Org/roam/`
- `~/HC/Org/daily/`

### GPT

[lisp/init-gpt.el](../lisp/init-gpt.el) 默认读取：

- `etc/mygpt.json`

现在推荐用多后端格式，旧的单后端格式也继续兼容。

格式示例：

```json
{
  "default_backend": "OpenAI",
  "default_model": "your-openai-model",
  "backends": [
    {
      "name": "OpenAI",
      "type": "openai",
      "key": "env:OPENAI_API_KEY",
      "model": "your-openai-model",
      "models": ["your-openai-model"],
      "stream": true
    },
    {
      "name": "Ollama",
      "type": "ollama",
      "host": "localhost:11434",
      "model": "your-local-model",
      "stream": true
    }
  ]
}
```

常见类型：

- `openai`
- `gemini`
- `anthropic`
- `ollama`
- `copilot`

额外说明：

- `key` 支持直接写明文，也支持 `env:ENV_NAME`
- `key` 也支持 `file:relative/path.txt`
- 没有 `etc/mygpt.json` 时，会回退到 gptel 自带的默认 ChatGPT 后端，不再启动时报 warning

### 运行时状态目录

配置已经统一把容易污染项目目录的文件收到了 [var/](../var/)：

- backup
- auto-save
- eln-cache
- lockfiles
- tramp
- company / copilot / projectile / transient / dirvish / treemacs 等状态目录

编译和清理现在统一走：

- `M-x my/compile-board`
- `SPC c b`

## 7. 启动后先确认什么

建议启动 Emacs 后依次确认：

1. 主题和字体是否正常
2. `M-x org-agenda` 是否能打开
3. `C-x C-f` / `C-x b` / `C-s` 是否符合预期
4. `M-x my/vterm-ssh` 是否能读到 SSH 主机
5. `M-x gptel` 是否能成功创建会话
6. `SPC l l` / `C-c l l` 是否能打开 LLM 菜单
7. `C-x g` 是否能打开 Magit

## 8. 下一步看什么

- 日常使用：看 [daily-usage.md](daily-usage.md)
- Org：看 [org-guide.md](org-guide.md)
- 编程 / 远程：看 [dev-guide.md](dev-guide.md)
- 想自己改：看 [settings-cookbook.md](settings-cookbook.md)
