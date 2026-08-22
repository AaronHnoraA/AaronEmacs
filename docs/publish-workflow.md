# 个人主页发布流程

个人学术主页的构建与部署。`make publish*` 这一族目标就在这里。

## 三个仓库,各管一段

| 位置 | 角色 |
|---|---|
| `~/.emacs.d/publish`(→ `~/HC/Org`) | **站点本身**。手写的 HTML/CSS/JS,仓库根就是网站根 |
| `lisp/roam/Noema/publish/CV/main.tex` | CV 的 LaTeX 源(在 Noema 仓库里) |
| `lisp/roam/init-aaronnote-publish.el` | 编译 CV、检查完整性、部署 |

站点没有构建步骤。页面是什么样,提交进去的就是什么样;唯一被生成的产物是 CV 的 PDF。
要改主页,直接编辑 publish 仓库里的文件。

## make 目标

```sh
make publish-build    # 编译 CV,并校验站点文件齐全
make publish          # 上一步 + git add/commit/push + 可选 NAS rsync
make publish-deploy   # 只部署
make publish-clean    # 清掉 CV 编译中间产物
```

这些目标走的是一个精简的 batch Emacs(`PUBLISH_BATCH`),只加载
`init-aaronnote-publish.el`,不加载 `init.el`,所以发布不受配置状态影响。
对应的交互命令是 `my/noema-publish`、`my/noema-publish-build`、
`my/noema-publish-deploy`、`my/noema-publish-clean`。

## 完整性检查

部署前 `my/noema-publish--check-site` 会确认这些文件存在,缺一个就报错退出:

- 页面与样式:`index.html`、`assets/css/site.css`、`assets/js/site.js`、
  `assets/js/hero-circuit.js`
- vendored 依赖:`vendor/three/three.module.min.js`、`vendor/three/three.core.min.js`、
  `vendor/anime/anime.esm.min.js`
- 许可证:`LICENSE`、`vendor/three/LICENSE`、`vendor/anime/LICENSE.md`

许可证是**硬性**检查项:vendored 的库缺了许可证文件就是违反其授权条款,所以宁可让发布失败。
`three.core.min.js` 也是硬性项 —— `three.module.min.js` 会 `import` 它,漏掉就是线上白屏。

同一份清单在 `.github/workflows/static.yml` 里再校验一次。

## 配置项

全部注册在 `config` registry,值在 `etc/config-store.el`:

| 变量 | 含义 |
|---|---|
| `my/noema-publish-root` | publish 仓库根 |
| `my/noema-publish-cv-dir` | CV LaTeX 源目录 |
| `my/noema-publish-state-dir` | CV 编译中间产物 |
| `my/noema-publish-nas-enable` | 是否在 push 后 rsync 到 NAS |
| `my/noema-publish-nas-target` | rsync 目标 |

改这些用 `M-x my/config-board`,不要在代码里 `setq`。

## 部署路径

1. `latexmk -xelatex` 把 CV 编到 `state-dir/cv/`,再拷成 `publish/CV/Aaron_He_CV.pdf`。
2. 在 publish 仓库 `git add -A` + commit(消息为 `site update: <时间戳>`)+ push。
3. 若 `nas-enable` 为真,`rsync -avh --delete` 整个仓库根到 `nas-target`,
   排除 `.git/`、`.github/`、`.DS_Store`。

仓库根即网站根,部署就是一次文件拷贝,没有中间产物目录。

## 许可证约定

- 站点自有代码(为本站编写的 HTML/CSS/JS/SVG):MIT,见 publish 仓库的 `LICENSE`。
- 文字、CV、图片:© Chang He,保留所有权利。
- `vendor/` 下的第三方代码:各自原许可证,原样分发,不得删改许可证头或许可证文件。

新增任何第三方依赖时,三件事一起做:vendored 文件 + 原始许可证文件 +
`credits.html` 里的条目,并把这两个路径加进 `my/noema-publish--required-licences`。

首页那段动画是原创实现;它致敬的作品、以及它所描绘的算法的文献出处,都写在
publish 仓库的 `credits.html` 里。

## 本地预览

页面用了 ES module 与 importmap,`file://` 打不开,必须起 http:

```sh
python3 -m http.server 8137 -d ~/.emacs.d/publish
```

`?static=1` 可以强制渲染静态那一帧,等同于系统开启「减弱动态效果」或没有 WebGL 时的表现。

## 历史

2026-08 之前,本站是由一个 Python 引擎(`Noema/publish/publish-site`)把 org-roam
笔记渲染成静态站,首页是 reveal.js 幻灯片。笔记发布这条线已经废弃 —— 笔记现在由独立的
wiki 承担,引擎、旧资源与已发布的笔记页面都已删除。
