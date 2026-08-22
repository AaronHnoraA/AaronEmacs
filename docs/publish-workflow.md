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

## 站点结构:一份文档,两种布局

`index.html` 就是整个站点 —— 各个 section 保持正常文档流。没有 JavaScript、没有 WebGL、或带
`?flat=1` 时直接按这份平面文档呈现;顶栏按钮可显式在两种模式间切换。窄屏仍默认进入 3D,
系统要求减弱动态时保留 3D 构图但只画静止帧。

条件允许时,`assets/js/site.js` 才把这些 section **提升**进 3D 世界:连续采样的双叶闭环承载
两组共八个抽象 qubit,首次进入默认跟随先导 qubit。`states.js` 是预先写好的视觉状态播放表,
浏览器只查表和插值,不实时模拟量子态。门、步骤公式与各 qubit 表达式只作为贴着对象的轻量
CSS3D 注记出现,不写进个人主页正文,平面 fallback 也不加载这些注记。
section 变成站在世界里的 CSS3D 面板 —— 仍是真 HTML,可选中、可读屏。点击面板,相机会飞到它
正前方对齐后才展开正文,文字没有任何透视变形;进入阅读位后滚轮、触摸和键盘输入不会回溢
到世界,必须显式退出。顶部导航和底部飞行控件都能定位现有站点。

两种布局不复制任何内容,没有第二份正文要同步。

### 修改或新增卡片

面向访客的正文只写在 `index.html`。`main[data-world]` 下每个带唯一 `id` 的直接
`section.panel` 会自动成为平面 section、CSS3D 卡片和底部站点;不需要同步 JS 注册表。

- `data-world-label` 可指定底部的短标签。
- `data-world-t="0..1"`、`data-world-lift`、`data-world-side="-1|1"` 可微调构图;
  省略 `data-world-t` 时自动放进闭环最大空段。
- 只有需要常驻顶栏的主 section 才手写一条 masthead 链接。
- 单纯新增正文卡片不需要改发布文件清单。

调试用的查询参数:`?flat=1` 强制平面,`?static=1` 只渲染一帧,`?debug=1` 把运行中的场景挂到
`window.__world`;`?head=0.25&flow=0.5` 可分别固定相机与量子流的位置,供可复现截图使用。

## 完整性检查

部署前 `my/noema-publish--check-site` 会确认这些文件存在,缺一个就报错退出:

- 页面与样式:`index.html`、`assets/css/site.css`、`assets/js/site.js`
- 世界模块:`assets/js/world/` 下的 `index`、`curve`、`circuit`、`flight`、`rig`、
  `panels`、`states`、`css3d`、`math`
- vendored 依赖:`vendor/three/three.module.min.js`、`vendor/three/three.core.min.js`、
  `vendor/three/CSS3DRenderer.js`、`vendor/anime/anime.esm.min.js`、
  `vendor/katex/katex.mjs`、`vendor/katex/katex.min.css` 与数学字体
- 许可证:`LICENSE`、`vendor/three/LICENSE`、`vendor/anime/LICENSE.md`、
  `vendor/katex/LICENSE`

许可证是**硬性**检查项:vendored 的库缺了许可证文件就是违反其授权条款,所以宁可让发布失败。
`three.core.min.js` 与 `CSS3DRenderer.js` 同为硬性项 —— 前者被 `three.module.min.js`
`import`,后者被页面 `import`,漏掉任何一个都是线上白屏而不是降级。KaTeX 只随
3D 世界模块加载,静态个人页面不加载公式运行时。

同一份清单在 `.github/workflows/static.yml` 里再校验一次。改动其一必须同步另一处。

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

新增任何第三方依赖时,三件事一起做:vendored 文件 + 原始许可证文件 + README 中的依赖说明,
并把许可证路径加进 `my/noema-publish--required-licences`。许可证与实现说明留在仓库,不再作为
个人学术主页卡片展示。

## 本地预览

页面用了 ES module 与 importmap,`file://` 打不开,必须起 http:

```sh
python3 -m http.server 8137 -d ~/.emacs.d/publish
```

`?static=1` 可以强制渲染静态那一帧,等同于系统开启「减弱动态效果」或没有 WebGL 时的表现。

## 本地预览与自查

页面用了 ES module 与 importmap,`file://` 打不开,必须起 http:

```sh
python3 -m http.server 8137 -d ~/.emacs.d/publish
```

改动 3D 世界后,值得实测而不是靠肉眼在单帧上判断的三件事:

1. **构图**:沿环取若干相位各截一张图连起来看,单帧很容易调过拟合。
2. **接缝**:在 `t = 0` 两侧各取一步,位移应与环上任意一点的同样一步无异 —— 这才叫首尾相接。
3. **阅读位**:点开面板后,相机视线与面板法向的夹角应接近 0°,否则文字会带透视变形。

## 历史

2026-08 之前,本站是由一个 Python 引擎(`Noema/publish/publish-site`)把 org-roam
笔记渲染成静态站,首页是 reveal.js 幻灯片。笔记发布这条线已经废弃 —— 笔记现在由独立的
wiki 承担,引擎、旧资源与已发布的笔记页面都已删除。同期首页也从"页面里的一条 3D 横幅"
改成"3D 世界即页面"。
