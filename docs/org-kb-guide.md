# Org 知识库与站点工作流

这份文档讲的不是 Emacs 里的按键，而是 `~/HC/Org/` 这个目录本身怎么被当作：

- 个人知识库
- 研究与写作素材库
- 站点源目录

这套系统的基本约定是：

- 内容主要用 Org 笔记维护
- 日常录入、整理、沉淀都在 Org 里完成
- 最终站点由 Emacs 生成

在本机上，站点发布入口位于：

- `~/HC/Org/publish.el`
- `~/HC/Org/Makefile`

也就是说，这不是“一个额外的网站项目 + 一套单独 CMS”，而是直接从你的 Org 内容树往外发布。

## 1. 总体定位

`~/HC/Org/` 不只是 `org-roam` 目录，也不只是 capture 落点。它承担三层职责：

1. 作为日常记录和研究笔记的主存储
2. 作为长期知识库的源数据
3. 作为个人网站的内容源

最重要的设计原则不是“分类绝对正确”，而是：

- 记录时不要被分类打断
- 整理时不要重写整套结构
- 随时间可以自然演化
- 同一份 Org 内容可以同时服务于笔记和发布

## 2. 目录模型

目前可以把它理解成下面几个区域：

- `daily/`
  当下记录区，先记下来，不要求成熟。
- `roam/`
  长期知识库，放已经想清楚、值得长期回看的内容。
- `attachments/`
  原始材料仓库，只放 PDF、图片、扫描件、邮件附件等文件本体。
- `references/`
  引用、BibTeX 和学术写作相关资源。
- `diary.org`
  线性私人记录，不追求结构化。
- `public/`
  站点生成结果，不是主要写作入口。

如果只记一句话：

- `daily/` 负责先写
- `roam/` 负责沉淀
- `attachments/` 负责存文件
- `public/` 负责发布结果

## 3. `daily/`、`roam/`、`attachments/` 的分工

### `daily/`

`daily/` 是“写的时候不考虑最终分类”的地方。

这里适合放：

- 突然想到的点子
- 临时会议记录
- 学习过程中的中间态笔记
- 还没成熟的研究想法
- 待整理的信息

这里允许：

- 杂乱
- 重复
- 暂时没有结论

### `roam/`

`roam/` 是长期知识库核心区。

这里适合放：

- 已经比较稳定的概念说明
- 方法总结
- 论文笔记
- 主题索引页
- 值得长期引用和链接的节点

这里强调的不是目录层级，而是：

- 双向链接
- tag
- index 页面

### `attachments/`

`attachments/` 不承载“思考”，只承载“文件本体”。

这里适合放：

- PDF
- 图片
- 扫描件
- 邮件附件
- 会议材料

原则很简单：

- 文件本体放 `attachments/`
- 对这些材料的理解、摘要和观点写进 Org 文件

## 4. 推荐工作流

比较稳的工作方式是：

1. 先 capture 到 `daily/`
2. 需要长期保留的内容再整理进 `roam/`
3. 外部材料统一放到 `attachments/`
4. 引用和文献信息收进 `references/`
5. 最终从 Org 内容树生成网站

这套流程的关键不在“第一下就放对地方”，而在：

- 先把内容写下来
- 之后再把高价值内容提炼出来

换句话说：

- `daily/` 面向输入速度
- `roam/` 面向长期复用

## 5. 站点是怎么来的

这套目录同时是站点源目录。

当前模型是：

- 源内容主要是 Org 文件
- Emacs 负责发布
- `org-publish` 是核心发布机制

本机上可以从 `publish.el` 看到这一点：发布项目会把 Org 内容导出到 `~/HC/Org/public/`，同时生成站点所需的数据和页面。

所以这里的站点不是从 Markdown / 数据库存一份、再同步到 Org；而是反过来，直接从 Org 笔记发布。

## 6. Tag 约定

Tag 的目标不是“把所有东西分完类”，而是减少目录压力、增强横向链接。

最重要的规则只有三条：

1. Tag 不等于目录
2. Tag 只回答“这是什么性质的内容”
3. 每个文件的 tag 数量尽量不要太多，建议不超过 5 个

### 推荐分类

可以把 tag 想成几类：

- `type`
  内容类型，例如 `concept`、`method`、`paper`、`idea`、`question`
- `status`
  成熟度或状态，例如 `draft`、`working`、`stable`、`review`
- `domain`
  学科或领域，例如 `math`、`cs`、`qc`
- `activity`
  这份内容主要服务什么活动，例如 `research`、`writing`、`learning`
- `property`
  少量横向属性，例如 `core`、`toolbox`、`formal`、`example`

### 使用原则

- 目录是粗粒度、稳定结构
- tag 是细粒度、灵活结构
- 如果已经在 `roam/math/` 里，就不一定还要再打 `math`
- 不要让 tag 退化成“另一套重复目录”

### 推荐示例

- 数学概念
  `#+filetags: :concept:math:theory:`
- 正在整理的论文笔记
  `#+filetags: :paper:research:draft:`
- 未成熟的原创想法
  `#+filetags: :idea:draft:research:`
- 已稳定的方法总结
  `#+filetags: :method:stable:toolbox:`
- 需要回看的问题
  `#+filetags: :question:review:`

### 不建议的 tag

以下几类最好不要用：

- 时间性 tag
  例如 `2025`、`week3`、`today`
- 情绪性 tag
  例如 `hard`、`boring`、`cool`
- 重复目录的 tag
  例如 `roam`、`daily`、`attachments`

一句话总结：

- 目录负责“住哪”
- tag 负责“你是谁”

## 7. `diary.org` 和 `references/`

这两个区域也有明确分工：

- `diary.org`
  更像线性私人时间轴，不追求可发布、可复用或强链接化。
- `references/`
  服务于引用、论文、BibTeX 和学术写作，不建议和一般笔记混放。

## 8. 这份文档和 `org-guide.md` 的区别

- [org-guide.md](org-guide.md)
  讲 Emacs 里怎么用 Org：入口、capture、roam、LaTeX 预览、UI、按键。
- 本文
  讲 `~/HC/Org/` 作为个人知识库和站点源目录时，该怎么理解它的结构和约定。
