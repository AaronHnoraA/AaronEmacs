# TODO 
这个文件用于收集emacs 的一些想要实现的功能和bugs, 如果不是不可接受的问题尽量收集起来统一解决/实现

## Emacs

### Features
- 美化 emacs的界面: nerd, unicode, emoji 
- Jupyter xweight支持
- AI融合
  - send 不直接提交
  - 权限严格隔离: ask 的时候不允许改动, 要求改动的时候不需要审批 
  - 生命周期管理, 内存管理, buffer管理. 能完整控制, 停止,追踪等行为
   

###  Bugs
 - space p . p 打开新项目后不要进入现在的选择文件界面, 而是直接进入新项目的根目录
 
---

##  Aaronnote && Roam

### Features
- roam 发布笔记
- 脱离链接附件检测
- vim在中文输入法下的适配能力
- 深化表格能力(参考OB - Markup插件)
- 附件M-click跳转能力
- 接受 `<!-- omit in toc -->`语法


### Bugs
- TOC pop 窗口大小调整问题
- roam node的local greph 报错(似乎是hidden的那些还有什么的 但是其实这些再roam里面是存在的至少tag是存在的也有局部的link关系图)
- 表格/html `<img>` 中对图片和../支持不健全

