# TODO 
这个文件用于收集emacs 的一些想要实现的功能和bugs, 如果不是不可接受的问题尽量收集起来统一解决/实现

## Emacs

### Features

- 美化 emacs的界面
- Jupyter 支持
- AI融合
  - send 不直接提交
  - 权限严格隔离: ask 的时候不允许改动, 要求改动的时候不需要审批
  - 生命周期管理, 内存管理, buffer管理. 能完整控制, 停止,追踪等行为

###  Bugs
- lean刚进入的时候卡顿, 目测为lean infoview的启动问题, 应当做异步 或者预处理

  
---

##  Aaronnote && Roam

### Features

- roam 发布笔记
- 脱离链接附件检测


### Bugs

- graphview 鼠标滚轮问题
- aarnote 下面卡中文输入法中英文enter问题 输入法输入英文的时候enter按道理应该是要输入英文但是这个enter会同时被appine吃掉变成激活snippet的, 我研究出来了,这个问题是appine的问题, 那么解决方案是换xwidget, 那么你要解决几个额外的问题: 第一, xwidget其他地方比如lean也会依赖,但是xwidget其实是可以多窗口的但是原生只会顶掉之前的,所以需要自己维护api做好tab, 多buffer 能力然后让lean 等适配使用, 然后就是xwidght目前键盘穿透没有做完, 输入delete space等甚至会乱码, 你需要修复好这一点. 解决这两个问题之后, 就可以从默认appine迁移到xwidgt方案了. 这样可以直接解决后面的 snippet过于敏感问题和C-C C-F渗透不稳定问题,认为值得实施
- aaronean面snippe 过于敏感问题
- TOC pop 窗口大小调整问题
- aaronote 一旦close tab就无法再打开了, 需要重启整个aaronote node 进程. 你应该做好生命周期管理的
- C-C C-F渗透有时候会失败, 有时候只会渗透出C-C
- 亮色主题看着有点眼睛不舒服, 这个不知道需不需要优化,s sisiss
  
  
