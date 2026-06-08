# TODO 
这个文件用于收集emacs 的一些想要实现的功能和bugs, 如果不是不可接受的问题尽量收集起来统一解决/实现

## Emacs

### Features
- 美化 emacs的界面: nerd, unicode, emoji 
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
- vim在中文输入法下的适配能力
- 深化表格能力
- 附件M-click跳转能力
- 接受 <!-- omit in toc -->语法

### Bugs
- graphview 鼠标滚轮问题
- TOC pop 窗口大小调整问题
- tab/shift tab 现在在正常状态下不是制表而是奇怪的光标跳转, 有时候又是制表,有点奇怪的 就是不太稳定, 需要修复成稳定的行为
- 英文字体被中文字体覆盖,但是修复的时候不要有覆盖掉中文字体
- vim行末尾a 或者 i会换行
- url会直接被解析隐藏 图片资源url也会被解析为link在光标上去的时候. 应该是按照[ ] \(\) 这样来解析而不是看url


