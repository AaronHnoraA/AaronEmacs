# TODO
这个文件用于收集emacs 的一些想要实现的功能和bugs, 如果不是不可接受的问题尽量收集起来统一解决/实现

## Emacs

### Features
- 美化 emacs的界面: nerd, unicode, emoji
- AI融合
  - send 不直接提交
  - 权限严格隔离: ask 的时候不允许改动, 要求改动的时候不需要审批
  - 生命周期管理, 内存管理, buffer管理. 能完整控制, 停止,追踪等行为

###  Bugs

---

##  Aaronnote && Roam

### Features
- 脱离链接附件检测

### Bugs
- copilot 补全不是很灵活 模仿vsocde补全的逻辑. 现在如果后面有字符 不管是括号还是copilot补全都会失效
- snippet不应该跟随光标实时检测, 应该是键入后再检测