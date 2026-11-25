# Sacha Chua 的 Emacs 语音输入

第一张参考图左侧的条目是 Sacha Chua 的「Yay Emacs 55 · Speech mode switching」。右侧是同时打开的 Emacs Lisp 源码。语音项目单独维护在 `speech-input`。阅读所用的提交是 `09c103b4dabdf61ecd7f944dd5470f47274dd1db`。 [直播记录](https://sachachua.com/topic/live/#orge5aaf00) · [源码](https://codeberg.org/sachac/speech-input/src/commit/09c103b4dabdf61ecd7f944dd5470f47274dd1db)

## 分层

| 层 | 源码 | 观察 |
| --- | --- | --- |
| 录音与语音活动 | `speech-input.el`、`speech-input-vad.el`、`vad-events.py` | 外部进程报告开始和结束，Emacs 用 hook 处理事件。一次录音将文件交给回调。 |
| 转写 | `speech-input-transcribe.el`、`speech-input-speaches.el` | 音频文件经可配置的接口转为文本。默认接口为本机地址，默认模型是英语变体。 |
| 匹配与插入 | `speech-input.el`、`speech-input-commands.el` | 命令表匹配和文本插入分开，可在决定动作前保留转写文本。 |
| 可选输入路径 | `speech-input-phony.el`、`speech-input-nerd-dictation.el` | Phony 在插入前处理能识别的文本命令。Nerd Dictation 从外部进程的输出获取文字。 |

把录音、转写和命令选择分开，能在校正转写时保留已有编辑状态。用于中文时，`speech-input-normalize-string` 的匹配路径会把 ASCII 字母、数字和空格以外的字符换成空格，例如「打开目录」归一化后没有可用于匹配的词。默认录音命令指定 FFmpeg 的 PulseAudio 输入。本次核查覆盖源码和文字处理测试，当前 Mac 的音频路径尚待实测。

## 核查

`emacs -Q --batch -L . -l speech-input.el -l speech-input-test.el -f ert-run-tests-batch-and-exit` 在上述提交运行 3 项，1 项通过。`speech-input-match-all` 的实际返回值包含匹配列表和剩余文字，测试期望的是单个列表。`speech-input-match-in-list` 的测试传入 3 个参数，函数签名接受 2 个。
