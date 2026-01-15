;;; "Compiled" snippets and support files for `sh-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
					 '(("shebang" "#!/bin/bash\n\n${1}\n" "shebang"
						nil nil nil
						"/Users/hc/.emacs.d/snippets/sh-mode/shebang_shebang"
						nil nil)
					   ("noti"
						"osascript -e 'display notification \"${1:body}\" with title \"${2:title}\"'\n"
						"notification" nil nil nil
						"/Users/hc/.emacs.d/snippets/sh-mode/noti_notification"
						nil nil)
					   ("multiline"
						"configs=\\$(\n	cat <<EOF\n${1:line1\nline2}\nEOF\n)\n\nwhile IFS= read -r line; do\n	echo \"Processing: \\$line\"\ndone <<<\"\\$configs\"\n"
						"multiline" nil nil nil
						"/Users/hc/.emacs.d/snippets/sh-mode/multiline_multiline"
						nil nil)
					   ("arg1"
						"if [ ! \\$# -gt 0 ]; then\n	echo \"No argument provided\"\n	exit 1\nfi\n\n${1}\n"
						"arg1" nil nil nil
						"/Users/hc/.emacs.d/snippets/sh-mode/arg1_arg1"
						nil nil)))


;;; Do not edit! File generated at Wed Jan 14 13:15:18 2026
