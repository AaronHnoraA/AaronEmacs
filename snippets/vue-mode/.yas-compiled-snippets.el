;;; "Compiled" snippets and support files for `vue-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'vue-mode
                     '(("vue"
                        "<template>\n  <p>{{ msg }}</p>\n</template>\n\n<script>\nexport default {\n  data() {\n    return {\n      msg: \"magic sytax\",\n      ok: true,\n    }\n  }\n}\n</script>\n"
                        "vue" nil nil nil
                        "/Users/hc/.emacs.d/snippets/vue-mode/vue" nil nil)
                       ("data"
                        "data() {\n  return {\n    msg: \"data\",\n    ok: true\n  }\n}\n"
                        "data" nil nil nil
                        "/Users/hc/.emacs.d/snippets/vue-mode/data" nil nil)))


;;; Do not edit! File generated at Wed Feb 18 00:24:07 2026
