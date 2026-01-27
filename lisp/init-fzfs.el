(use-package fzf
  :ensure t
  :bind
  ;; 绑定快捷键，并确保它作用于当前目录 (default-directory)
  ("C-c z" . (lambda () (interactive) (fzf default-directory)))
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nrH"
        fzf/position-bottom t
        fzf/window-height 15))

;; 这是一个更智能的调用方式
(defun my/fzf-at-current-dir ()
  (interactive)
  ;; 如果在项目里，就从项目根目录开始；否则从当前目录开始
  (let ((path (or (project-root (project-current)) default-directory)))
    (fzf path)))

;; 然后在 :bind 中使用
;; ("C-c f" . my/fzf-at-current-dir)


(provide 'init-fzfs)
