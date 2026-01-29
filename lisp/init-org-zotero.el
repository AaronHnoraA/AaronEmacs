;;; init-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'bibtex)
(require 'subr-x) ; 确保加载 string-trim 等函数

(defun my/org-zotero-fill-metadata ()
  "解析当前 Buffer 中的 BibTeX 代码块，并自动填充 ${title}, ${author} 等占位符。
v3 修复版：彻底解决了 stringp/nil 和 listp 报错问题。"
  (interactive)
  (save-excursion
    (let ((bib-content nil)
          (entries nil)
          (key nil)
          (title nil)
          (author nil)
          (year nil)
          (doi nil))

      ;; 1. 寻找并提取 BibTeX src block 的内容
      (goto-char (point-min))
      (if (search-forward "#+begin_src bibtex" nil t)
          (let ((start (point)))
            (search-forward "#+end_src")
            (setq bib-content (buffer-substring-no-properties start (match-beginning 0))))
        (error "未找到 BibTeX 代码块 (#+begin_src bibtex)"))

      ;; 2. 解析 BibTeX (在临时 Buffer 中进行)
      (with-temp-buffer
        ;; 这里的 mode 设置对于正确解析很重要
        (bibtex-mode)
        (insert bib-content)
        (goto-char (point-min))
        
        ;; 跳过空行，找到条目
        (bibtex-skip-to-valid-entry)
        
        ;; 获取 Citekey
        (setq key (bibtex-key-in-head)) 
        ;; 获取所有字段的原始 alist，例如 (("title" . "{Book Name}") ...)
        (setq entries (bibtex-parse-entry))
        
        ;; --- 内部辅助函数：清洗 BibTeX 值 ---
        (cl-flet ((get-clean-field (field-name)
                    (let* ((entry (assoc field-name entries))
                           (val (if entry (cdr entry) nil)))
                      (if val
                          (progn
                            (setq val (string-trim val))
                            ;; 去除首尾的花括号 { }
                            (when (and (string-prefix-p "{" val) (string-suffix-p "}" val))
                              (setq val (substring val 1 -1)))
                            ;; 去除首尾的引号 " "
                            (when (and (string-prefix-p "\"" val) (string-suffix-p "\"" val))
                              (setq val (substring val 1 -1)))
                            val)
                        nil)))) ;; 字段不存在返回 nil
          
          ;; 提取并清洗字段
          (setq title (get-clean-field "title"))
          (setq author (get-clean-field "author"))
          (setq year (get-clean-field "year"))
          (unless year (setq year (get-clean-field "date")))
          (setq doi (get-clean-field "doi"))))

      ;; 3. 执行替换操作
      (cl-flet ((replace-tag (tag value)
                  (goto-char (point-min))
                  (while (search-forward tag nil t)
                    ;; 如果 value 是 nil，用空字符串 "" 替换，防止报错
                    (replace-match (or value "") t t))))
        
        (replace-tag "${title}" title)
        (replace-tag "${author}" author)
        (replace-tag "${year}" year)
        (replace-tag "${citekey}" key)
        (replace-tag "${doi}" doi)
        
        (message "元数据填充完成：Title: %s" (or title "未找到标题"))))))



(provide 'init-org-zotero)
;;; init-utils.el ends here
