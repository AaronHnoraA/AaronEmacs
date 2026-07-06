;;; beancount-dashboard.el --- Beancount dashboard buffer -*- lexical-binding: t -*-

;;; Commentary:
;; The right-hand pane opened by `my/beancount': a magit-section buffer
;; showing this month's expenses, roommate balances and recent
;; transactions, all driven by `bean-tool summary --json'.

;;; Code:

(require 'magit-section)
(require 'cl-lib)
(require 'beancount-tool)

(declare-function my/beancount-add-entry "beancount-entry")
(declare-function my/beancount-add-split "beancount-entry")
(declare-function my/beancount-sync "beancount-frame")
(declare-function my/beancount-fava-reload "beancount-frame")

(defconst my/beancount-dashboard-buffer-name "*beancount-dashboard*")

(defvar-local my/beancount--dashboard-summary nil
  "Last JSON summary alist rendered in this buffer.")

(defun my/beancount--json-true-p (value)
  "Return non-nil if VALUE is JSON true (parsed as Lisp `t')."
  (eq value t))

(defun my/beancount--format-amount (amount)
  "Format an {number, currency} AMOUNT alist as \"123.42 AUD\"."
  (if amount
      (format "%s %s" (alist-get 'number amount) (alist-get 'currency amount))
    "-"))

(defun my/beancount--format-account-row (row)
  "Format a {account, amounts} ROW alist as an indented display line."
  (let* ((account (alist-get 'account row))
         (amounts (alist-get 'amounts row))
         (text (mapconcat #'my/beancount--format-amount amounts "  ")))
    (format "  %-42s %s" account text)))

(defun my/beancount--format-txn-row (txn)
  "Format a recent-transaction TXN alist as a display line."
  (let* ((date (alist-get 'date txn))
         (payee (or (alist-get 'payee txn) ""))
         (narration (or (alist-get 'narration txn) ""))
         (amount (alist-get 'amount txn))
         (links (alist-get 'links txn)))
    (format "  %s  %s %s  %s%s"
            (if (and date (>= (length date) 10)) (substring date 5) (or date ""))
            payee narration
            (my/beancount--format-amount amount)
            (if links (concat "  ^" (mapconcat #'identity links " ^")) ""))))

(defun my/beancount--dashboard-insert (data)
  "Render the parsed summary DATA into the current (erased) buffer."
  (let* ((month (alist-get 'month data))
         (check (alist-get 'check data))
         (ok (my/beancount--json-true-p (alist-get 'ok check)))
         (errors (alist-get 'errors check))
         (expenses (alist-get 'expenses data))
         (expense-total (alist-get 'expense_total data))
         (roommates (alist-get 'roommates data))
         (recent (alist-get 'recent data)))
    (magit-insert-section (my/beancount-root)
      (insert (propertize "我的账本" 'face 'bold)
              (format "  %s  " (or month ""))
              (if ok
                  (propertize "✓ check 通过" 'face 'success)
                (propertize (format "✗ check %d 处错误" (length errors)) 'face 'error))
              "\n\n")
      (when (and (not ok) errors)
        (magit-insert-section (my/beancount-errors nil)
          (magit-insert-heading "校验错误")
          (magit-insert-section-body
            (dolist (err errors)
              (magit-insert-section
                  (my/beancount-txn (cons (alist-get 'filename err) (alist-get 'lineno err)))
                (insert (format "  %s:%s %s\n"
                                (or (alist-get 'filename err) "?")
                                (or (alist-get 'lineno err) "?")
                                (alist-get 'message err)))))
            (insert "\n"))))
      (magit-insert-section (my/beancount-expenses nil)
        (magit-insert-heading
          (format "本月支出  %s"
                  (mapconcat #'my/beancount--format-amount expense-total "  ")))
        (magit-insert-section-body
          (dolist (row expenses)
            (insert (my/beancount--format-account-row row) "\n"))
          (insert "\n")))
      (magit-insert-section (my/beancount-roommates nil)
        (magit-insert-heading "室友余额")
        (magit-insert-section-body
          (dolist (row roommates)
            (insert (my/beancount--format-account-row row) "\n"))
          (insert "\n")))
      (magit-insert-section (my/beancount-recent nil)
        (magit-insert-heading "最近流水")
        (magit-insert-section-body
          (dolist (txn recent)
            (magit-insert-section
                (my/beancount-txn (cons (alist-get 'filename txn) (alist-get 'lineno txn)))
              (insert (my/beancount--format-txn-row txn) "\n")))
          (insert "\n"))))))

(defun my/beancount-dashboard-buffer ()
  "Return the dashboard buffer, creating and initializing it if needed."
  (let ((buffer (get-buffer-create my/beancount-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'my/beancount-dashboard-mode)
        (my/beancount-dashboard-mode)))
    buffer))

(defun my/beancount-dashboard-refresh ()
  "Refresh the Beancount dashboard from `bean-tool summary --json'."
  (interactive)
  (let ((buffer (my/beancount-dashboard-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "(刷新中…)\n")))
    (my/beancount-run-tool
     (list "summary")
     (lambda (data)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((inhibit-read-only t)
                 (pos (point)))
             (erase-buffer)
             (if (alist-get 'error data)
                 (insert (propertize (format "bean-tool 出错: %s\n" (alist-get 'error data))
                                      'face 'error))
               (setq my/beancount--dashboard-summary data)
               (my/beancount--dashboard-insert data))
             (goto-char (min pos (point-max))))))))
    buffer))

(defun my/beancount-dashboard-visit ()
  "Jump to the ledger location of the transaction or error at point."
  (interactive)
  (let* ((section (magit-current-section))
         (value (and section
                     (eq (oref section type) 'my/beancount-txn)
                     (oref section value))))
    (unless value
      (user-error "Nothing to visit at point"))
    (pcase-let ((`(,filename . ,lineno) value))
      (unless (and filename (file-exists-p filename))
        (user-error "No source location for this entry"))
      (let ((buf (find-file-noselect filename)))
        (pop-to-buffer buf '(display-buffer-below-selected))
        (goto-char (point-min))
        (when lineno (forward-line (1- lineno)))))))

(define-derived-mode my/beancount-dashboard-mode magit-section-mode "Beancount-Dashboard"
  "Major mode for the Beancount dashboard buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(define-key my/beancount-dashboard-mode-map (kbd "g") #'my/beancount-dashboard-refresh)
(define-key my/beancount-dashboard-mode-map (kbd "a") #'my/beancount-add-entry)
(define-key my/beancount-dashboard-mode-map (kbd "s") #'my/beancount-add-split)
(define-key my/beancount-dashboard-mode-map (kbd "RET") #'my/beancount-dashboard-visit)
(define-key my/beancount-dashboard-mode-map (kbd "S") #'my/beancount-sync)
(define-key my/beancount-dashboard-mode-map (kbd "G") #'my/beancount-fava-reload)
(define-key my/beancount-dashboard-mode-map (kbd "q") #'quit-window)

(provide 'beancount-dashboard)
;;; beancount-dashboard.el ends here
