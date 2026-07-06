;;; beancount-entry.el --- Minibuffer entry wizard for bean-tool add -*- lexical-binding: t -*-

;;; Commentary:
;; `my/beancount-add-entry' / `my/beancount-add-split' walk the user
;; through a small completing-read wizard, then pipe the resulting JSON
;; spec to `bean-tool add --stdin' (see tools/add.py for the templates:
;; simple/split/transfer).

;;; Code:

(require 'json)
(require 'beancount-tool)
(require 'beancount-dashboard)

(defvar my/beancount--completions-cache nil
  "Cached result of `bean-tool completions --json'.")

(defvar my/beancount--last-funding-account nil
  "Last funding/source account chosen, offered as the default next time.")

(defun my/beancount-completions-refresh ()
  "Asynchronously (re)fill the completions cache used by the entry wizard."
  (interactive)
  (my/beancount-run-tool
   (list "completions")
   (lambda (data)
     (unless (alist-get 'error data)
       (setq my/beancount--completions-cache data)))))

(defun my/beancount--completions ()
  "Return the completions alist, fetching synchronously if the cache is cold."
  (or my/beancount--completions-cache
      (setq my/beancount--completions-cache
            (my/beancount-run-tool-sync (list "completions")))))

(defun my/beancount--read-template ()
  "Prompt for which `bean-tool add' template to use; return its name string."
  (let ((choice (completing-read "模板: "
                                  '("记账 (simple)" "分账 (split)" "转账 (transfer)")
                                  nil t)))
    (cond ((string-prefix-p "记账" choice) "simple")
          ((string-prefix-p "分账" choice) "split")
          (t "transfer"))))

(defun my/beancount--read-date ()
  "Prompt for a transaction date, defaulting to today."
  (let ((default (format-time-string "%Y-%m-%d")))
    (read-string (format "日期 (默认 %s): " default) nil nil default)))

(defun my/beancount--read-amount (prompt)
  "Prompt for a positive decimal amount string."
  (let ((input (read-string (format "%s: " prompt))))
    (unless (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" input)
      (user-error "金额格式不对: %s" input))
    input))

(defun my/beancount--read-currency (currencies)
  "Prompt for a currency, defaulting to AUD."
  (let ((default "AUD"))
    (completing-read (format "币种 (默认 %s): " default)
                      (or currencies '("AUD" "CNY")) nil nil nil nil default)))

(defun my/beancount--read-payee (payees)
  "Prompt for a payee, allowing free text not in PAYEES."
  (completing-read "商家/对方: " payees))

(defun my/beancount--read-account (prompt accounts &optional default)
  "Prompt for an account among ACCOUNTS, allowing free text."
  (completing-read (format "%s: " prompt) accounts nil nil nil nil default))

(defun my/beancount--build-spec (template)
  "Run the completing-read wizard for TEMPLATE.
Return the JSON-ready spec alist for `bean-tool add'."
  (let* ((completions (my/beancount--completions))
         (accounts (alist-get 'accounts completions))
         (payees (alist-get 'payees completions))
         (currencies (alist-get 'currencies completions))
         (date (my/beancount--read-date))
         (payee (my/beancount--read-payee payees))
         (narration (read-string "备注: "))
         (currency (my/beancount--read-currency currencies)))
    (pcase template
      ("transfer"
       (let* ((amount (my/beancount--read-amount "金额"))
              (from (my/beancount--read-account "转出账户" accounts
                                                 my/beancount--last-funding-account))
              (to (my/beancount--read-account "转入账户" accounts)))
         (setq my/beancount--last-funding-account from)
         `((template . "transfer") (date . ,date) (payee . ,payee)
           (narration . ,narration) (amount . ,amount) (currency . ,currency)
           (from_account . ,from) (to_account . ,to))))
      (_
       (let* ((amount (my/beancount--read-amount "金额"))
              (expense (my/beancount--read-account "支出账户" accounts))
              (funding (my/beancount--read-account "支付账户" accounts
                                                    my/beancount--last-funding-account)))
         (setq my/beancount--last-funding-account funding)
         `((template . ,template) (date . ,date) (payee . ,payee)
           (narration . ,narration) (amount . ,amount) (currency . ,currency)
           (expense_account . ,expense) (funding_account . ,funding)))))))

(defun my/beancount--visit-written (result)
  "Jump to the location bean-tool reports it wrote RESULT at."
  (let ((filename (alist-get 'filename result))
        (lineno (alist-get 'lineno result)))
    (when (and filename (file-exists-p filename))
      (let ((buf (find-file-noselect filename)))
        (pop-to-buffer buf '(display-buffer-below-selected))
        (goto-char (point-min))
        (when lineno (forward-line (1- lineno)))))))

(defun my/beancount--submit-spec (spec)
  "Serialize SPEC to JSON, pipe it to bean-tool add, and report the result."
  (my/beancount-run-tool
   (list "add" "--stdin")
   (lambda (result)
     (cond
      ((alist-get 'error result)
       (message "bean-tool add 失败: %s" (alist-get 'error result)))
      (t
       (let* ((check (alist-get 'check result))
              (ok (my/beancount--json-true-p (alist-get 'ok check))))
         (if ok
             (message "已写入 %s:%s" (alist-get 'filename result) (alist-get 'lineno result))
           (message "已写入,但 check 报错,跳转查看")
           (my/beancount--visit-written result))
         (my/beancount-dashboard-refresh)))))
   :stdin (json-serialize spec)))

(defun my/beancount-add-entry ()
  "Interactively build and append a Beancount transaction via bean-tool."
  (interactive)
  (my/beancount--submit-spec (my/beancount--build-spec (my/beancount--read-template))))

(defun my/beancount-add-split ()
  "Like `my/beancount-add-entry', seeded directly with the roommate split template."
  (interactive)
  (my/beancount--submit-spec (my/beancount--build-spec "split")))

(provide 'beancount-entry)
;;; beancount-entry.el ends here
