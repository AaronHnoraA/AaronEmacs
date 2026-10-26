;;; lang/beancount/entry.el --- Minibuffer entry wizard for bean-tool add -*- lexical-binding: t -*-

;;; Commentary:
;; `+beancount/add-entry' / `+beancount/add-split' walk the user
;; through a small completing-read wizard, then pipe the resulting JSON
;; spec to `bean-tool add --stdin' (see tools/add.py for the templates:
;; simple/split/transfer).

;;; Code:

(require 'json)
(require 'lang/beancount/tool)
(require 'lang/beancount/dashboard)

(defvar +beancount--completions-cache nil
  "Cached result of `bean-tool completions --json'.")

(defvar +beancount--last-funding-account nil
  "Last funding/source account chosen, offered as the default next time.")

(defun +beancount/completions-refresh ()
  "Asynchronously (re)fill the completions cache used by the entry wizard."
  (interactive)
  (+beancount-run-tool
   (list "completions")
   (lambda (data)
     (unless (alist-get 'error data)
       (setq +beancount--completions-cache data)))))

(defun +beancount--completions ()
  "Return the completions alist, fetching synchronously if the cache is cold."
  (or +beancount--completions-cache
      (setq +beancount--completions-cache
            (+beancount-run-tool-sync (list "completions")))))

(defun +beancount--read-template ()
  "Prompt for which `bean-tool add' template to use; return its name string."
  (let ((choice (completing-read "模板: "
                                  '("记账 (simple)" "分账 (split)" "转账 (transfer)")
                                  nil t)))
    (cond ((string-prefix-p "记账" choice) "simple")
          ((string-prefix-p "分账" choice) "split")
          (t "transfer"))))

(defun +beancount--read-date ()
  "Prompt for a transaction date, defaulting to today."
  (let ((default (format-time-string "%Y-%m-%d")))
    (read-string (format "日期 (默认 %s): " default) nil nil default)))

(defun +beancount--read-amount (prompt)
  "Prompt for a positive decimal amount string."
  (let ((input (read-string (format "%s: " prompt))))
    (unless (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)?\\'" input)
      (user-error "金额格式不对: %s" input))
    input))

(defun +beancount--read-currency (currencies)
  "Prompt for a currency, defaulting to AUD."
  (let ((default "AUD"))
    (completing-read (format "币种 (默认 %s): " default)
                      (or currencies '("AUD" "CNY")) nil nil nil nil default)))

(defun +beancount--read-payee (payees)
  "Prompt for a payee, allowing free text not in PAYEES."
  (completing-read "商家/对方: " payees))

(defun +beancount--read-account (prompt accounts &optional default)
  "Prompt for an account among ACCOUNTS, allowing free text."
  (completing-read (format "%s: " prompt) accounts nil nil nil nil default))

(defun +beancount--build-spec (template)
  "Run the completing-read wizard for TEMPLATE.
Return the JSON-ready spec alist for `bean-tool add'."
  (let* ((completions (+beancount--completions))
         (accounts (alist-get 'accounts completions))
         (payees (alist-get 'payees completions))
         (currencies (alist-get 'currencies completions))
         (date (+beancount--read-date))
         (payee (+beancount--read-payee payees))
         (narration (read-string "备注: "))
         (currency (+beancount--read-currency currencies)))
    (pcase template
      ("transfer"
       (let* ((amount (+beancount--read-amount "金额"))
              (from (+beancount--read-account "转出账户" accounts
                                                 +beancount--last-funding-account))
              (to (+beancount--read-account "转入账户" accounts)))
         (setq +beancount--last-funding-account from)
         `((template . "transfer") (date . ,date) (payee . ,payee)
           (narration . ,narration) (amount . ,amount) (currency . ,currency)
           (from_account . ,from) (to_account . ,to))))
      (_
       (let* ((amount (+beancount--read-amount "金额"))
              (expense (+beancount--read-account "支出账户" accounts))
              (funding (+beancount--read-account "支付账户" accounts
                                                    +beancount--last-funding-account)))
         (setq +beancount--last-funding-account funding)
         `((template . ,template) (date . ,date) (payee . ,payee)
           (narration . ,narration) (amount . ,amount) (currency . ,currency)
           (expense_account . ,expense) (funding_account . ,funding)))))))

(defun +beancount--visit-written (result)
  "Jump to the location bean-tool reports it wrote RESULT at."
  (let ((filename (alist-get 'filename result))
        (lineno (alist-get 'lineno result)))
    (when (and filename (file-exists-p filename))
      (let ((buf (find-file-noselect filename)))
        (pop-to-buffer buf '(display-buffer-below-selected))
        (goto-char (point-min))
        (when lineno (forward-line (1- lineno)))))))

(defun +beancount--submit-spec (spec)
  "Serialize SPEC to JSON, pipe it to bean-tool add, and report the result."
  (+beancount-run-tool
   (list "add" "--stdin")
   (lambda (result)
     (cond
      ((alist-get 'error result)
       (message "bean-tool add 失败: %s" (alist-get 'error result)))
      (t
       (let* ((check (alist-get 'check result))
              (ok (+beancount--json-true-p (alist-get 'ok check))))
         (if ok
             (message "已写入 %s:%s" (alist-get 'filename result) (alist-get 'lineno result))
           (message "已写入,但 check 报错,跳转查看")
           (+beancount--visit-written result))
         (+beancount/dashboard-refresh)))))
   :stdin (json-serialize spec)))

(defun +beancount/add-entry ()
  "Interactively build and append a Beancount transaction via bean-tool."
  (interactive)
  (+beancount--submit-spec (+beancount--build-spec (+beancount--read-template))))

(defun +beancount/add-split ()
  "Like `+beancount/add-entry', seeded directly with the roommate split template."
  (interactive)
  (+beancount--submit-spec (+beancount--build-spec "split")))

(provide 'lang/beancount/entry)
;;; lang/beancount/entry.el ends here
