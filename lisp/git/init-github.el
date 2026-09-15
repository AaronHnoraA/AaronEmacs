;;; init-github.el --- GitHub CLI integration -*- lexical-binding: t; -*-

;;; Commentary:
;; User-invoked GitHub operations routed through the current logical target.

;;; Code:

(require 'browse-url)
(require 'config)
(require 'json)
(require 'magit-git)
(require 'remote-fs)
(require 'remote-process)
(require 'seq)
(require 'subr-x)

(defgroup my/github nil
  "GitHub CLI integration."
  :group 'tools)

(config-defvar my/github-cache-ttl nil
  "Seconds before cached GitHub topic data is refreshed."
  :type 'integer
  :group 'my/github)

(defconst my/github-cache-directory
  (expand-file-name "var/github/" user-emacs-directory)
  "Directory containing GitHub CLI response caches.")

(defvar-local my/github-patch-root nil)
(defvar-local my/github-patch-context nil)
(defvar-local my/github-patch-number nil)

(defun my/github--repo-info ()
  "Return (ROOT CONTEXT GH OWNER-REPO) for the current GitHub repository."
  (let* ((root (or (magit-toplevel)
                   (user-error "This is not a Git repository")))
         (context (remote-context root))
         (gh (or (remote-executable-find "gh" context)
                 (user-error "gh is unavailable on this target")))
         (result (remote-exec gh
                              :args '("repo" "view" "--json" "nameWithOwner"
                                      "--jq" ".nameWithOwner")
                              :context context :trim t
                              :filesystem-effects 'none)))
    (unless (zerop (remote-exec-result-status result))
      (user-error "gh cannot resolve this repository: %s"
                  (remote-exec-result-stderr result)))
    (let ((owner-repo (remote-exec-result-stdout result)))
      (unless (string-match-p "\\`[^/]+/[^/]+\\'" owner-repo)
        (user-error "Unexpected GitHub repository name: %s" owner-repo))
      (list root context gh owner-repo))))

(defun my/github--cache-file (context root owner-repo)
  "Return cache file for CONTEXT, ROOT, and OWNER-REPO."
  (make-directory my/github-cache-directory t)
  (expand-file-name
   (concat (secure-hash
            'sha256
            (format "%s\0%s\0%s"
                    (remote-context-target-id context) root owner-repo))
           ".eld")
   my/github-cache-directory))

(defun my/github--cache-read (file)
  "Read a fresh topic cache from FILE, or return nil."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((value (read (current-buffer))))
            (when (and (listp value)
                       (numberp (plist-get value :time))
                       (< (- (float-time) (plist-get value :time))
                          my/github-cache-ttl))
              (plist-get value :items))))
      (error nil))))

(defun my/github--cache-write (file items)
  "Persist ITEMS in topic cache FILE."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 (list :time (float-time) :items items) (current-buffer)))))

(defun my/github--parse-topics (json)
  "Parse topic JSON returned by `gh search issues'."
  (json-parse-string json
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun my/github--with-topics (callback &optional refresh)
  "Call CALLBACK with repository info and GitHub topics.
When REFRESH is non-nil, ignore the persistent cache."
  (pcase-let* ((info (my/github--repo-info))
               (`(,root ,context ,gh ,owner-repo) info)
               (cache-file (my/github--cache-file context root owner-repo))
               (cached (and (not refresh) (my/github--cache-read cache-file))))
    (if cached
        (funcall callback info cached)
      (message "Fetching GitHub issues and pull requests for %s…" owner-repo)
      (remote-exec-async
       gh
       :args (list "search" "issues" "--repo" owner-repo "--state" "open"
                   "--limit" "100" "--json"
                   "number,title,author,isPullRequest,updatedAt,url")
       :context context
       :filesystem-effects 'none
       :name "github-topics"
       :callback
       (lambda (result)
         (if (zerop (remote-exec-result-status result))
             (condition-case err
                 (let ((items (my/github--parse-topics
                               (remote-exec-result-stdout result))))
                   (my/github--cache-write cache-file items)
                   (funcall callback info items))
               (error
                (message "GitHub response parse failed: %s"
                         (error-message-string err))))
           (message "GitHub query failed: %s"
                    (string-trim (remote-exec-result-stderr result)))))))))

(defun my/github--topic-author (item)
  "Return ITEM's author login."
  (alist-get 'login (alist-get 'author item)))

(defun my/github--topic-candidate (item)
  "Return a display candidate for GitHub topic ITEM."
  (format "%-4s #%s  %s  · %s"
          (if (alist-get 'isPullRequest item) "PR" "Issue")
          (alist-get 'number item)
          (alist-get 'title item)
          (or (my/github--topic-author item) "unknown")))

(defun my/github--read-topic (items &optional prompt)
  "Read and return one topic from ITEMS."
  (unless items
    (user-error "No GitHub topics found"))
  (let* ((candidates (mapcar (lambda (item)
                               (cons (my/github--topic-candidate item) item))
                             items))
         (choice (completing-read (or prompt "GitHub topic: ")
                                  candidates nil t)))
    (cdr (assoc choice candidates))))

(defun my/github-topic-open (&optional refresh)
  "Choose an issue or pull request and open it.
With REFRESH, bypass the topic cache."
  (interactive "P")
  (my/github--with-topics
   (lambda (_info items)
     (browse-url (alist-get 'url (my/github--read-topic items))))
   refresh))

(defun my/github-topic-insert (&optional refresh)
  "Choose an issue or pull request and insert its #NUMBER reference."
  (interactive "P")
  (let ((origin (current-buffer))
        (marker (copy-marker (point) t)))
    (my/github--with-topics
     (lambda (_info items)
       (unwind-protect
           (when (and (buffer-live-p origin) (marker-buffer marker))
             (let ((item (my/github--read-topic items)))
               (with-current-buffer origin
                 (goto-char marker)
                 (insert (format "#%s" (alist-get 'number item))))))
         (set-marker marker nil)))
     refresh)))

(defun my/github-topic-open-by-author (&optional refresh)
  "Choose an author and open one of their issues or pull requests."
  (interactive "P")
  (my/github--with-topics
   (lambda (_info items)
     (let* ((authors (sort (delete-dups (delq nil (mapcar #'my/github--topic-author items)))
                           #'string-lessp))
            (author (completing-read "GitHub author: " authors nil t))
            (filtered (seq-filter
                       (lambda (item)
                         (equal (my/github--topic-author item) author))
                       items)))
       (browse-url
        (alist-get 'url
                   (my/github--read-topic filtered
                                          (format "Topic by %s: " author))))))
   refresh))

(defun my/github-contributor-insert ()
  "Choose a mentionable GitHub user and insert @LOGIN."
  (interactive)
  (pcase-let* ((info (my/github--repo-info))
               (`(,_root ,context ,gh ,owner-repo) info)
               (`(,owner ,repo) (split-string owner-repo "/" t))
               (origin (current-buffer))
               (marker (copy-marker (point) t))
               (query "query($owner:String!,$repo:String!){repository(owner:$owner,name:$repo){mentionableUsers(first:100){nodes{login,name}}}}"))
    (message "Fetching mentionable GitHub users…")
    (remote-exec-async
     gh
     :args (list "api" "graphql" "-f" (concat "query=" query)
                 "-F" (concat "owner=" owner) "-F" (concat "repo=" repo))
     :context context :filesystem-effects 'none :name "github-contributors"
     :callback
     (lambda (result)
       (unwind-protect
           (if (zerop (remote-exec-result-status result))
               (let* ((payload (json-parse-string
                                (remote-exec-result-stdout result)
                                :object-type 'alist :array-type 'list
                                :null-object nil :false-object nil))
                      (users (alist-get
                              'nodes
                              (alist-get
                               'mentionableUsers
                               (alist-get 'repository
                                          (alist-get 'data payload)))))
                      (candidates
                       (mapcar
                        (lambda (user)
                          (cons (format "%-24s %s"
                                        (alist-get 'login user)
                                        (or (alist-get 'name user) ""))
                                user))
                        users))
                      (choice (completing-read "Contributor: " candidates nil t))
                      (login (alist-get 'login (cdr (assoc choice candidates)))))
                 (when (and login (buffer-live-p origin) (marker-buffer marker))
                   (with-current-buffer origin
                     (goto-char marker)
                     (insert "@" login))))
             (message "GitHub contributor query failed: %s"
                      (string-trim (remote-exec-result-stderr result))))
         (set-marker marker nil))))))

(defun my/github-refresh ()
  "Refresh GitHub issue and pull request data for the current repository."
  (interactive)
  (my/github--with-topics
   (lambda (_info items)
     (message "Cached %d GitHub topics" (length items)))
   t))

(defun my/github-pr-patch-preview ()
  "Choose a pull request and preview its patch in a Diff buffer."
  (interactive)
  (my/github--with-topics
   (lambda (info items)
     (let* ((prs (seq-filter (lambda (item) (alist-get 'isPullRequest item)) items))
            (item (my/github--read-topic prs "Pull request: "))
            (number (alist-get 'number item)))
       (pcase-let ((`(,root ,context ,gh ,_owner-repo) info))
         (remote-exec-async
          gh :args (list "pr" "diff" (number-to-string number))
          :context context :filesystem-effects 'none :name "github-pr-diff"
          :callback
          (lambda (result)
            (if (zerop (remote-exec-result-status result))
                (let ((buffer (get-buffer-create
                               (format "*GitHub PR #%s patch*" number))))
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert (remote-exec-result-stdout result))
                      (diff-mode)
                      (setq-local my/github-patch-root root
                                  my/github-patch-context context
                                  my/github-patch-number number)
                      (local-set-key (kbd "a") #'my/github-apply-previewed-patch)
                      (goto-char (point-min))))
                  (pop-to-buffer buffer)
                  (message "Press a to check and apply PR #%s" number))
              (message "Fetching PR patch failed: %s"
                       (string-trim (remote-exec-result-stderr result)))))))))
   nil))

(defun my/github--patch-target-file (context number)
  "Return a target-side temporary patch path for CONTEXT and NUMBER."
  (remote-expand-file-name
   (format "/tmp/emacs-github-pr-%s-%s.patch" number (emacs-pid))
   nil context))

(defun my/github-apply-previewed-patch ()
  "Check and apply the pull request patch in the current preview buffer."
  (interactive)
  (unless (and my/github-patch-root my/github-patch-context my/github-patch-number)
    (user-error "This is not a GitHub patch preview"))
  (let* ((context my/github-patch-context)
         (number my/github-patch-number)
         (git (or (remote-executable-find "git" context)
                  (user-error "git is unavailable on this target")))
         (status-result (remote-exec git :args '("status" "--porcelain")
                                     :context context :trim t
                                     :filesystem-effects 'none)))
    (unless (and (zerop (remote-exec-result-status status-result))
                 (string-empty-p (remote-exec-result-stdout status-result)))
      (user-error "The Git working tree must be clean"))
    (let ((local-patch (make-temp-file "emacs-github-pr-" nil ".patch"))
          (target-patch (my/github--patch-target-file context number)))
      (unwind-protect
          (progn
            (write-region (point-min) (point-max) local-patch nil 'silent)
            (remote-copy-file-to-target local-patch target-patch
                                        :context context :overwrite t)
            (let ((check (remote-exec
                          git :args (list "apply" "--check"
                                          (remote-file-local-name target-patch))
                          :context context :trim t
                          :filesystem-effects 'none)))
              (unless (zerop (remote-exec-result-status check))
                (user-error "Patch check failed: %s"
                            (remote-exec-result-stderr check))))
            (when (yes-or-no-p (format "Apply GitHub PR #%s with --3way? " number))
              (let ((apply-result
                     (remote-exec
                      git :args (list "apply" "--3way"
                                      (remote-file-local-name target-patch))
                      :context context :trim t
                      :filesystem-effects 'content)))
                (unless (zerop (remote-exec-result-status apply-result))
                  (user-error "Patch apply failed: %s"
                              (remote-exec-result-stderr apply-result)))
                (message "Applied GitHub PR #%s" number))))
        (when (file-exists-p local-patch)
          (delete-file local-patch))
        (when (file-exists-p target-patch)
          (delete-file target-patch))))))

(provide 'init-github)
;;; init-github.el ends here
