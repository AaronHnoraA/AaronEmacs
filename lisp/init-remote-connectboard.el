;;; init-remote-connectboard.el --- Curated remote target picker -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A small "connectboard" for remote targets.  It avoids TRAMP's default host
;; completion path and instead reads a curated list from:
;;
;;   - ~/.ssh/config
;;   - ~/.config/ssh/config
;;   - ~/.emacs.d/etc/remote.json
;;
;; The JSON file can both add explicit entries and define import ranges such as
;; ssh/rpc groups backed by SSH config host aliases.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tramp)

(declare-function my/evil-global-leader-set "init-funcs" (key command &optional replacement))
(declare-function my/vterm--open-in-directory "init-shell" (buffer-name directory))

(defgroup my/remote-connectboard nil
  "Curated picker for remote TRAMP targets."
  :group 'tools)

(defcustom my/remote-connectboard-config-file
  (expand-file-name "etc/remote.json" user-emacs-directory)
  "JSON file that defines curated remote targets."
  :type 'file)

(defcustom my/remote-connectboard-ssh-config-files
  (list (expand-file-name "~/.ssh/config")
        (expand-file-name "~/.config/ssh/config"))
  "SSH config files imported by the remote connectboard."
  :type '(repeat file))

(defcustom my/remote-connectboard-default-method "ssh"
  "Default TRAMP method used for JSON entries without an explicit method."
  :type 'string)

(defcustom my/remote-connectboard-default-path "~/"
  "Default remote path used for JSON entries without an explicit path."
  :type 'string)

(defcustom my/remote-connectboard-default-action 'open
  "Default action used by `my/remote-connectboard-dispatch'."
  :type '(choice (const :tag "Open" open)
                 (const :tag "VTerm" vterm)
                 (const :tag "Copy TRAMP path" copy)))

(defvar my/remote-connectboard-history nil)
(defvar my/remote-connectboard-action-history nil)
(defvar my/remote-connectboard--cache nil)
(defvar my/remote-connectboard--cache-signature nil)

(defconst my/remote-connectboard--template
  "{\n  \"ssh\": {\n    \"enabled\": true,\n    \"include\": [],\n    \"exclude\": []\n  },\n  \"rpc\": {\n    \"enabled\": false,\n    \"include\": [],\n    \"exclude\": []\n  },\n  \"entries\": []\n}\n"
  "Template used when bootstrapping `my/remote-connectboard-config-file'.")

(defun my/remote-connectboard--string-list (value)
  "Return VALUE as a list of strings."
  (cond
   ((null value) nil)
   ((stringp value) (list value))
   ((listp value)
    (delq nil
          (mapcar (lambda (item)
                    (and (stringp item) item))
                  value)))
   (t nil)))

(defun my/remote-connectboard--json-read ()
  "Read `my/remote-connectboard-config-file' and return its object."
  (when (file-readable-p my/remote-connectboard-config-file)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false nil))
      (json-read-file my/remote-connectboard-config-file))))

(defun my/remote-connectboard--expand-config-path (path)
  "Expand PATH relative to the JSON config directory when needed."
  (expand-file-name
   path
   (file-name-directory my/remote-connectboard-config-file)))

(defun my/remote-connectboard--import-config (root method)
  "Return import config for METHOD from JSON ROOT."
  (let ((config (alist-get method root)))
    (if (listp config) config nil)))

(defun my/remote-connectboard--ssh-config-files-resolved (root)
  "Return the SSH config files to import for JSON ROOT."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (file)
            (when (and (stringp file)
                       (not (string-empty-p file)))
              (my/remote-connectboard--expand-config-path file)))
          (append (my/remote-connectboard--string-list
                   (alist-get 'files
                              (my/remote-connectboard--import-config root 'ssh)))
                  my/remote-connectboard-ssh-config-files)))))

(defun my/remote-connectboard--source-signature (root)
  "Return a cache signature for JSON ROOT and all source files."
  (mapcar
   (lambda (file)
     (list file
           (when (file-exists-p file)
             (file-attribute-modification-time
              (file-attributes file)))))
   (delete-dups
    (append (my/remote-connectboard--ssh-config-files-resolved root)
            (list my/remote-connectboard-config-file)))))

(defun my/remote-connectboard--strip-ssh-comments (line)
  "Strip trailing SSH comments from LINE."
  (string-trim
   (replace-regexp-in-string "[[:space:]]+#.*\\'" "" line)))

(defun my/remote-connectboard--parse-ssh-config-file (file &optional seen)
  "Return concrete `Host' entries found in SSH config FILE.
SEEN tracks visited files for recursive Include handling."
  (let* ((file (expand-file-name file))
         (seen (or seen (make-hash-table :test #'equal)))
         hosts)
    (unless (or (gethash file seen)
                (not (file-readable-p file)))
      (puthash file t seen)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (my/remote-connectboard--strip-ssh-comments
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))))
            (cond
             ((string-match-p "\\`[[:space:]]*\\'" line)
              nil)
             ((string-match "\\`[[:space:]]*Include[[:space:]]+\\(.+\\)\\'" line)
              (dolist (pattern (split-string (match-string 1 line) "[[:space:]]+" t))
                (dolist (included (file-expand-wildcards
                                   (expand-file-name pattern
                                                     (file-name-directory file))
                                   t))
                  (setq hosts
                        (nconc hosts
                               (my/remote-connectboard--parse-ssh-config-file
                                included
                                seen))))))
             ((string-match "\\`[[:space:]]*Host[[:space:]]+\\(.+\\)\\'" line)
              (dolist (host (split-string (match-string 1 line) "[[:space:]]+" t))
                (unless (or (string-prefix-p "!" host)
                            (string-match-p "[*?]" host))
                  (push host hosts))))))
          (forward-line 1))))
    (delete-dups (nreverse hosts))))

(defun my/remote-connectboard--pattern-match-p (value pattern)
  "Return non-nil when VALUE matches PATTERN."
  (if (string-match-p "[[*?]" pattern)
      (string-match-p (wildcard-to-regexp pattern) value)
    (string= value pattern)))

(defun my/remote-connectboard--host-allowed-p (host import-config)
  "Return non-nil if HOST passes the IMPORT-CONFIG include/exclude filters."
  (let ((include (my/remote-connectboard--string-list
                  (alist-get 'include import-config)))
        (exclude (my/remote-connectboard--string-list
                  (alist-get 'exclude import-config))))
    (and (or (null include)
             (cl-some (lambda (pattern)
                        (my/remote-connectboard--pattern-match-p host pattern))
                      include))
         (not (cl-some (lambda (pattern)
                         (my/remote-connectboard--pattern-match-p host pattern))
                       exclude)))))

(defun my/remote-connectboard--normalize-remote-path (path)
  "Normalize remote PATH for TRAMP."
  (let ((path (or path my/remote-connectboard-default-path)))
    (cond
     ((string-match-p "\\`[~/]" path) path)
     (t (concat "~/" path)))))

(defun my/remote-connectboard--entry (plist)
  "Return PLIST when it has enough information to describe a target."
  (when (or (plist-get plist :tramp)
            (plist-get plist :host))
    plist))

(defun my/remote-connectboard--make-import-entry (host method import-config)
  "Build an imported METHOD entry for HOST using IMPORT-CONFIG."
  (my/remote-connectboard--entry
   (list :name host
         :host host
         :method (symbol-name method)
         :path (my/remote-connectboard--normalize-remote-path
                (alist-get 'path import-config))
         :group (or (alist-get 'group import-config)
                    (symbol-name method))
         :description (alist-get 'description import-config)
         :source (format "%s-config" (symbol-name method)))))

(defun my/remote-connectboard--json-entry-name (entry)
  "Return the display name for JSON ENTRY."
  (or (alist-get 'name entry)
      (alist-get 'label entry)
      (alist-get 'title entry)
      (alist-get 'host entry)))

(defun my/remote-connectboard--make-json-entry (entry)
  "Convert JSON ENTRY into an internal plist."
  (let* ((enabled (alist-get 'enabled entry t))
         (name (my/remote-connectboard--json-entry-name entry))
         (host (alist-get 'host entry))
         (tramp (alist-get 'tramp entry))
         (method (or (alist-get 'method entry)
                     my/remote-connectboard-default-method))
         (path (my/remote-connectboard--normalize-remote-path
                (alist-get 'path entry))))
    (when (and enabled
               (or tramp host)
               name)
      (my/remote-connectboard--entry
       (list :name name
             :host host
             :user (alist-get 'user entry)
             :port (alist-get 'port entry)
             :method method
             :path path
             :tramp tramp
             :terminal (alist-get 'terminal entry)
             :group (or (alist-get 'group entry)
                        (alist-get 'region entry))
             :description (alist-get 'description entry)
             :source (or (alist-get 'source entry) "remote.json"))))))

(defun my/remote-connectboard--json-entries (root)
  "Return explicit remote entries declared in JSON ROOT."
  (let ((entries (alist-get 'entries root)))
    (delq nil
          (mapcar #'my/remote-connectboard--make-json-entry
                  (if (listp entries) entries nil)))))

(defun my/remote-connectboard--import-enabled-p (import-config)
  "Return non-nil when IMPORT-CONFIG enables host import."
  (not (eq (alist-get 'enabled import-config t) nil)))

(defun my/remote-connectboard--import-entries-for-method (root method)
  "Return imported entries for METHOD from JSON ROOT."
  (let ((import-config (my/remote-connectboard--import-config root method)))
    (when (my/remote-connectboard--import-enabled-p import-config)
      (let (entries)
        (dolist (file (my/remote-connectboard--ssh-config-files-resolved root))
          (dolist (host (my/remote-connectboard--parse-ssh-config-file file))
            (when (my/remote-connectboard--host-allowed-p host import-config)
              (push (my/remote-connectboard--make-import-entry
                     host method import-config)
                    entries))))
        (nreverse (delete-dups entries))))))

(defun my/remote-connectboard--import-entries (root)
  "Return all imported entries for JSON ROOT."
  (append (my/remote-connectboard--import-entries-for-method root 'ssh)
          (my/remote-connectboard--import-entries-for-method root 'rpc)))

(defun my/remote-connectboard--entry-tramp-path (entry)
  "Return the TRAMP path for ENTRY."
  (or (plist-get entry :tramp)
      (let ((method (or (plist-get entry :method)
                        my/remote-connectboard-default-method))
            (user (plist-get entry :user))
            (host (plist-get entry :host))
            (port (plist-get entry :port))
            (path (my/remote-connectboard--normalize-remote-path
                   (plist-get entry :path))))
        (format "/%s:%s%s%s:%s"
                method
                (if (and user (not (string-empty-p user)))
                    (concat user "@")
                  "")
                host
                (if port
                    (format "#%s" port)
                  "")
                path))))

(defun my/remote-connectboard--entry-ssh-directory (entry)
  "Return an SSH TRAMP directory suitable for VTerm from ENTRY."
  (or (plist-get entry :terminal)
      (let* ((raw (my/remote-connectboard--entry-tramp-path entry))
             (vec (ignore-errors (tramp-dissect-file-name raw nil)))
             (user (or (plist-get entry :user)
                       (and vec (tramp-file-name-user vec))))
             (host (or (plist-get entry :host)
                       (and vec (tramp-file-name-host vec))))
             (port (or (plist-get entry :port)
                       (and vec (tramp-file-name-port vec))))
             (path (or (plist-get entry :path)
                       (and vec (tramp-file-name-localname vec))
                       my/remote-connectboard-default-path)))
        (when host
          (format "/ssh:%s%s%s:%s"
                  (if (and user (not (string-empty-p user)))
                      (concat user "@")
                    "")
                  host
                  (if port
                      (format "#%s" port)
                    "")
                  (my/remote-connectboard--normalize-remote-path path))))))

(defun my/remote-connectboard--entry-display (entry)
  "Return the minibuffer display string for ENTRY."
  (let ((name (or (plist-get entry :name) "remote"))
        (method (or (plist-get entry :method) "ssh"))
        (group (plist-get entry :group))
        (path (or (plist-get entry :path) my/remote-connectboard-default-path))
        (source (plist-get entry :source))
        (description (plist-get entry :description)))
    (string-join
     (delq nil
           (list (format "%-24s" name)
                 (format "[%s]" method)
                 (and group (format "{%s}" group))
                 path
                 (and source (format "<%s>" source))
                 (and description (format "- %s" description))))
     "  ")))

(defun my/remote-connectboard--build-entries (root)
  "Build the combined connectboard entries for JSON ROOT."
  (append (my/remote-connectboard--json-entries root)
          (my/remote-connectboard--import-entries root)))

(defun my/remote-connectboard-entries ()
  "Return cached remote connectboard entries."
  (let* ((root (or (my/remote-connectboard--json-read) '()))
         (signature (my/remote-connectboard--source-signature root)))
    (if (equal signature my/remote-connectboard--cache-signature)
        my/remote-connectboard--cache
      (setq my/remote-connectboard--cache-signature signature
            my/remote-connectboard--cache
            (my/remote-connectboard--build-entries root)))))

(defun my/remote-connectboard-refresh ()
  "Clear the cached connectboard entries."
  (interactive)
  (setq my/remote-connectboard--cache nil
        my/remote-connectboard--cache-signature nil)
  (message "Remote connectboard cache cleared"))

(defun my/remote-connectboard--read-entry ()
  "Prompt for and return one remote entry."
  (let ((entries (my/remote-connectboard-entries))
        (table (make-hash-table :test #'equal))
        candidates)
    (unless entries
      (user-error "No remote targets found in SSH config or %s"
                  my/remote-connectboard-config-file))
    (dolist (entry entries)
      (let* ((base (my/remote-connectboard--entry-display entry))
             (display base)
             (index 2))
        (while (gethash display table)
          (setq display (format "%s <%d>" base index)
                index (1+ index)))
        (puthash display entry table)
        (push display candidates)))
    (gethash
     (completing-read "Remote: "
                      (nreverse candidates)
                      nil
                      t
                      nil
                      'my/remote-connectboard-history)
     table)))

(defun my/remote-connectboard--read-action ()
  "Prompt for an action for `my/remote-connectboard-dispatch'."
  (let* ((default (symbol-name my/remote-connectboard-default-action))
         (choice (completing-read
                  (format "Action (default %s): " default)
                  '("open" "vterm" "copy" "config" "refresh")
                  nil
                  t
                  nil
                  'my/remote-connectboard-action-history
                  default)))
    (intern choice)))

(defun my/remote-connectboard-open (entry)
  "Open remote ENTRY with `find-file'."
  (interactive (list (my/remote-connectboard--read-entry)))
  (find-file (my/remote-connectboard--entry-tramp-path entry)))

(defun my/remote-connectboard (&optional choose-action)
  "Open one curated remote target.
With prefix argument CHOOSE-ACTION, prompt for the action first."
  (interactive "P")
  (if choose-action
      (let ((action (my/remote-connectboard--read-action)))
        (my/remote-connectboard-dispatch
         (unless (memq action '(config refresh))
           (my/remote-connectboard--read-entry))
         action))
    (my/remote-connectboard-open
     (my/remote-connectboard--read-entry))))

(defun my/remote-connectboard-vterm (entry)
  "Open a dedicated VTerm for remote ENTRY."
  (interactive (list (my/remote-connectboard--read-entry)))
  (let* ((directory (my/remote-connectboard--entry-ssh-directory entry))
         (name (or (plist-get entry :name)
                   (plist-get entry :host)
                   "remote")))
    (unless directory
      (user-error "Entry %s has no SSH-compatible target for VTerm" name))
    (pop-to-buffer
     (my/vterm--open-in-directory
      (format "*vterm:remote:%s*" name)
      directory))))

(defun my/remote-connectboard-copy (entry)
  "Copy ENTRY's TRAMP path to the kill ring."
  (interactive (list (my/remote-connectboard--read-entry)))
  (let ((path (my/remote-connectboard--entry-tramp-path entry)))
    (kill-new path)
    (message "Copied remote path: %s" path)))

(defun my/remote-connectboard-dispatch (entry action)
  "Run ACTION for remote ENTRY."
  (interactive
   (let ((action (my/remote-connectboard--read-action)))
     (list (unless (memq action '(config refresh))
             (my/remote-connectboard--read-entry))
           action)))
  (pcase action
    ('open (my/remote-connectboard-open entry))
    ('vterm (my/remote-connectboard-vterm entry))
    ('copy (my/remote-connectboard-copy entry))
    ('config (my/remote-connectboard-edit-config))
    ('refresh (my/remote-connectboard-refresh))
    (_ (user-error "Unknown connectboard action: %s" action))))

(defun my/remote-connectboard-edit-config ()
  "Open `my/remote-connectboard-config-file'."
  (interactive)
  (unless (file-exists-p my/remote-connectboard-config-file)
    (make-directory (file-name-directory my/remote-connectboard-config-file) t)
    (with-temp-file my/remote-connectboard-config-file
      (insert my/remote-connectboard--template)))
  (find-file my/remote-connectboard-config-file))

(my/evil-global-leader-set "o r" #'my/remote-connectboard "remote open")
(my/evil-global-leader-set "o R" #'my/remote-connectboard-dispatch "remote board")
(my/evil-global-leader-set "o C" #'my/remote-connectboard-edit-config "remote config")

(provide 'init-remote-connectboard)
;;; init-remote-connectboard.el ends here
