;;; remote-path.el --- Target-native PATH profiles and active probing -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Static profiles are hints, not truth.  `remote-path-probe' asks the target
;; through its selected route and caches the actual PATH, HOME, SHELL, system,
;; and architecture.  Environment capsules consume those facts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'remote-core)
(require 'remote-fs)
(require 'remote-process)
(require 'remote-environment)
(require 'remote-background)

(cl-defstruct (remote-path-profile
               (:constructor remote-path-profile-create))
  id priority systems architectures paths)

(cl-defstruct (remote-path-facts
               (:constructor remote-path-facts-create))
  target-id system architecture shell home path source probed-at)

(defvar remote-path-profiles nil
  "Registered static PATH hint profiles.")

(defvar remote-path-facts-cache (make-hash-table :test #'equal)
  "Actively probed host facts keyed by target ID.")

(defvar remote-background-defer-commit nil)

(defun remote-path-invalidate (&optional target-id)
  "Invalidate probed path facts, optionally only for TARGET-ID."
  (if target-id
      (remhash (remote-normalize-id target-id) remote-path-facts-cache)
    (clrhash remote-path-facts-cache)))

(defconst remote-path--probe-marker
  "__EMACS_REMOTE_FACTS_V1__\0"
  "Boundary marker separating shell startup output from probed facts.")

(cl-defun remote-register-path-profile
    (id &key (priority 0) systems architectures paths)
  "Register static PATH hint profile ID."
  (let* ((id (remote-normalize-id id))
         (profile
          (remote-path-profile-create
           :id id
           :priority priority
           :systems (mapcar #'downcase
                            (remote-path--string-list systems))
           :architectures (mapcar #'downcase
                                  (remote-path--string-list architectures))
           :paths (copy-sequence paths))))
    (setq remote-path-profiles
          (cons profile
                (cl-remove id remote-path-profiles
                           :key #'remote-path-profile-id
                           :test #'equal)))
    profile))

(defun remote-path--string-list (value)
  "Return VALUE as a list of strings."
  (cond
   ((null value) nil)
   ((stringp value) (list value))
   ((listp value)
    (delq nil
          (mapcar (lambda (item)
                    (and item (format "%s" item)))
                  value)))))

(defun remote-path--normalize-fact (value)
  "Normalize a probed fact VALUE."
  (let ((value (and value (string-trim value))))
    (and value (not (string-empty-p value)) (downcase value))))

(defun remote-path--parse-probe-output (output)
  "Extract the five NUL-delimited target facts from probe OUTPUT.
Interactive remote shells can emit banners or system notices before the
command's stdout.  Only data after `remote-path--probe-marker' belongs to the
probe protocol."
  (when-let* ((start
               (string-match
                (regexp-quote remote-path--probe-marker) output)))
    (let ((fields
           (split-string
            (substring
             output (+ start (length remote-path--probe-marker)))
            "\0")))
      (when (>= (length fields) 5)
        (seq-take fields 5)))))

(defun remote-path--probe-sync (context)
  "Probe target facts synchronously for CONTEXT."
  (let* ((remote-environment-inhibit t)
         (result
          (remote-exec
           "sh"
           :args
           (list
            "-lc"
            (concat
             "printf '__EMACS_REMOTE_FACTS_V1__\\0"
             "%s\\0%s\\0%s\\0%s\\0%s\\0' "
             "\"$(uname -s 2>/dev/null || printf unknown)\" "
             "\"$(uname -m 2>/dev/null || printf unknown)\" "
             "\"${SHELL-}\" \"${HOME-}\" \"${PATH-}\""))
           :context context
           :adapter "environment"
           :filesystem-effects 'none
           :check t))
         (fields
          (or (remote-path--parse-probe-output
               (remote-exec-result-stdout result))
              (error "Remote PATH probe returned no framed facts for %s"
                     (remote-context-target-id context))))
         (target-id (remote-context-target-id context))
         (facts
          (remote-path-facts-create
           :target-id target-id
           :system (remote-path--normalize-fact (nth 0 fields))
           :architecture (remote-path--normalize-fact (nth 1 fields))
           :shell (nth 2 fields)
           :home (nth 3 fields)
           :path (split-string (or (nth 4 fields) "")
                               path-separator t)
           :source (remote-route-link-plugin-id
                    (remote-exec-result-route result))
           :probed-at (current-time))))
    (unless remote-background-defer-commit
      (remote-path--commit-facts facts))
    facts))

(defun remote-path--commit-facts (facts)
  "Commit generation-validated host FACTS to target and cache state."
  (let ((target-id (remote-path-facts-target-id facts)))
    (when-let* ((target (remote-get-target target-id)))
      (unless (remote-target-system target)
        (setf (remote-target-system target)
              (remote-path-facts-system facts)))
      (unless (remote-target-architecture target)
        (setf (remote-target-architecture target)
              (remote-path-facts-architecture facts)))
      (unless (remote-target-shell target)
        (setf (remote-target-shell target)
              (remote-path-facts-shell facts))))
    (puthash target-id facts remote-path-facts-cache)
    facts))

(defun remote-path-probe (&optional context force callback)
  "Actively probe PATH and host facts for CONTEXT.
Reuse cached facts unless FORCE is non-nil.  With CALLBACK, schedule the probe
and invoke CALLBACK with the facts without blocking the caller."
  (let* ((context
          (cond
           ((remote-context-p context) context)
           ((stringp context) (remote-context context))
           (t (remote-context))))
         (target-id (remote-context-target-id context))
         (cached (and (not force)
                      (gethash target-id remote-path-facts-cache)))
         (buffer (current-buffer)))
    (if callback
        (progn
          (remote-background-submit
           (list 'path-probe target-id)
           (lambda ()
             (or cached (remote-path--probe-sync context)))
           :target-id target-id
           :owner-buffer buffer
           :non-essential nil
           :callback
           (lambda (facts)
             (remote-path--commit-facts facts)
             (funcall callback facts))
           :error-callback
           (lambda (error)
             (remote-log
              'path-probe-error :target target-id
              :error (error-message-string error))))
          nil)
      (when force
        (remote-path-invalidate target-id)
        (remote-environment-invalidate target-id))
      (or cached (remote-path--probe-sync context)))))

(defalias 'remote-environment-probe #'remote-path-probe)

(defun remote-path--profile-matches-p (profile system architecture)
  "Return non-nil when PROFILE matches SYSTEM and ARCHITECTURE."
  (and
   (or (null (remote-path-profile-systems profile))
       (member system (remote-path-profile-systems profile)))
   (or (null (remote-path-profile-architectures profile))
       (member architecture
               (remote-path-profile-architectures profile)))))

(defun remote-path--expand-hint (path home)
  "Expand static PATH hint PATH using target HOME."
  (let ((path
         (if (and home (string-prefix-p "$HOME/" path))
             (expand-file-name
              (substring path (length "$HOME/"))
              (file-name-as-directory home))
           path)))
    (and (file-name-absolute-p path) path)))

(defun remote-path-candidates (&optional context probe)
  "Return ordered target-native PATH candidates for CONTEXT.
When PROBE is non-nil, actively refresh target facts first."
  (let* ((context
          (cond
           ((remote-context-p context) context)
           ((stringp context) (remote-context context))
           (t (remote-context))))
         (target (remote-get-target
                  (remote-context-target-id context)))
         (facts
          (if probe
              (remote-path-probe context t)
            (gethash (remote-context-target-id context)
                     remote-path-facts-cache)))
         (system
          (or (and facts (remote-path-facts-system facts))
              (and target
                   (remote-path--normalize-fact
                    (remote-target-system target)))))
         (architecture
          (or (and facts (remote-path-facts-architecture facts))
              (and target
                   (remote-path--normalize-fact
                    (remote-target-architecture target)))))
         (home (and facts (remote-path-facts-home facts)))
         (selected-profiles
          (remote-path--string-list
           (and target
                (remote--environment-config-value
                 (remote--target-environment-config target)
                 'pathProfiles))))
         (profiles
          (sort
           (seq-filter
            (lambda (profile)
              (if selected-profiles
                  (member (remote-path-profile-id profile)
                          selected-profiles)
                (remote-path--profile-matches-p
                 profile system architecture)))
            (copy-sequence remote-path-profiles))
           (lambda (left right)
             (> (remote-path-profile-priority left)
                (remote-path-profile-priority right)))))
         (paths (and facts (copy-sequence
                            (remote-path-facts-path facts)))))
    (dolist (profile profiles)
      (dolist (path (remote-path-profile-paths profile))
        (when-let* ((expanded
                     (remote-path--expand-hint path home)))
          (setq paths (append paths (list expanded))))))
    (delete-dups (delq nil paths))))

(remote-register-adapter
 "environment"
 :capabilities '(process-sync environment)
 :preferences '((default . ("tramp-rpc" "tramp" "native"))))

(remote-register-path-profile
 "posix"
 :priority 0
 :paths '("$HOME/.local/bin" "$HOME/bin"
          "/usr/local/bin" "/usr/bin" "/bin"))

(remote-register-path-profile
 "linux"
 :priority 20
 :systems '("linux")
 :paths '("/usr/local/sbin" "/usr/sbin" "/sbin"
          "/snap/bin"))

(remote-register-path-profile
 "nix"
 :priority 30
 :systems '("linux" "darwin")
 :paths '("$HOME/.nix-profile/bin"
          "/run/current-system/sw/bin"
          "/nix/var/nix/profiles/default/bin"))

(remote-register-path-profile
 "darwin"
 :priority 40
 :systems '("darwin")
 :paths '("/opt/homebrew/bin" "/opt/homebrew/sbin"
          "/usr/local/bin" "/usr/local/sbin"
          "/opt/local/bin"))

(remote-register-environment-provider
 "host-path"
 :priority 0
 :scope 'host
 :predicate (lambda (_context) t)
 :fingerprint
 (lambda (context)
   (let ((target
          (remote-get-target
           (remote-context-target-id context))))
     (list (remote-context-target-id context)
           (and target (remote-target-system target))
           (and target (remote-target-architecture target)))))
 :load
 (lambda (context)
   (let* ((facts (remote-path-probe context))
          (path (remote-path-facts-path facts)))
     (list
      :vars
      (delq nil
            (list
             (and path
                  (cons "PATH"
                        (mapconcat #'identity path path-separator)))
             (and (remote-path-facts-home facts)
                  (cons "HOME" (remote-path-facts-home facts)))
             (and (remote-path-facts-shell facts)
                  (cons "SHELL" (remote-path-facts-shell facts)))))
      :source
      (list 'host-path
            (remote-path-facts-source facts))))))

(provide 'remote-path)
;;; remote-path.el ends here
