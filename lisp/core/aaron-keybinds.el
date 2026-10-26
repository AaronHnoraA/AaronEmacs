;;; aaron-keybinds.el --- Keybinding layer over general.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The configuration's front door for key bindings, on top of the core's
;; `general.el':
;;
;;   `map!'                     one form for leader, localleader, keymap,
;;                              major-mode and Evil-state bindings
;;   `define-leader-key!'       SPC bindings
;;   `define-localleader-key!'  SPC m bindings
;;   `define-key!' / `undefine-key!'  `general-def' / `general-unbind'
;;
;; Every binding still goes through `general-define-key', so the module
;; ledger records it under the module that made it and `module-board'
;; reports two modules binding one key.
;;
;; Adapted from Doom Emacs (MIT, Henrik Lissner):
;;   core/modules/doom/compat/+keybinds.el @ 59cdaa3
;; Changed: the leader is Evil's own `<leader>' / `<localleader>' in
;; normal state (set in editor/evil), not Doom's leader map in
;; `general-override-mode-map'; `:desc' becomes general's `:which-key', so
;; labels reach which-key as before; `map!' also binds in batch sessions,
;; and Evil-state bindings are always emitted (general delays them until
;; Evil loads) rather than dropped when Evil was absent at expansion time.
;; Dropped: `:prefix-map', the leader-key options and
;; `general-auto-unbind-keys'.

;;; Code:

(require 'cl-lib)
(require 'general)
(require 'aaron-lib)

(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)

(general-create-definer define-leader-key!
  :states 'normal
  :keymaps 'global
  :prefix "<leader>")

(general-create-definer define-localleader-key!
  :states 'normal
  :prefix "<localleader>")


;;; `map!'

(defvar aaron-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "Letters of `map!' state keywords and the Evil states they name.")

(defun aaron--map-keyword-to-states (keyword)
  "Return the Evil states named by KEYWORD, as :nvi names three."
  (cl-loop for l across (substring (symbol-name keyword) 1)
           if (assq l aaron-evil-state-alist) collect (cdr it)
           else do (error "Not a valid state: %s" l)))

(defvar aaron--map-forms nil)
(defvar aaron--map-fn nil)
(defvar aaron--map-batch-forms nil)
(defvar aaron--map-state '(:dummy t))
(defvar aaron--map-parent-state nil)
(defvar aaron--map-last-prefix nil)

(defun aaron--map-process (rest)
  "Return the forms that bind REST, the arguments of a `map!' block."
  (let ((aaron--map-fn aaron--map-fn)
        aaron--map-state
        aaron--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (aaron--map-nested nil key))
              ((keywordp key)
               (pcase key
                 (:leader
                  (aaron--map-commit)
                  (setq aaron--map-fn 'define-leader-key!))
                 (:localleader
                  (aaron--map-commit)
                  (setq aaron--map-fn 'define-localleader-key!))
                 (:after
                  (aaron--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (aaron--map-set :keymaps `(backquote ,(ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (aaron--map-nested (list (intern (substring (symbol-name key) 1))
                                           (pop rest))
                                     rest)
                  (setq rest nil))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (aaron--map-set (if aaron--map-fn :infix :prefix) prefix)
                    (when (stringp desc)
                      (setq aaron--map-last-prefix (cons nil desc)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          aaron--map-forms)))
                 (_
                  (condition-case _
                      (aaron--map-def (pop rest) (pop rest)
                                      (aaron--map-keyword-to-states key)
                                      desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))
              ((aaron--map-def key (pop rest) nil desc)
               (setq desc nil)))))
    (aaron--map-commit)
    (macroexp-progn (nreverse (delq nil aaron--map-forms)))))

(defun aaron--map-append-keys (prop)
  "Return the parent's and this block's PROP keys joined."
  (let ((a (plist-get aaron--map-parent-state prop))
        (b (plist-get aaron--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))

(defun aaron--map-nested (wrapper rest)
  "Process REST as a nested block, inside WRAPPER when non-nil."
  (aaron--map-commit)
  (let ((aaron--map-parent-state (aaron--map-state)))
    (push (if wrapper
              (append wrapper (list (aaron--map-process rest)))
            (aaron--map-process rest))
          aaron--map-forms)))

(defun aaron--map-set (prop &optional value)
  "Set PROP to VALUE for the following keys of this block."
  (unless (equal (plist-get aaron--map-state prop) value)
    (aaron--map-commit))
  (setq aaron--map-state (plist-put aaron--map-state prop value)))

(defun aaron--map-label (def desc)
  "Return DEF labelled DESC for which-key, as a general extended definition."
  (let ((unquoted (aaron-unquote def)))
    (if (and (listp unquoted) (keywordp (car-safe unquoted)))
        `(quote ,(plist-put (copy-sequence unquoted) :which-key desc))
      `(list :def ,def :which-key ,desc))))

(defun aaron--map-def (key def &optional states desc)
  "Queue KEY bound to DEF in STATES, labelled DESC."
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (setq def (if (and (equal key "") (null def))
                  (progn (setq aaron--map-last-prefix nil)
                         `(list :ignore t :which-key ,desc))
                (aaron--map-label def desc))))
  (dolist (state states)
    (when (and aaron--map-last-prefix
               (not (memq state (car aaron--map-last-prefix))))
      (push state (car aaron--map-last-prefix))
      (push (list "" `(list :ignore t :which-key ,(cdr aaron--map-last-prefix)))
            (alist-get state aaron--map-batch-forms)))
    (push (list key def)
          (alist-get state aaron--map-batch-forms)))
  t)

(defun aaron--map-commit ()
  "Turn the queued keys into definer forms."
  (when aaron--map-batch-forms
    (cl-loop with attrs = (aaron--map-state)
             for (state . defs) in aaron--map-batch-forms
             collect `(,(or aaron--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) aaron--map-forms))
    (setq aaron--map-last-prefix nil)
    (setq aaron--map-batch-forms nil)))

(defun aaron--map-state ()
  "Return this block's definer properties, inherited ones included."
  (let ((plist
         (append (list :prefix (aaron--map-append-keys :prefix)
                       :infix  (aaron--map-append-keys :infix)
                       :keymaps
                       (append (plist-get aaron--map-parent-state :keymaps)
                               (plist-get aaron--map-state :keymaps)))
                 aaron--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

(defmacro map! (&rest rest)
  "Bind keys, through `general-define-key'.

Properties
  :leader [...]                   bind under SPC (`define-leader-key!')
  :localleader [...]              bind under SPC m; needs a :map
  :mode [MODE(s)] [...]           bind in MODE's keymap, MODE-map
  :map [KEYMAP(s)] [...]          bind in KEYMAP(S)
  :prefix [PREFIX] [...]          prefix the following keys.  PREFIX may be
                                  (PREFIX . DESCRIPTION), labelling it
  :after [FEATURE] [...]          bind once FEATURE loads (`after!')
  :textobj KEY INNER-FN OUTER-FN  an Evil text object pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]
  :desc DESCRIPTION               the which-key label of the next key

  Properties nest: a list applies them to its own keys only.

States
  :n normal  :v visual  :i insert  :e emacs  :o operator  :m motion
  :r replace  :g global

  Combine letters (:nv) and put them right before the key.  They apply to
  the next key and definition only.  Without one the key is bound outside
  Evil, in the keymap itself.  Under :leader and :localleader the default
  state is normal.

  Do    (map! :leader :desc \"Description\" \"x\" #\\='command)
  Don't (map! :n :leader \"x\" #\\='command)"
  (aaron--map-process rest))

(provide 'aaron-keybinds)
;;; aaron-keybinds.el ends here
