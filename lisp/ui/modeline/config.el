;;; ui/modeline/config.el --- Evil state block with theme-picked colours -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; From neo-emacs's modelinexp.  The mode line of the selected window starts
;; with the Evil state as a coloured block ending in a powerline arrow.  The
;; block colours come from the theme: on every theme load they are picked
;; from the foregrounds of `+modeline-color-faces' so that each state stands
;; apart from the mode-line background and from the other states (the
;; smallest of those colour distances is made as large as possible).
;; neo-emacs then overrode most of the choice with a fixed palette; here the
;; theme decides.  The rest of the mode line is doom-modeline's `main'.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'aaron-lib)

(defvar evil-state)
(defvar doom-modeline-height)
(declare-function doom-modeline--active "doom-modeline-core" ())
(declare-function doom-modeline "doom-modeline-core" (key))
(declare-function doom-modeline-set-modeline "doom-modeline-core" (key &optional default))
(declare-function powerline-arrow-left "powerline-separators" (face1 face2 &optional height))
(declare-function powerline-reset "powerline" ())

(defvar +modeline-color-faces
  '(font-lock-variable-name-face font-lock-string-face font-lock-type-face
    font-lock-warning-face font-lock-keyword-face font-lock-builtin-face
    font-lock-constant-face font-lock-preprocessor-face error success)
  "Theme faces whose foregrounds the state blocks are coloured from.
Earlier faces win ties.")

(defvar +modeline-states
  '((normal . "NORMAL") (insert . "INSERT") (visual . "VISUAL")
    (replace . "REPLACE") (motion . "MOTION") (operator . "OPERATOR")
    (emacs . "EMACS"))
  "Evil states shown in the mode line, in the order their colours are picked.
NORMAL is first, so it gets the first candidate that stands out best.")

(defvar +modeline-colors nil
  "The picked block colour of each state, as (STATE . COLOR).")


;;; Picking colours

(defun +modeline--rgb (color)
  "Return COLOR as normalized RGB without terminal palette quantization."
  (when (stringp color)
    (if (and (string-match "\\`#\\([[:xdigit:]]+\\)\\'" color)
             (memq (length (match-string 1 color)) '(3 6 9 12)))
        (let* ((hex (match-string 1 color))
               (digits (/ (length hex) 3))
               (maximum (float (1- (expt 16 digits)))))
          (cl-loop for offset from 0 below (length hex) by digits
                   collect (/ (string-to-number
                               (substring hex offset (+ offset digits)) 16)
                              maximum)))
      (color-name-to-rgb color))))

(defun +modeline--hex (face)
  "Return FACE's foreground as #RRGGBB, or nil."
  (let* ((color (and (facep face) (face-foreground face nil t)))
         (rgb (+modeline--rgb color)))
    (when rgb
      (apply #'color-rgb-to-hex (append rgb '(2))))))

(defun +modeline--distance (a b)
  "Return the perceptual distance between colours A and B (CIEDE2000)."
  (color-cie-de2000 (apply #'color-srgb-to-lab (+modeline--rgb a))
                    (apply #'color-srgb-to-lab (+modeline--rgb b))))

(defun +modeline-pick-colors (candidates base roles)
  "Give each of ROLES a distinct colour from CANDIDATES that stands out.
Pick greedily in ROLES order: each role takes the candidate whose smallest
distance to BASE and to the colours already taken is largest.  Return an
alist (ROLE . COLOR); roles beyond the candidates reuse the farthest one."
  (let (taken result)
    (dolist (role roles (nreverse result))
      (let* ((pool (or (cl-remove-if (lambda (color) (member color taken))
                                     candidates)
                       candidates))
             (best (car pool))
             (best-score -1.0))
        (dolist (color pool)
          (let ((score (cl-loop for other in (cons base taken)
                                minimize (+modeline--distance color other))))
            (when (> score best-score)
              (setq best color best-score score))))
        (push best taken)
        (push (cons role best) result)))))

(defun +modeline--text-color (background)
  "Return black or white, whichever reads better on BACKGROUND."
  (pcase-let ((`(,r ,g ,b) (+modeline--rgb background)))
    (if (> (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 0.5) "#000000" "#ffffff")))

(defun +modeline--face (state)
  "Return the face of STATE's block."
  (intern (format "+modeline-state-%s" state)))

(defun +modeline-update-colors-h (&rest _)
  "Pick the state block colours from the current theme and apply them."
  (let* ((base (or (let ((bg (face-background 'mode-line nil t)))
                     (and (+modeline--rgb bg) bg))
                   "#000000"))
         (candidates (delete-dups (delq nil (mapcar #'+modeline--hex +modeline-color-faces)))))
    (when candidates
      (setq +modeline-colors
            (+modeline-pick-colors candidates base (mapcar #'car +modeline-states)))
      (pcase-dolist (`(,state . ,color) +modeline-colors)
        (let ((face (+modeline--face state)))
          (unless (facep face)
            (make-face face))
          (set-face-attribute face nil :background color
                              :foreground (+modeline--text-color color)
                              :weight 'bold :inherit 'mode-line)))
      (when (featurep 'powerline)
        (powerline-reset))
      (force-mode-line-update t))))


;;; Segments

(defun +modeline--state ()
  "Return the relevant Evil state, or nil for native Portal input."
  (and (bound-and-true-p evil-local-mode)
       ;; Portal owns its input routing even when Evil still says `normal'.
       ;; Its [app]/[emacs] tag is the authoritative keyboard state.
       (not (bound-and-true-p portal-input-mode))
       (assq evil-state +modeline-states)
       evil-state))

(use-package powerline
  :ensure t
  :defer t)
(autoload 'powerline-arrow-left "powerline" nil nil)

(after! doom-modeline
  (doom-modeline-def-segment +modeline-state
    "The Evil state of the selected window, as a coloured block."
    (when-let* (((doom-modeline--active))
                (state (+modeline--state)))
      (propertize (format " %s " (cdr (assq state +modeline-states)))
                  'face (+modeline--face state))))

  (doom-modeline-def-segment +modeline-state-arrow
    "A powerline arrow from the state block into the mode line."
    (when-let* (((doom-modeline--active))
                (state (+modeline--state)))
      (if (and (display-graphic-p) (image-type-available-p 'xpm))
          (propertize " " 'display (powerline-arrow-left (+modeline--face state)
                                                         'mode-line
                                                         doom-modeline-height))
        " ")))

  ;; doom-modeline's `main', with the state block in front.
  (doom-modeline-def-modeline '+main
    '(+modeline-state +modeline-state-arrow eldoc bar window-state workspace-name
      window-number modals matches follow buffer-info remote-host
      buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info project-name persp-name battery grip
      irc mu4e gnus github debug repl lsp minor-modes input-method indent-info
      buffer-encoding major-mode process vcs check time))

  (add-hook! 'doom-modeline-mode-hook
    (defun +modeline-use-main-h ()
      "Use `+main' for Doom buffers, including ones already open.
Only replace Doom's unmodified `main' layout; special modelines keep their
own formats.  Do nothing when Doom modeline is being disabled."
      (when doom-modeline-mode
        (let ((old-main (list "%e" (doom-modeline 'main))))
          (doom-modeline-set-modeline '+main 'default)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (local-variable-p 'mode-line-format)
                         (equal mode-line-format old-main))
                (doom-modeline-set-modeline '+main))))))))
  (when (bound-and-true-p doom-modeline-mode)
    (+modeline-use-main-h))
  (+modeline-update-colors-h))

(add-hook 'enable-theme-functions #'+modeline-update-colors-h)

(provide 'ui/modeline/config)

;;; ui/modeline/config.el ends here
