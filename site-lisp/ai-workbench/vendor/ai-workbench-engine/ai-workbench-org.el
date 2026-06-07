;;; ai-workbench-org.el --- Org functions for ai-workbench-engine         -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'org-element)
(require 'outline)
(require 'mailcap)                    ;FIXME Avoid this somehow
(eval-when-compile (require 'ai-workbench-request))

;; Functions used for saving/restoring ai-workbench-engine state in Org buffers
(defvar ai-workbench--num-messages-to-send)
(defvar org-entry-property-inherited-from)
(defvar ai-workbench-backend)
(defvar ai-workbench--known-backends)
(defvar ai-workbench-system-prompt)
(defvar ai-workbench-model)
(defvar ai-workbench-temperature)
(defvar ai-workbench-max-tokens)
(defvar ai-workbench--link-type-cache)
(defvar ai-workbench--preset)

(defvar org-link-angle-re)
(defvar org-link-bracket-re)
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function ai-workbench--model-capable-p "ai-workbench-request")
(declare-function ai-workbench--model-mime-capable-p "ai-workbench-request")
(declare-function ai-workbench--model-name "ai-workbench-request")
(declare-function ai-workbench--to-string "ai-workbench-request")
(declare-function ai-workbench--to-number "ai-workbench-request")
(declare-function ai-workbench--intern "ai-workbench-request")
(declare-function ai-workbench-backend-name "ai-workbench-request")
(declare-function ai-workbench--parse-buffer "ai-workbench-request")
(declare-function ai-workbench--parse-directive "ai-workbench-request")
(declare-function ai-workbench--with-buffer-copy "ai-workbench-request")
(declare-function ai-workbench--file-binary-p "ai-workbench-request")
(declare-function ai-workbench--get-buffer-bounds "ai-workbench-engine")
(declare-function ai-workbench--restore-props "ai-workbench-engine")
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-with-wide-buffer "org-macs")
(declare-function org-set-property "org")
(declare-function org-property-values "org")
(declare-function org-open-line "org")
(declare-function org-at-heading-p "org")
(declare-function org-get-heading "org")
(declare-function org-at-heading-p "org")

;; Bundle `org-element-lineage-map' if it's not available (for Org 9.67 or older)
(eval-and-compile
  (if (fboundp 'org-element-lineage-map)
      (progn (declare-function org-element-lineage-map "org-element-ast")
             (defalias 'ai-workbench-org--element-lineage-map 'org-element-lineage-map))
    (defun ai-workbench-org--element-lineage-map (datum fun &optional types with-self first-match)
      "Map FUN across ancestors of DATUM, from closest to furthest.

DATUM is an object or element.  For TYPES, WITH-SELF and
FIRST-MATCH see `org-element-lineage-map'.

This function is provided for compatibility with older versions
of Org."
      (declare (indent 2))
      (setq fun (if (functionp fun) fun `(lambda (node) ,fun)))
      (let ((up (if with-self datum (org-element-parent datum)))
	    acc rtn)
        (catch :--first-match
          (while up
            (when (or (not types) (org-element-type-p up types))
              (setq rtn (funcall fun up))
              (if (and first-match rtn)
                  (throw :--first-match rtn)
                (when rtn (push rtn acc))))
            (setq up (org-element-parent up)))
          (nreverse acc)))))
  (if (fboundp 'org-element-begin)
      (progn (declare-function org-element-begin "org-element")
             (declare-function org-element-end "org-element")
             (declare-function org-element-parent "org-element")
             (defalias 'ai-workbench-org--element-begin 'org-element-begin)
             (defalias 'ai-workbench-org--element-end 'org-element-end)
             (defalias 'ai-workbench-org--element-parent 'org-element-parent))
    (defsubst ai-workbench-org--element-begin (node)
      "Get `:begin' property of NODE."
      (org-element-property :begin node))
    (defsubst ai-workbench-org--element-end (node)
      "Get `:end' property of NODE."
      (org-element-property :end node))
    (defsubst ai-workbench-org--element-parent (node)
      "Return `:parent' property of NODE."
      (org-element-property :parent node))))


;;; User options
(defcustom ai-workbench-org-branching-context nil
  "Use the lineage of the current heading as the context for ai-workbench-engine in Org buffers.

This makes each same level heading a separate conversation
branch.

By default, ai-workbench-engine uses a linear context: all the text up to the
cursor is sent to the LLM.  Enabling this option makes the
context the hierarchical lineage of the current Org heading.  In
this example:

-----
Top level text

* Heading 1
heading 1 text

* Heading 2
heading 2 text

** Heading 2.1
heading 2.1 text
** Heading 2.2
heading 2.2 text
-----

With the cursor at the end of the buffer, the text sent to the
LLM will be limited to

-----
Top level text

* Heading 2
heading 2 text

** Heading 2.2
heading 2.2 text
-----

This makes it feasible to have multiple conversation branches."
  :type 'boolean
  :group 'ai-workbench-engine)

(defcustom ai-workbench-org-ignore-elements '(property-drawer)
  "Types of Org elements to be stripped from the prompt before sending.

By default ai-workbench-engine will remove Org property drawers from the
prompt.  For the full list of available elements, please see
`org-element-all-elements'.

Please note: Removing property-drawer elements is fast, but
adding elements to this list can significantly slow down
`ai-workbench-send'."
  :group 'ai-workbench-engine
  :type '(repeat symbol))

(defcustom ai-workbench-org-validate-link #'always
  "Validate links to be sent as context with ai-workbench-engine queries.

When `ai-workbench-track-media' is enabled, this option determines if a
supported link will be followed and its source included with ai-workbench-engine
queries from Org buffers.  Currently only \"file\" and \"attachment\"
link types are supported (along with web URLs if the model supports
them).

It should be a function that accepts an Org link object and return
non-nil if the link should be followed.

By default, all links are considered valid.

Set this to `ai-workbench-org--link-standalone-p' to only follow links placed
on a line by themselves, separated from surrounding text."
  :group 'ai-workbench-engine
  :type '(choice
          (const :tag "All links" always)
          (const :tag "Standalone links" ai-workbench-org--link-standalone-p)
          (function :tag "Function")))

(defconst ai-workbench-org--link-regex
  (concat "\\(?:" org-link-bracket-re "\\|" org-link-angle-re "\\)")
  "Link regex for `ai-workbench-mode' in Org mode.")


;;; Setting context and creating queries
(defun ai-workbench-org--get-topic-start ()
  "If a conversation topic is set, return it."
  (when (org-entry-get (point) "GPTEL_TOPIC" 'inherit)
    (marker-position org-entry-property-inherited-from)))

(defun ai-workbench-org-set-topic (topic)
  "Set a TOPIC and limit this conversation to the current heading.

This limits the context sent to the LLM to the text between the current
heading (i.e. the heading with the topic set) and the cursor position."
  (interactive
   (list
    (progn
      (or (derived-mode-p 'org-mode)
          (user-error "Support for multiple topics per buffer is only implemented for `org-mode'"))
      (completing-read "Set topic as: "
                       (org-property-values "GPTEL_TOPIC")
                       nil nil (downcase
                                (truncate-string-to-width
                                 (substring-no-properties
                                  (replace-regexp-in-string
                                   "\\s-+" "-"
                                   (org-entry-get nil "ITEM")))
                                 50))))))
  (when (stringp topic) (org-set-property "GPTEL_TOPIC" topic)))

;; NOTE: This can be converted to a cl-defmethod for
;; `ai-workbench--create-prompt-buffer' (conceptually cleaner), but will cause
;; load-order issues in ai-workbench-engine.el and might be harder to debug.
(defun ai-workbench-org--create-prompt-buffer (&optional prompt-end)
  "Return a buffer with the conversation prompt to be sent.

If the region is active limit the prompt text to the region contents.
Otherwise the prompt text is constructed from the contents of the
current buffer up to point, or PROMPT-END if provided.  Its contents
depend on the value of `ai-workbench-org-branching-context', which see."
  (when (use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (setq prompt-end (point-max)))
  (goto-char (or prompt-end (setq prompt-end (point))))
  (let ((topic-start (ai-workbench-org--get-topic-start)))
    (when topic-start
      ;; narrow to GPTEL_TOPIC property scope
      (narrow-to-region topic-start prompt-end))
    (if (and ai-workbench-org-branching-context
             (or (fboundp 'org-element-lineage-map)
                 (prog1 nil
                   (display-warning
                    '(ai-workbench-engine org)
                    "Using `ai-workbench-org-branching-context' requires Org version 9.7 or higher, it will be ignored."))))
        ;; Create prompt from direct ancestors of point
        (save-excursion
          (let* ((org-buf (current-buffer))
                 ;; Collect all heading start positions in the lineage
                 (full-bounds (ai-workbench-org--element-lineage-map
                                  (org-element-at-point) #'ai-workbench-org--element-begin
                                '(headline) 'with-self) )
                 ;; lineage-map returns the full lineage in the unnarrowed
                 ;; buffer.  Remove heading start positions before (point-min)
                 ;; that are invalid due to narrowing, and add (point-min) if
                 ;; it's not already included in the lineage
                 (start-bounds
                  (nconc (cl-delete-if (lambda (p) (< p (point-min)))
                                       full-bounds)
                         (unless (save-excursion (goto-char (point-min))
                                                 (looking-at-p outline-regexp))
                           (list (point-min)))))
                 (end-bounds
                  (cl-loop
                   ;; (car start-bounds) is the begining of the current element,
                   ;; not relevant
                   for pos in (cdr start-bounds)
                   do (goto-char pos) (outline-next-heading)
                   collect (point) into ends
                   finally return (cons prompt-end ends))))
            (ai-workbench--with-buffer-copy org-buf nil nil
              (cl-loop for start in start-bounds
                       for end in end-bounds
                       do (insert-buffer-substring org-buf start end)
                       (goto-char (point-min)))
              (goto-char (point-max))
              (ai-workbench-org--unescape-tool-results)
              (ai-workbench-org--strip-block-headers)
              (when-let* ((ai-workbench-org-ignore-elements ;not copied by -with-buffer-copy
                           (buffer-local-value 'ai-workbench-org-ignore-elements
                                               org-buf)))
                (ai-workbench-org--strip-elements))
              (setq org-complex-heading-regexp ;For org-element-context to run
                    (buffer-local-value 'org-complex-heading-regexp org-buf))
              (setq tab-width      ;Match source indentation for list parsing
                    (buffer-local-value 'tab-width org-buf))
              (current-buffer))))
      ;; Create prompt the usual way
      (let ((org-buf (current-buffer))
            (beg (point-min)))
        (ai-workbench--with-buffer-copy org-buf beg prompt-end
          (ai-workbench-org--unescape-tool-results)
          (ai-workbench-org--strip-block-headers)
          (when-let* ((ai-workbench-org-ignore-elements ;not copied by -with-buffer-copy
                       (buffer-local-value 'ai-workbench-org-ignore-elements
                                           org-buf)))
                (ai-workbench-org--strip-elements))
          (setq org-complex-heading-regexp ;For org-element-context to run
                (buffer-local-value 'org-complex-heading-regexp org-buf))
          (setq tab-width      ;Match source indentation for list parsing
                (buffer-local-value 'tab-width org-buf))
          (current-buffer))))))

(defun ai-workbench-org--strip-elements ()
  "Remove all elements in `ai-workbench-org-ignore-elements' from the prompt."
  (let ((major-mode 'org-mode) element-markers)
    (if (equal '(property-drawer) ai-workbench-org-ignore-elements)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-property-drawer-re nil t)
            ;; ;; Slower but accurate
            ;; (let ((drawer (org-element-at-point)))
            ;;   (when (org-element-type-p drawer 'property-drawer)
            ;;     (delete-region (org-element-begin drawer) (org-element-end drawer))))

            ;; Fast but inexact, can have false positives
            (delete-region (match-beginning 0) (match-end 0))))
      ;; NOTE: Parsing the buffer is extremely slow.  Avoid this path unless
      ;; required.
      ;; NOTE: `org-element-map' takes a third KEEP-DEFERRED argument in newer
      ;; Org versions
      (org-element-map (org-element-parse-buffer 'element nil)
          ai-workbench-org-ignore-elements
        (lambda (node)
          (push (list (ai-workbench-org--element-begin node)
                      (ai-workbench-org--element-end node))
                element-markers)))
      (dolist (bounds element-markers)
        (apply #'delete-region bounds)))))

(defun ai-workbench-org--strip-block-headers ()
  "Remove all ai-workbench-specific block headers and footers.
Every line that matches will be removed entirely.

This removal is necessary to avoid auto-mimicry by LLMs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (rx line-start (literal "#+")
                (or (literal "begin") (literal "end"))
                (or (literal "_tool") (literal "_reasoning")))
            nil t)
      (delete-region (match-beginning 0)
                     (min (point-max) (1+ (line-end-position)))))))

(defun ai-workbench-org--unescape-tool-results ()
  "Undo escapes done to keep results from escaping blocks.
Scans backward for ai-workbench-engine tool text property, then unescapes the block
contents."
  (save-excursion
    (goto-char (point-max))
    (let ((prev-pt (point)))
      (while (> prev-pt (point-min))
        (goto-char
         (previous-single-char-property-change (point) 'ai-workbench-engine))
        (let ((prop (get-text-property (point) 'ai-workbench-engine))
              (backward-progress (point)))
          (when (eq (car-safe prop) 'tool)
            ;; User edits to clean up can potentially insert a tool-call header
            ;; that is propertized.  Tool call headers should not be
            ;; propertized.
            (when (looking-at-p "[[:space:]]*#\\+begin_tool")
              (goto-char (match-end 0)))
            ;; TODO this code is able to put the point behind prev-pt, which
            ;; makes the region inverted.  The `max' catches this, but really
            ;; `read' and `looking-at' are the culprits.  Badly formed tool
            ;; blocks can lead to this being necessary.
            (org-unescape-code-in-region
             (min prev-pt (point)) prev-pt))
          (goto-char (setq prev-pt backward-progress)))))))

(defun ai-workbench-org--link-standalone-p (object)
  "Check if link OBJECT is on a line by itself."
  (when-let* ((par (ai-workbench-org--element-parent object))
              ((eq (org-element-type par) 'paragraph)))
    (and (= (ai-workbench-org--element-begin object)
            (save-excursion
              (goto-char (org-element-property :contents-begin par))
              (skip-chars-forward "\t ")
              (point)))                 ;account for leading space before object
         (<= (- (org-element-property :contents-end par)
                (org-element-property :end object))
             1))))

(defsubst ai-workbench-org--validate-link (link)
  "Validate an Org LINK as sendable under the current ai-workbench-engine settings.

Return a form (validp link-type path . REST), where REST is a list
explaining why sending the link is not supported by ai-workbench-engine.  Only the
first nil value in REST is guaranteed to be correct."
  (let ((mime))
    (if-let* ((link-type (org-element-property :type link))
              (resource-type
               (or (and (member link-type '("attachment" "file")) 'file)
                   (and (ai-workbench--model-capable-p 'url)
                        (member link-type '("http" "https" "ftp")) 'url)))
              (path (org-element-property :path link))
              (user-check (funcall ai-workbench-org-validate-link link))
              (readablep (or (eq resource-type 'url) (file-remote-p path)
                             (file-readable-p path)))
              (mime-valid
               (or (eq resource-type 'url)
                   (and (with-memoization
                            (alist-get (expand-file-name path)
                                       ai-workbench--link-type-cache
                                       nil nil #'string=)
                          (if (ai-workbench--file-binary-p path) t))
                        (setq mime (mailcap-file-name-to-mime-type path))
                        (ai-workbench--model-mime-capable-p mime))
                   t)))
        (list t link-type path resource-type user-check readablep mime-valid mime)
      (list nil link-type path resource-type user-check readablep mime-valid mime))))

(cl-defmethod ai-workbench--parse-media-links ((_mode (eql 'org-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the ai-workbench-engine request."
  (let ((parts) (from-pt))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward ai-workbench-org--link-regex end t)
        (let* ((link (org-element-context))
               (link-status (ai-workbench-org--validate-link link)))
          (cl-destructuring-bind
              (valid type path resource-type user-check readablep mime-valid mime)
              link-status
            (cond
             ((and valid (member type '("file" "attachment")))
              ;; Text file or supported binary file: collect text up to link
              (when-let* ((text (buffer-substring-no-properties
                                 from-pt (ai-workbench-org--element-begin link))))
                (unless (string-blank-p text) (push (list :text text) parts)))
              ;; collect link
              (push (if mime (list :media path :mime mime) (list :textfile path))
                    parts)
              (setq from-pt (point)))
             ((and valid (member type '("http" "https" "ftp")))
              ;; Collect text up to this image, and collect this image url
              (when-let* ((text (buffer-substring-no-properties
                                 from-pt (ai-workbench-org--element-begin link))))
                (unless (string-blank-p text) (push (list :text text) parts)))
              (push (list :url (org-element-property :raw-link link) :mime mime) parts)
              (setq from-pt (point)))
             ((not resource-type)
              (message "Link source not followed for unsupported link type \"%s\"." type))
             ((not user-check)
              (message (if (eq ai-workbench-org-validate-link 'ai-workbench--link-standalone-p)
                           "Ignoring non-standalone link \"%s\"."
                         "Link %s failed to validate, see `ai-workbench-org-validate-link'.")
                       path))
             ((not readablep)
              (message "Ignoring inaccessible file \"%s\"." path))
             ((and (not mime-valid) (eq resource-type 'file))
              (message "Ignoring unsupported binary file \"%s\"." path))))))
      (unless (= from-pt end)
        (push (list :text (buffer-substring-no-properties from-pt end)) parts)))
    (nreverse parts)))

(defun ai-workbench-org--annotate-links (beg end)
  "Annotate Org links whose sources will be sent with `ai-workbench-send'.

Search between BEG and END."
  (when ai-workbench-track-media
    (save-excursion
      (goto-char beg) (forward-line -1)
      (let ((link-ovs (cl-loop for o in (overlays-in (point) end)
                               if (overlay-get o 'ai-workbench-track-media)
                               collect o into os finally return os)))
        (while (re-search-forward ai-workbench-org--link-regex end t)
          (unless (ai-workbench--in-response-p (1- (point)))
            (let* ((link (org-element-context))
                   (from (org-element-begin link))
                   (to (org-element-end link))
                   (link-status (ai-workbench-org--validate-link link))
                   (ov (cl-loop for o in (overlays-in from to)
                                if (overlay-get o 'ai-workbench-track-media)
                                return o)))
              (if ov                    ; Ensure overlay over each link
                  (progn (move-overlay ov from to)
                         (setq link-ovs (delq ov link-ovs)))
                (setq ov (make-overlay from to nil t))
                (overlay-put ov 'ai-workbench-track-media t)
                (overlay-put ov 'evaporate t)
                (overlay-put ov 'priority -80))
              ;; Check if link will be sent, and annotate accordingly
              (ai-workbench--annotate-link ov link-status))))
        (and link-ovs (mapc #'delete-overlay link-ovs))))
    `(jit-lock-bounds ,beg . ,end)))

(defun ai-workbench-org--send-with-props (send-fun &rest args)
  "Conditionally modify SEND-FUN's calling environment.

If in an Org buffer under a heading containing a stored ai-workbench-engine
configuration, use that for requests instead.  This includes the
system message, model and provider (backend), among other
parameters.

ARGS are the original function call arguments."
  (if (derived-mode-p 'org-mode)
      (pcase-let ((`( ,ai-workbench--preset ,ai-workbench-system-prompt ,ai-workbench-backend
                      ,ai-workbench-model ,ai-workbench-temperature ,ai-workbench-max-tokens
                      ,ai-workbench--num-messages-to-send ,ai-workbench-llm-tools)
                   (seq-mapn (lambda (a b) (or a b))
                             (ai-workbench-org--entry-properties)
                             (list ai-workbench--preset ai-workbench-system-prompt ai-workbench-backend
                                   ai-workbench-model ai-workbench-temperature ai-workbench-max-tokens
                                   ai-workbench--num-messages-to-send ai-workbench-llm-tools))))
        (apply send-fun args))
    (apply send-fun args)))

(advice-add 'ai-workbench-send :around #'ai-workbench-org--send-with-props)
(advice-add 'ai-workbench--suffix-send :around #'ai-workbench-org--send-with-props)

;; ;; NOTE: Basic uses in org-mode are covered by advising ai-workbench-send and
;; ;; ai-workbench--suffix-send.  For custom commands it might be necessary to advise
;; ;; ai-workbench-request instead.
;; (advice-add 'ai-workbench-request :around #'ai-workbench-org--send-with-props)


;;; Saving and restoring state
(defun ai-workbench-org--entry-properties (&optional pt)
  "Find ai-workbench-engine configuration properties stored at PT."
  (pcase-let
      ((`(,preset ,system ,backend ,model ,temperature ,tokens ,num ,tools)
         (mapcar
          (lambda (prop) (org-entry-get (or pt (point)) prop 'selective))
          '("GPTEL_PRESET" "GPTEL_SYSTEM" "GPTEL_BACKEND"
            "GPTEL_MODEL" "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
            "GPTEL_NUM_MESSAGES_TO_SEND" "GPTEL_TOOLS"))))
    (when preset (setq preset (ai-workbench--intern preset)))
    (when system
      (setq system (string-replace "\\n" "\n" system)))
    (when backend
      (setq backend (alist-get backend ai-workbench--known-backends
                               nil nil #'equal)))
    (when model (setq model (ai-workbench--intern model)))
    (when temperature
      (setq temperature (ai-workbench--to-number temperature)))
    (when tokens (setq tokens (ai-workbench--to-number tokens)))
    (when num (setq num (ai-workbench--to-number num)))
    (when tools
      (setq tools (cl-loop
                   for tname in (split-string tools)
                   for tool = (with-demoted-errors "ai-workbench-engine: %S"
                                (ai-workbench-get-tool tname))
                   if tool collect tool else do
                   (display-warning
                    '(ai-workbench-engine org tools)
                    (format "Tool %s not found, ignoring" tname)))))
    (list preset system backend model temperature tokens num tools)))

(defun ai-workbench-org--restore-state ()
  "Restore ai-workbench-engine state for Org buffers when turning on `ai-workbench-mode'."
  (save-restriction
    (widen)
    (condition-case status
        (progn
          (when-let* ((bounds (org-entry-get (point-min) "GPTEL_BOUNDS")))
            (ai-workbench--restore-props (read bounds)))
          (pcase-let ((`(,preset ,system ,backend ,model ,temperature ,tokens ,num ,tools)
                       (ai-workbench-org--entry-properties (point-min))))
            (when preset
              (if (ai-workbench-get-preset preset)
                  (progn (ai-workbench--apply-preset
                          preset (lambda (sym val) (set (make-local-variable sym) val)))
                         (setq ai-workbench--preset preset))
                (display-warning
                 '(ai-workbench-engine presets)
                 (format "Could not activate ai-workbench-engine preset `%s' in buffer \"%s\""
                         preset (buffer-name)))))
            (when system (setq-local ai-workbench-system-prompt system))
            (if backend (setq-local ai-workbench-backend backend)
              (message
               (substitute-command-keys
                (concat
                 "Could not activate ai-workbench-engine backend \"%s\"!  "
                 "Switch backends with \\[universal-argument] \\[ai-workbench-send]"
                 " before using ai-workbench-engine."))
               backend))
            (when model (setq-local ai-workbench-model model))
            (when temperature (setq-local ai-workbench-temperature temperature))
            (when tokens (setq-local ai-workbench-max-tokens tokens))
            (when num (setq-local ai-workbench--num-messages-to-send num))
            (when tools (setq-local ai-workbench-llm-tools tools))))
      (:success (message "ai-workbench-engine chat restored."))
      (error (message "Could not restore ai-workbench-engine state, sorry! Error: %s" status)))))

(defun ai-workbench-org-set-properties (pt &optional msg)
  "Store the active ai-workbench-engine configuration under the current heading.

PT is the cursor position by default.  If MSG is non-nil (default),
display a message afterwards.

If a ai-workbench-engine preset has been applied in this buffer, a reference to it is
saved.

Additional metadata is stored only if no preset was applied or if it
differs from the preset specification.  This is limited to the active
ai-workbench-engine model and backend names, the system message, active tools, the
response temperature, max tokens and number of conversation turns to
send in queries.  (See `ai-workbench--num-messages-to-send' for the last one.)"
  (interactive (list (point) t))
  (require 'ai-workbench-engine)
  (let ((preset-spec (and ai-workbench--preset (ai-workbench-get-preset ai-workbench--preset))))
    (if preset-spec
        (org-entry-put pt "GPTEL_PRESET" (ai-workbench--to-string ai-workbench--preset))
      (org-entry-delete pt "GPTEL_PRESET"))

    ;; FIXME: nil can mean "no value was explicitly set by the user" as well as
    ;; "this setting has been set to nil".  We are not yet distinguishing
    ;; between the two when saving Org properties.  This is particularly
    ;; relevant for the system message, whose explicit nil value will not be
    ;; captured when saving Org buffers.

    ;; Model and backend
    (if (ai-workbench--preset-mismatch-value preset-spec :model ai-workbench-model)
        (org-entry-put pt "GPTEL_MODEL" (ai-workbench--model-name ai-workbench-model)))
    (if (ai-workbench--preset-mismatch-value preset-spec :backend ai-workbench-backend)
        (org-entry-put pt "GPTEL_BACKEND" (ai-workbench-backend-name ai-workbench-backend)))
    ;; System message
    (let ((parsed (car-safe (ai-workbench--parse-directive ai-workbench-system-prompt))))
      (if (ai-workbench--preset-mismatch-value preset-spec :system parsed)
          (when parsed
            (org-entry-put pt "GPTEL_SYSTEM" (string-replace "\n" "\\n" parsed)))
        (org-entry-delete pt "GPTEL_SYSTEM")))
    ;; Tools
    (let ((tool-names (mapcar #'ai-workbench-tool-name ai-workbench-llm-tools)))
      (if (ai-workbench--preset-mismatch-value preset-spec :tools tool-names)
          (org-entry-put pt "GPTEL_TOOLS" (string-join tool-names " "))
        (org-entry-delete pt "GPTEL_TOOLS")))
    ;; Temperature, max tokens and cutoff
    (if (and (ai-workbench--preset-mismatch-value preset-spec :temperature ai-workbench-temperature)
             (not (equal (default-value 'ai-workbench-temperature) ai-workbench-temperature)))
        (org-entry-put pt "GPTEL_TEMPERATURE" (number-to-string ai-workbench-temperature))
      (org-entry-delete pt "GPTEL_TEMPERATURE"))
    (if (and (ai-workbench--preset-mismatch-value preset-spec :max-tokens ai-workbench-max-tokens)
             ai-workbench-max-tokens)
        (org-entry-put pt "GPTEL_MAX_TOKENS" (number-to-string ai-workbench-max-tokens))
      (org-entry-delete pt "GPTEL_MAX_TOKENS"))
    (if (and (ai-workbench--preset-mismatch-value
              preset-spec :num-messages-to-send ai-workbench--num-messages-to-send)
             (natnump ai-workbench--num-messages-to-send))
        (org-entry-put pt "GPTEL_NUM_MESSAGES_TO_SEND"
                       (number-to-string ai-workbench--num-messages-to-send))
      (org-entry-delete pt "GPTEL_NUM_MESSAGES_TO_SEND")))
  (when msg
    (message "Added ai-workbench-engine configuration to current headline.")))

(defun ai-workbench-org--save-state ()
  "Write the ai-workbench-engine state to the Org buffer as Org properties."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (org-at-heading-p)
     (org-open-line 1))
   (ai-workbench-org-set-properties (point-min))
   ;; Save response boundaries
   (letrec ((write-bounds
             (lambda (attempts)
               (when-let* ((bounds (ai-workbench--get-buffer-bounds))
                           ;; first value of ((prop . ((beg end val)...))...)
                           (offset (caadar bounds))
                           (offset-marker (set-marker (make-marker) offset)))
                 (org-entry-put (point-min) "GPTEL_BOUNDS"
                                (prin1-to-string (ai-workbench--get-buffer-bounds)))
                 (when (and (not (= (marker-position offset-marker) offset))
                            (> attempts 0))
                   (funcall write-bounds (1- attempts)))))))
     (funcall write-bounds 6))))


;;; Transforming responses
;;;###autoload
(defun ai-workbench--convert-markdown->org (str)
  "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "`+\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
      (pcase (match-string 0)
        ;; Handle backticks
        ((and (guard (eq (char-before) ?`)) ticks)
         (ai-workbench--replace-source-marker (length ticks))
         (save-match-data
           (catch 'block-end
             (while (search-forward ticks nil t)
               (unless (or (eq (char-before (match-beginning 0)) ?`)
                           (eq (char-after) ?`))
                 (ai-workbench--replace-source-marker (length ticks) 'end)
                 (throw 'block-end nil))))))
        ;; Handle headings
        ((and (guard (eq (char-before) ?#)) heading)
         (cond
          ((looking-at "[[:space:]]")   ;Handle headings
           (delete-region (line-beginning-position) (point))
           (insert (make-string (length heading) ?*)))
          ((looking-at "\\+begin_src") ;Overeager LLM switched to using Org src blocks
           (save-match-data (re-search-forward "^#\\+end_src" nil t)))))
        ;; Handle emphasis
        ("**" (cond
               ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)")
               ;;  (delete-char 1))
               ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                              (max (- (point) 3) (point-min)))
                (delete-char -1))))
        ("*"
         (cond
          ((save-match-data
             (and (or (= (point) 2)
                      (looking-back "\\(?:[[:space:]]\\|\s\\)\\(?:_\\|\\*\\)"
                                    (max (- (point) 2) (point-min))))
                  (not (looking-at "[[:space:]]\\|\s"))))
           ;; Possible beginning of emphasis
           (and
            (save-excursion
              (when (and (re-search-forward (regexp-quote (match-string 0))
                                            (line-end-position) t)
                         (looking-at "[[:space:][:punct:]]\\|\s")
                         (not (looking-back "\\(?:[[:space]]\\|\s\\)\\(?:_\\|\\*\\)"
                                            (max (- (point) 2) (point-min)))))
                (delete-char -1) (insert "/") t))
            (progn (delete-char -1) (insert "/"))))
          ((save-excursion
             (ignore-errors (backward-char 2))
             (or (and (bobp) (looking-at "\\*[[:space:]]"))
                 (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")))
           ;; Bullet point, replace with hyphen
           (delete-char -1) (insert "-"))))))
    (buffer-string)))

(defun ai-workbench--replace-source-marker (num-ticks &optional end)
  "Replace markdown style backticks with Org equivalents.

NUM-TICKS is the number of backticks being replaced.  If END is
true these are \"ending\" backticks.

This is intended for use in the markdown to org stream converter."
  (let ((from (match-beginning 0)))
    (delete-region from (point))
    (if (and (= num-ticks 3)
             (save-excursion (beginning-of-line)
                             (skip-chars-forward " \t")
                             (eq (point) from)))
        (insert (if end "#+end_src" "#+begin_src "))
      (insert "="))))

;;;###autoload
(defun ai-workbench--stream-convert-markdown->org (start-marker)
  "Return a Markdown to Org converter.

This function parses a stream of Markdown text to Org
continuously when it is called with successive chunks of the
text stream.

START-MARKER is used to identify the corresponding process when
cleaning up after."
  (letrec ((in-src-block nil)           ;explicit nil to address BUG #183
           (in-org-src-block nil)
           (temp-buf ; NOTE: Switch to `generate-new-buffer' after we drop Emacs 27.1
            (ai-workbench--temp-buffer " *ai-workbench-temp*"))
           (start-pt (make-marker))
           (ticks-total 0)      ;MAYBE should we let-bind case-fold-search here?
           (cleanup-fn
            (lambda (beg _)
              (when (and (equal beg (marker-position start-marker))
                         (eq (current-buffer) (marker-buffer start-marker)))
                (when (buffer-live-p (get-buffer temp-buf))
                  (set-marker start-pt nil)
                  (kill-buffer temp-buf))
                (remove-hook 'ai-workbench-post-response-functions cleanup-fn)))))
    (add-hook 'ai-workbench-post-response-functions cleanup-fn)
    (lambda (str)
      (let ((noop-p) (ticks 0))
        (with-current-buffer (get-buffer temp-buf)
          (save-excursion (goto-char (point-max)) (insert str))
          (when (marker-position start-pt) (goto-char start-pt))
          (when in-src-block (setq ticks ticks-total))
          (save-excursion
            (while (re-search-forward "`\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
              (pcase (match-string 0)
                ("`"
                 ;; Count number of consecutive backticks
                 (backward-char)
                 (while (and (char-after) (eq (char-after) ?`))
                   (forward-char)
                   (if in-src-block (cl-decf ticks) (cl-incf ticks)))
                 ;; Set the verbatim state of the parser
                 (if (and (eobp)
                          ;; Special case heuristic: If the response ends with
                          ;; ^``` we don't wait for more input.
                          ;; FIXME: This can have false positives.
                          (not (save-excursion (beginning-of-line)
                                               (looking-at "^```$"))))
                     ;; End of input => there could be more backticks coming,
                     ;; so we wait for more input
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; We reached a character other than a backtick
                   (cond
                    ;; Ticks balanced, end src block
                    ((= ticks 0)
                     (progn (setq in-src-block nil)
                            (ai-workbench--replace-source-marker ticks-total 'end)))
                    ;; Positive number of ticks, start an src block
                    ((and (> ticks 0) (not in-src-block))
                     (setq ticks-total ticks
                           in-src-block t)
                     (ai-workbench--replace-source-marker ticks-total))
                    ;; Negative number of ticks or in a src block already,
                    ;; reset ticks
                    (t (setq ticks ticks-total)))))
                ;; Handle headings and misguided #+begin_src text
                ((and (guard (and (eq (char-before) ?#) (or (not in-src-block) in-org-src-block)))
                      heading)
                 (if in-org-src-block
                     ;; If we are inside an Org-style src block, look for #+end_src
                     (cond
                      ((< (- (point-max) (point)) 8) ;not enough information to close Org src block
                       (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                      ((looking-at "\\+end_src") ;Close Org src block
                       (setq in-src-block nil in-org-src-block nil)))
                   ;; Otherwise check for Markdown headings, or for #+begin_src
                   (cond
                    ((eobp)       ; Not enough information about the heading yet
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "[[:space:]]") ; Convert markdown heading to Org heading
                     (delete-region (line-beginning-position) (point))
                     (insert (make-string (length heading) ?*)))
                    ((< (- (point-max) (point)) 11) ;Not enough information to check if Org src block
                     (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                    ((looking-at "\\+begin_src ") ;Overeager LLM switched to using Org src blocks
                     (setq in-src-block t in-org-src-block t)))))
                ;; Handle other chars: emphasis, bold and bullet items
                ((and "**" (guard (not in-src-block)))
                 (cond
                  ;; TODO Not sure why this branch was needed
                  ;; ((looking-at "\\*\\(?:[[:word:]]\\|\s\\)") (delete-char 1))

                  ;; Looking back at "w**" or " **"
                  ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\s\\)\\*\\{2\\}"
                                 (max (- (point) 3) (point-min)))
                   (delete-char -1))))
                ((and "*" (guard (not in-src-block)))
                 (if (eobp)
                     ;; Not enough information about the "*" yet
                     (progn (setq noop-p t) (set-marker start-pt (match-beginning 0)))
                   ;; "*" is either emphasis or a bullet point
                   (save-match-data
                     (save-excursion
                       (ignore-errors (backward-char 2))
                       (cond
                        ((and     ; At bob, underscore/asterisk followed by word
                          (or (and (bobp) (looking-at "\\(?:_\\|\\*\\)\\([^[:space:][:punct:]]\\|$\\)"))
                              (looking-at ; word followed by underscore/asterisk
                               "[^[:space:]\n]\\(?:_\\|\\*\\)\\(?:[[:space:][:punct:]]\\|$\\)")
                              (looking-at ; underscore/asterisk followed by word
                               "\\(?:[[:space:]]\\)\\(?:_\\|\\*\\)\\([^[:space:]]\\|$\\)"))
                          (not (looking-at "[[:punct:]]\\(?:_\\|\\*\\)[[:punct:]]")))
                         ;; Emphasis, replace with slashes
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "/"))
                        ((or (and (bobp) (looking-at "\\*[[:space:]]"))
                             (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]"))
                         ;; Bullet point, replace with hyphen
                         (forward-char (if (bobp) 1 2)) (delete-char -1) (insert "-"))))))))))
          (if noop-p
              (buffer-substring (point) start-pt)
            (prog1 (buffer-substring (point) (point-max))
                   (set-marker start-pt (point-max)))))))))

(provide 'ai-workbench-org)
;;; ai-workbench-org.el ends here

;; Silence warnings about `org-element-type-p' and `org-element-parent', see #294.
;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
