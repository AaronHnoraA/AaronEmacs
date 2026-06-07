;;; ai-workbench-engine.el --- Interact with ChatGPT or other LLMs     -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Version: 0.9.9.5
;; Package-Requires: ((emacs "27.1") (transient "0.7.8") (compat "30.1.0.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/karthink/ai-workbench-engine

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; ai-workbench-engine is a simple Large Language Model chat client, with support for multiple
;; models and backends.
;;
;; It works in the spirit of Emacs, available at any time and in any buffer.
;;
;; ai-workbench-engine supports:
;;
;; - The services ChatGPT, Azure, Gemini, Anthropic AI, Together.ai, Perplexity,
;;   AI/ML API, Anyscale, OpenRouter, Groq, PrivateGPT, DeepSeek, Cerebras, Github Models,
;;   GitHub Copilot chat, AWS Bedrock, Novita AI, xAI, Sambanova, Mistral Le
;;   Chat and Kagi (FastGPT & Summarizer).
;; - Local models via Ollama, Llama.cpp, Llamafiles or GPT4All
;;
;; Additionally, any LLM service (local or remote) that provides an
;; OpenAI-compatible API is supported.
;;
;; Features:
;;
;; - Interact with LLMs from anywhere in Emacs (any buffer, shell, minibuffer,
;;   wherever).
;; - LLM responses are in Markdown or Org markup.
;; - Supports conversations and multiple independent sessions.
;; - Supports tool-use to equip LLMs with agentic capabilities.
;; - Supports Model Context Protocol (MCP) integration using the mcp.el package.
;; - Supports multi-modal models (send images, documents).
;; - Supports "reasoning" content in LLM responses.
;; - Save chats as regular Markdown/Org/Text files and resume them later.
;; - You can go back and edit your previous prompts or LLM responses when
;;   continuing a conversation.  These will be fed back to the model.
;; - Redirect prompts and responses easily
;; - Rewrite, refactor or fill in regions in buffers.
;; - Write your own commands for custom tasks with a simple API.
;;
;; Requirements for ChatGPT, Azure, Gemini or Kagi:
;;
;; - You need an appropriate API key.  Set the variable `ai-workbench-api-key' to the
;;   key or to a function of no arguments that returns the key.  (It tries to
;;   use `auth-source' by default)
;;
;; ChatGPT is configured out of the box.  For the other sources:
;;
;; - For Azure: define a ai-workbench-backend with `ai-workbench-make-azure'.
;; - For Gemini: define a ai-workbench-backend with `ai-workbench-make-gemini'.
;; - For Anthropic (Claude): define a ai-workbench-backend with `ai-workbench-make-anthropic'.
;; - For AI/ML API, Together.ai, Anyscale, Groq, OpenRouter, DeepSeek, Cerebras
;;   or Github Models: define a ai-workbench-backend with `ai-workbench-make-openai'.
;; - For PrivateGPT: define a backend with `ai-workbench-make-privategpt'.
;; - For Perplexity: define a backend with `ai-workbench-make-perplexity'.
;; - For Deepseek: define a backend with `ai-workbench-make-deepseek'.
;; - For Kagi: define a ai-workbench-backend with `ai-workbench-make-kagi'.
;;
;; For local models using Ollama, Llama.cpp or GPT4All:
;;
;; - The model has to be running on an accessible address (or localhost)
;; - Define a ai-workbench-backend with `ai-workbench-make-ollama' or `ai-workbench-make-gpt4all'.
;; - Llama.cpp or Llamafiles: Define a ai-workbench-backend with `ai-workbench-make-openai'.
;;
;; Consult the package README for examples and more help with configuring
;; backends.
;;
;; Usage:
;;
;; ai-workbench-engine can be used in any buffer or in a dedicated chat buffer.  The
;; interaction model is simple: Type in a query and the response will be
;; inserted below.  You can continue the conversation by typing below the
;; response.
;;
;; To use this in any buffer:
;;
;; - Call `ai-workbench-send' to send the buffer's text up to the cursor.  Select a
;;   region to send only the region.
;;
;; - You can select previous prompts and responses to continue the conversation.
;;
;; - Call `ai-workbench-send' with a prefix argument to access a menu where you can set
;;   your backend, model and other parameters, or to redirect the
;;   prompt/response.
;;
;; To use this in a dedicated buffer:
;; 
;; - M-x ai-workbench-engine: Start a chat session.
;;
;; - In the chat session: Press `C-c RET' (`ai-workbench-send') to send your prompt.
;;   Use a prefix argument (`C-u C-c RET') to access a menu.  In this menu you
;;   can set chat parameters like the system directives, active backend or
;;   model, or choose to redirect the input or output elsewhere (such as to the
;;   kill ring or the echo area).
;;
;; - You can save this buffer to a file.  When opening this file, turn on
;;   `ai-workbench-mode' before editing it to restore the conversation state and
;;   continue chatting.
;;
;; - To include media files with your request, you can add them to the context
;;   (described next), or include them as links in Org or Markdown mode chat
;;   buffers.  Sending media is disabled by default, you can turn it on globally
;;   via `ai-workbench-track-media', or locally in a chat buffer via the header line.
;; 
;; Include more context with requests:
;;
;; If you want to provide the LLM with more context, you can add arbitrary
;; regions, buffers, files or directories to the query with `ai-workbench-add'.  To add
;; text or media files, call `ai-workbench-add' in Dired or use the dedicated
;; `ai-workbench-add-file'.
;;
;; You can also add context from ai-workbench-engine's menu instead (`ai-workbench-send' with a
;; prefix arg), as well as examine or modify context.
;;
;; When context is available, ai-workbench-engine will include it with each LLM query.
;;
;; LLM Tool use:
;;
;; ai-workbench-engine supports "tool calling" behavior, where LLMs can specify arguments with
;; which to call provided "tools" (elisp functions).  The results of running the
;; tools are fed back to the LLM, giving it capabilities and knowledge beyond
;; what is available out of the box.  For example, tools can perform web
;; searches or API lookups, modify files and directories, and so on.
;;
;; Tools can be specified via `ai-workbench-make-tool', or obtained from other
;; repositories, or from Model Context Protocol (MCP) servers using the mcp.el
;; package.  See the README for details.
;;
;; Tools can be included with LLM queries using ai-workbench-engine's menu, or from
;; `ai-workbench-llm-tools'.
;;
;; Rewrite interface
;;
;; In any buffer: with a region selected, you can rewrite prose, refactor code
;; or fill in the region.  This is accessible via `ai-workbench-rewrite', and also from
;; the `ai-workbench-send' menu.
;;
;; Presets
;;
;; Define a bundle of configuration (model, backend, system message, tools etc)
;; as a "preset" that can be applied together, making it easy to switch between
;; tasks in ai-workbench-engine.  Presets can be saved and applied from ai-workbench-engine's transient
;; menu.  You can also include a cookie of the form "@preset-name" in the prompt
;; to send a request with a preset applied.  This feature works everywhere, but
;; preset cookies are also fontified in chat buffers.
;;
;; ai-workbench-engine in Org mode:
;;
;; ai-workbench-engine offers a few extra conveniences in Org mode:
;;
;; - You can limit the conversation context to an Org heading with
;;   `ai-workbench-org-set-topic'.
;;   
;; - You can have branching conversations in Org mode, where each hierarchical
;;   outline path through the document is a separate conversation branch.
;;   See the variable `ai-workbench-org-branching-context'.
;;   
;; - You can declare the ai-workbench-engine model, backend, temperature, system message and
;;   other parameters as Org properties with the command
;;   `ai-workbench-org-set-properties'.  ai-workbench-engine queries under the corresponding heading
;;   will always use these settings, allowing you to create mostly reproducible
;;   LLM chat notebooks.
;;
;; Finally, ai-workbench-engine offers a general purpose API for writing LLM ineractions that
;; suit your workflow.  See `ai-workbench-request', and `ai-workbench-fsm' for more advanced
;; usage.

;;; Code:
(defconst ai-workbench-version "0.9.9.5")

(declare-function markdown-mode "markdown-mode")
(declare-function ai-workbench-menu "ai-workbench-transient")
(declare-function ai-workbench-system-prompt "ai-workbench-transient")
(declare-function ai-workbench-llm-tools "ai-workbench-transient")
(declare-function ai-workbench--vterm-pre-insert "ai-workbench-integrations")
(declare-function pulse-momentary-highlight-region "pulse")

(declare-function ediff-make-cloned-buffer "ediff-util")
(declare-function ediff-regions-internal "ediff")
(declare-function hl-line-highlight "hl-line")

(declare-function org-escape-code-in-string "org-src")
(declare-function ai-workbench-org-set-topic "ai-workbench-org")
(declare-function ai-workbench-org--save-state "ai-workbench-org")
(declare-function ai-workbench-org--restore-state "ai-workbench-org")
(declare-function ai-workbench-org--annotate-links "ai-workbench-org")
(define-obsolete-function-alias
  'ai-workbench-set-topic 'ai-workbench-org-set-topic "0.7.5")

(declare-function markdown-link-at-pos "markdown-mode")

(eval-when-compile
  (require 'subr-x))
(require 'cl-lib)
(require 'compat nil t)
(require 'url)
(require 'map)
(require 'text-property-search)
(require 'cl-generic)
(eval-and-compile (require 'ai-workbench-request))


;;; User options
(defcustom ai-workbench-pre-response-hook nil
  "Hook run before inserting the LLM response into the current buffer.

This hook is called in the buffer where the LLM response will be
inserted.

Note: this hook only runs if the request succeeds."
  :type 'hook
  :group 'ai-workbench-engine)

(define-obsolete-variable-alias
  'ai-workbench-post-response-hook 'ai-workbench-post-response-functions
  "0.6.0"
  "Post-response functions are now called with two arguments: the
start and end buffer positions of the response.")

(defcustom ai-workbench-post-response-functions nil
  "Abnormal hook run after inserting the LLM response into the current buffer.

This hook is called in the buffer to which the LLM response is
sent, and after the full response has been inserted.  Each
function is called with two arguments: the response beginning and
end positions.

Note: this hook runs even if the request fails.  In this case the
response beginning and end positions are both the cursor position
at the time of the request."
  :type 'hook
  :group 'ai-workbench-engine)

(add-hook 'ai-workbench-post-response-functions 'pulse-momentary-highlight-region 70)

(defcustom ai-workbench-post-stream-hook nil
  "Hook run after each insertion of the LLM's streaming response.

This hook is called in the buffer from which the prompt was sent
to the LLM, and after a text insertion."
  :type 'hook
  :group 'ai-workbench-engine)

(defcustom ai-workbench-pre-tool-call-functions nil
  "Abnormal hook called before each tool call.

Each hook function is called a plist with the following keys:

:name - the name of the tool being called, a string
:args - a plist of the tool call arguments, as specified in the tool
        definition.  For a hypothetical edit_file tool that takes three
        arguments, a FILENAME, an ORIGINAL and REPLACEMENT strings, this
        plist is structured as

  (:filename \"/path/to/file.md\"
   :original \"...\"
   :replacement \"...\")

:buffer  - The name of the buffer from which the request was sent.
:backend - The name of the ai-workbench-engine backend used for the request.
:model   - The name of the ai-workbench-engine model used for the request.

The function can work by side effects and return nil, or return a plist
with one or more of the following keys.

:stop        - If non-nil, stop the request entirely.
:stop-reason - If :stop is non-nil, the reason for stopping.  Intended
               for the user, not the LLM.

:block -   If non-nil, continue the request but block this tool call and
           mark it as having erred.  Can be a string to send as the
           result instead, typically an explanation for why the tool was
           not run.  Intended for the LLM, not the user.

:confirm - Whether the tool call should seek confirmation from the user.
           t and nil are both meaningful, signifying that the tool call
           should and should not seek user confirmation, respectively.
           When present, this key overrides all other confirmation
           options (such as `ai-workbench-confirm-tool-calls' and the tool's
           CONFIRM slot).
:args    - The updated argument plist for the tool call.
:result  - The result of this tool call, used instead of the tool call
           output.  Not marked as an error."
  :type 'hook
  :group 'ai-workbench-engine)

(defcustom ai-workbench-post-tool-call-functions nil
  "Abnormal hook called after each tool call.

Each hook function is called a plist with the following keys:

:name - the name of the tool being called, a string
:args - a plist of the tool call arguments, as specified in the tool
        definition.  For a hypothetical edit_file tool that takes three
        arguments, a FILENAME, an ORIGINAL and REPLACEMENT strings, this
        plist is structured as

  (:filename \"/path/to/file.md\"
   :original \"...\"
   :replacement \"...\")

:result  - The tool call result, serialized to a string.
:buffer  - The name of the buffer from which the request was sent.
:backend - The name of the ai-workbench-engine backend used for the request.
:model   - The name of the ai-workbench-engine model used for the request.

The function can work by side effects and return nil, or return a plist
with one or more of the following keys.

:stop        - If non-nil, stop the request entirely.
:stop-reason - If :stop is non-nil, the reason for stopping.  Intended
               for the user, not the LLM.

:block -   If non-nil, continue the request but block this tool call and
           mark it as having erred.  Can be a string to send as the
           result instead, typically an explanation for why the tool was
           not run.  Intended for the LLM, not the user.

:result  - The updated result of this tool call, used instead of the
           tool call output.  Not marked as an error."
  :type 'hook
  :group 'ai-workbench-engine)

(defcustom ai-workbench-save-state-hook nil
  "Hook run before ai-workbench-engine saves model parameters to a file.

You can use this hook to store additional conversation state or
model parameters to the chat buffer, or to modify the buffer in
some other way."
  :type 'hook
  :group 'ai-workbench-engine)

(defcustom ai-workbench-default-mode (if (fboundp 'markdown-mode)
				  'markdown-mode
				'text-mode)
  "The default major mode for dedicated chat buffers.

If `markdown-mode' is available, it is used.  Otherwise ai-workbench-engine
defaults to `text-mode'."
  :type 'function
  :group 'ai-workbench-engine)

(defcustom ai-workbench-use-header-line t
  "Whether `ai-workbench-mode' should use header-line for status information.

When set to nil, use the mode line for (minimal) status
information and the echo area for messages."
  :type 'boolean
  :group 'ai-workbench-engine)

;; Set minimally to avoid display-buffer action alist conflicts (#533)
(defcustom ai-workbench-display-buffer-action `(nil (body-function . ,#'select-window))
  "The action used to display ai-workbench-engine chat buffers.

The ai-workbench-engine buffer is displayed in a window using

  (display-buffer BUFFER ai-workbench-display-buffer-action)

The value of this option has the form (FUNCTION . ALIST),
where FUNCTION is a function or a list of functions.  Each such
function should accept two arguments: a buffer to display and an
alist of the same form as ALIST.  See info node `(elisp)Choosing
Window' for details."
  :type display-buffer--action-custom-type
  :group 'ai-workbench-engine)

(defcustom ai-workbench-crowdsourced-prompts-file
  (let ((cache-dir (or (eval-when-compile
			 (require 'xdg)
			 (xdg-cache-home))
                       user-emacs-directory)))
    (expand-file-name "ai-workbench-crowdsourced-prompts.csv" cache-dir))
  "File used to store crowdsourced system prompts.

These are prompts cached from an online source (see
`ai-workbench--crowdsourced-prompts-url'), and can be set from the
transient menu interface provided by `ai-workbench-menu'."
  :type 'file
  :group 'ai-workbench-engine)

(defvar ai-workbench-refresh-buffer-hook '(jit-lock-refontify)
  "Hook run in ai-workbench-engine buffers after changing ai-workbench-engine's configuration.

This hook runs in ai-workbench-engine chat buffers after making a change to ai-workbench-engine's
configuration that might require a UI update.")

(defvar-local ai-workbench--bounds nil)
(put 'ai-workbench--bounds 'safe-local-variable #'listp)

(defvar ai-workbench--preset nil
  "Name of last applied ai-workbench-engine preset.

For internal use only.")
(put 'ai-workbench--preset 'safe-local-variable #'symbolp)

(defvar-local ai-workbench--tool-names nil
  "Store to persist tool names to file across Emacs sessions.

Note: Changing this variable does not affect ai-workbench-engine\\='s behavior
in any way.")
(put 'ai-workbench--tool-names 'safe-local-variable #'listp)

(defvar-local ai-workbench--backend-name nil
  "Store to persist backend name across Emacs sessions.

Note: Changing this variable does not affect ai-workbench-engine\\='s behavior
in any way.")
(put 'ai-workbench--backend-name 'safe-local-variable #'stringp)

(defvar-local ai-workbench--old-header-line nil)

(defvar ai-workbench--markdown-block-map
  (define-keymap
    "<tab>" 'ai-workbench-markdown-cycle-block
    "TAB"   'ai-workbench-markdown-cycle-block)
  "Keymap for folding and unfolding Markdown code blocks.")


;;; Utility functions
(defun ai-workbench--modify-value (original new-spec)
  "Combine ORIGINAL with NEW-SPEC and return the new result.

This function is non-destructive, ORIGINAL is not modified.

NEW-SPEC is either a declarative action spec (plist) of the form
 (:key val ...), or a simple value.  Recognized spec keys are :append,
:prepend, :eval, :function and :merge.  If NEW-SPEC does not have this
form it is returned as is.

- :append and :prepend will append/prepend val (a list or string) to ORIGINAL.
  Actions on strings are idempotent, they will only be appended/prepended once.
- :eval will evaluate val and return the result, and
- :function will call val with ORIGINAL as its argument, and return the result.
- :merge will treat ORIGINAL and NEW-SPEC as plists and return a merged plist,
  with NEW-SPEC taking precedence."
  (if (not (and (consp new-spec) (keywordp (car new-spec))))
      new-spec
    (let ((current original) (tail new-spec))
      (while tail
        (let ((key (pop tail)) (form (pop tail)))
          (setq current
                (pcase key
                  (:append (if (stringp form)
                               (if (string-suffix-p form current t)
                                   current (concat current form))
                             (append current form)))
                  (:prepend (if (stringp form)
                                (if (string-prefix-p form current t)
                                    current (concat form current))
                              (append form current)))
                  (:eval (eval form t))
                  (:function (funcall form current))
                  (:merge (ai-workbench--merge-plists (copy-sequence current) form))
                  (_ new-spec)))))
      current)))

(defun ai-workbench-auto-scroll ()
  "Scroll window if LLM response continues below viewport.

Note: This will move the cursor."
  (when-let* ((win (get-buffer-window (current-buffer) 'visible))
              ((not (pos-visible-in-window-p (point) win)))
              (scroll-error-top-bottom t))
    (condition-case nil
        (with-selected-window win
          (scroll-up-command))
      (error nil))))

(defun ai-workbench-beginning-of-response (&optional beg _end arg)
  "Move point to BEG, or to the beginning of the LLM response ARG times."
  (interactive (list nil nil
                     (prefix-numeric-value current-prefix-arg)))
  (ai-workbench-end-of-response beg nil (- (or arg 1))))

(defun ai-workbench-end-of-response (&optional beg end arg)
  "Move point to end of LLM response.

With BEG, start search from BEG when ARG is negative.
With END, start search from END when ARG is positive.
Otherwise move ARG times, defaulting to 1."
  (interactive (list nil nil
                     (prefix-numeric-value current-prefix-arg)))
  (unless arg (setq arg 1))
  (let* ((search (if (> arg 0)
                     #'text-property-search-forward
                   #'text-property-search-backward))
         (goto-prefix-end
          (lambda () (when-let* ((prefix (ai-workbench-prompt-prefix-string))
                            ((not (string-empty-p prefix)))
                            ((looking-at (concat "\n\\{1,2\\}"
                                                 (regexp-quote prefix) "?"))))
                  (goto-char (match-end 0)))))
         (goto-prefix-beg
          (lambda () (when-let* ((prefix (ai-workbench-response-prefix-string))
                            ((not (string-empty-p prefix)))
                            ((looking-back (concat (regexp-quote prefix) "?")
                                           (point-min))))
                  (goto-char (match-beginning 0))))))
    (cond
     ((and end (> arg 0)) (goto-char end) (cl-decf arg) (funcall goto-prefix-end))
     ((and beg (< arg 0)) (goto-char beg) (cl-incf arg) (funcall goto-prefix-beg)))
    (dotimes (_ (abs arg))
      (funcall search 'ai-workbench-engine 'response t)
      (if (> arg 0)
          (funcall goto-prefix-end)
        (funcall goto-prefix-beg)))))

(defun ai-workbench-markdown-cycle-block ()
  "Cycle code blocks in Markdown."
  (interactive)
  (save-excursion
    (forward-line 0)
    (let (start end (parity 0))
      (cond            ;Find start and end of block, with possible nested blocks
       ((looking-at-p "^``` *\n")       ;end of block, find corresponding start
        (setq parity -1 end (line-end-position))
        (while (and (not (= parity 0)) (not (bobp)) (forward-line -1))
          (cond ((looking-at-p "^``` *\n") (cl-decf parity))
                ((looking-at-p "^``` ?[a-z]") (cl-incf parity))))
        (when (= parity 0) (setq start (point))))

       ((looking-at-p "^``` ?[a-z]") ;beginning of block, find corresponding end
        (setq parity 1 start (point))
        (while (and (not (= parity 0)) (not (eobp)) (forward-line 1))
          (cond ((looking-at-p "^``` *\n") (cl-decf parity))
                ((looking-at-p "^``` ?[a-z]") (cl-incf parity))))
        (when (= parity 0) (setq end (line-end-position)))))
      (when (and start end)
        (goto-char start)
        (end-of-line)
        (pcase-let* ((`(,value . ,hide-ov)
                      (get-char-property-and-overlay (point) 'invisible)))
          (if (and hide-ov (eq value t))
              (delete-overlay hide-ov)
            (unless hide-ov (setq hide-ov (make-overlay (point) end)))
            (overlay-put hide-ov 'evaporate t)
            (overlay-put hide-ov 'invisible t)
            (overlay-put hide-ov 'before-string
                         (propertize "..." 'face 'shadow))))))))

(defsubst ai-workbench--annotate-link (ov link-status)
  "Annotate link overlay OV according to LINK-STATUS.

LINK-STATUS is a list of link properties relevant to ai-workbench-engine queries, of
the form (valid . REST).  See `ai-workbench-markdown--validate-link' for
details.  Indicate the (in)validity of the link for inclusion with ai-workbench-engine
queries via OV."
  (cl-destructuring-bind
      (valid _ path resource-type user-check readablep mime-valid _mime)
      link-status
    (if valid
        (progn
          (overlay-put
           ov 'before-string
           (concat (propertize "SEND" 'face '(:inherit success :height 0.8))
                   (if (display-graphic-p)
                       (propertize " " 'display '(space :width 0.5)) " ")))
          (overlay-put ov 'help-echo
                       (format "Sending %s %s with ai-workbench-engine requests" resource-type path)))
      (overlay-put ov 'before-string
                   (concat (propertize "!" 'face '(:inherit error))
                           (propertize " " 'display '(space :width 0.3))))
      (overlay-put
       ov 'help-echo
       (concat
        "Sending only link text with ai-workbench-engine requests, "
        "this link will not be followed to its source.\n\nReason: "
        (cond
         ((not resource-type) "Not a supported link type\
 (Only \"file\" and \"attachment\" are supported)")
         ((not user-check)
          (concat
           "\nNot a standalone link -- separate link from text around it. \n           (OR)
Link failed to validate, see `ai-workbench-markdown-validate-link' or `ai-workbench-org-validate-link'."))
         ((not readablep) (format "File %s is not readable" path))
         ((not mime-valid)
          (pcase resource-type
            ('file (format "%s does not support binary file %s" ai-workbench-model path))
            ('url (format "%s does not support fetching non-image URLs" ai-workbench-model))))))))))

(defun ai-workbench--annotate-link-clear (&optional beg end)
  "Delete all ai-workbench-engine org link annotations between BEG and END."
  (mapc #'delete-overlay
        (cl-delete-if-not
         (lambda (o) (overlay-get o 'ai-workbench-track-media))
         (overlays-in (or beg (point-min)) (or end (point-max))))))

;;;; Response text recognition

(defun ai-workbench--get-buffer-bounds ()
  "Return the ai-workbench-engine response boundaries in the buffer as an alist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let ((bounds) (prev-pt (point)))
        (while (and (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'ai-workbench-engine nil (point-min))))
          (when-let* ((prop (get-char-property (point) 'ai-workbench-engine)))
            (let* ((prop-name (if (symbolp prop) prop (car prop)))
                   (val (when (consp prop) (cdr prop)))
                   (bound (if val
                              (list (point) prev-pt val)
                            (list (point) prev-pt))))
              (push bound (alist-get prop-name bounds))))
          (setq prev-pt (point)))
        bounds))))

(define-obsolete-function-alias
  'ai-workbench--get-bounds 'ai-workbench--get-response-bounds "0.9.8")

(defun ai-workbench--get-response-bounds ()
  "Return the ai-workbench-engine response boundaries around point."
  (let (prop)
    (save-excursion
      (when (text-property-search-forward
                          'ai-workbench-engine 'response t)
        (when (setq prop (text-property-search-backward
                          'ai-workbench-engine 'response t))
          (cons (prop-match-beginning prop)
                (prop-match-end prop)))))))

(defun ai-workbench--in-response-p (&optional pt)
  "Check if position PT is inside a ai-workbench-engine response."
  (eq (get-char-property (or pt (point)) 'ai-workbench-engine) 'response))

(defun ai-workbench--at-response-history-p (&optional pt)
  "Check if ai-workbench-engine response at position PT has variants."
  (get-char-property (or pt (point)) 'ai-workbench-history))


;;; Saving and restoring state

(defun ai-workbench--restore-props (bounds-alist)
  "Restore text properties from BOUNDS-ALIST.
BOUNDS-ALIST is (PROP . BOUNDS).  BOUNDS is a list of BOUND.  Each BOUND
is either (BEG END VAL) or (BEG END).

For (BEG END VAL) forms, even if VAL is nil, the ai-workbench-engine property will be
set to (PROP . VAL).  For (BEG END) forms, except when PROP is response,
the ai-workbench-engine property is set to just PROP.

The legacy structure, a list of (BEG . END) is also supported and will be
applied before being re-persisted in the new structure."
  ;; Run silently to avoid `ai-workbench--inherit-stickiness' and other hooks that
  ;; might modify the ai-workbench-engine text property.
  (with-silent-modifications
    (if (symbolp (caar bounds-alist))
        (mapc
         (lambda (bounds)
           (let* ((prop (pop bounds)))
             (mapc
              (lambda (bound)
                (let ((prop-has-val (> (length bound) 2)))
                  (add-text-properties
                   (pop bound) (pop bound)
                   (if (eq prop 'response)
                       '(ai-workbench-engine response front-sticky (ai-workbench-engine))
                     (list 'ai-workbench-engine
                           (if prop-has-val
                               (cons prop (pop bound))
                             prop))))))
              bounds)))
         bounds-alist)
      (mapc (lambda (bound)
              (add-text-properties
               (car bound) (cdr bound) '(ai-workbench-engine response front-sticky (ai-workbench-engine))))
            bounds-alist))))

(defun ai-workbench--restore-state ()
  "Restore ai-workbench-engine state when turning on `ai-workbench-mode'."
  (when (buffer-file-name)
    (if (derived-mode-p 'org-mode)
        (progn
          (require 'ai-workbench-org)
          (ai-workbench-org--restore-state))
      (when ai-workbench--bounds
        (ai-workbench--restore-props ai-workbench--bounds)
        (message "ai-workbench-engine chat restored."))
      (when ai-workbench--preset
        (if (ai-workbench-get-preset ai-workbench--preset)
            (ai-workbench--apply-preset
             ai-workbench--preset (lambda (sym val) (set (make-local-variable sym) val)))
          (display-warning
           '(ai-workbench-engine presets)
           (format "Could not activate ai-workbench-engine preset `%s' in buffer \"%s\""
                   ai-workbench--preset (buffer-name)))))
      (when ai-workbench--backend-name
        (if-let* ((backend (alist-get
                            ai-workbench--backend-name ai-workbench--known-backends
                            nil nil #'equal)))
            (setq-local ai-workbench-backend backend)
          (message
           (substitute-command-keys
            (concat
             "Could not activate ai-workbench-engine backend \"%s\"!  "
             "Switch backends with \\[universal-argument] \\[ai-workbench-send]"
             " before using ai-workbench-engine."))
           ai-workbench--backend-name)))
      (when ai-workbench--tool-names
        (if-let* ((tools (cl-loop
                          for tname in ai-workbench--tool-names
                          for tool = (with-demoted-errors "ai-workbench-engine: %S"
                                       (ai-workbench-get-tool tname))
                          if tool collect tool else do
                          (display-warning
                           '(ai-workbench-engine org tools)
                           (format "Tool %s not found, ignoring" tname)))))
            (setq-local ai-workbench-llm-tools tools))))))

(defun ai-workbench--save-state ()
  "Write the ai-workbench-engine state to the buffer.

This saves chat metadata when writing the buffer to disk.  To
restore a chat session, turn on `ai-workbench-mode' after opening the
file.

If a ai-workbench-engine preset has been applied in this buffer, a reference to it is
saved.

Additional metadata is stored only if no preset was applied or if it
differs from the preset specification.  This is limited to the active
ai-workbench-engine model and backend names, the system message, active tools, the
response temperature, max tokens and number of conversation turns to
send in queries.  (See `ai-workbench--num-messages-to-send' for the last one.)"
  (run-hooks 'ai-workbench-save-state-hook)
  (if (derived-mode-p 'org-mode)
      (progn
        (require 'ai-workbench-org)
        (ai-workbench-org--save-state))
    (let ((print-escape-newlines t)
          (preset-spec (and ai-workbench--preset
                            (ai-workbench-get-preset ai-workbench--preset))))
      (save-excursion
        (save-restriction

          (if preset-spec
              (add-file-local-variable 'ai-workbench--preset ai-workbench--preset)
            (delete-file-local-variable 'ai-workbench--preset))

          ;; Model and backend
          (if (ai-workbench--preset-mismatch-value preset-spec :model ai-workbench-model)
              (add-file-local-variable 'ai-workbench-model ai-workbench-model))
          (if (ai-workbench--preset-mismatch-value preset-spec :backend ai-workbench-backend)
              (add-file-local-variable 'ai-workbench--backend-name
                                       (ai-workbench-backend-name ai-workbench-backend)))
          ;; System message compat
          ;; TODO(v1.0): Remove this fix for duplicate system prompts
          (delete-file-local-variable 'ai-workbench--system-message)
          ;; System message
          (let ((parsed (car-safe (ai-workbench--parse-directive ai-workbench-system-prompt))))
            (if (ai-workbench--preset-mismatch-value preset-spec :system parsed)
                (add-file-local-variable 'ai-workbench-system-prompt parsed)
              (delete-file-local-variable 'ai-workbench-system-prompt)))
          ;; Tools
          (let ((tool-names (mapcar #'ai-workbench-tool-name ai-workbench-llm-tools)))
            (if (ai-workbench--preset-mismatch-value preset-spec :tools tool-names)
                (add-file-local-variable 'ai-workbench--tool-names tool-names)
              (delete-file-local-variable 'ai-workbench--tool-names)))
          ;; Temperature, max tokens and cutoff
          (if (and (ai-workbench--preset-mismatch-value preset-spec :temperature ai-workbench-temperature)
                   (not (equal (default-value 'ai-workbench-temperature) ai-workbench-temperature)))
              (add-file-local-variable 'ai-workbench-temperature ai-workbench-temperature)
            (delete-file-local-variable 'ai-workbench-temperature))
          (if (and (ai-workbench--preset-mismatch-value preset-spec :max-tokens ai-workbench-max-tokens)
                   ai-workbench-max-tokens)
              (add-file-local-variable 'ai-workbench-max-tokens ai-workbench-max-tokens)
            (delete-file-local-variable 'ai-workbench-max-tokens))
          (if (and (ai-workbench--preset-mismatch-value
                    preset-spec :num-messages-to-send ai-workbench--num-messages-to-send)
                   (natnump ai-workbench--num-messages-to-send))
              (add-file-local-variable 'ai-workbench--num-messages-to-send
                                       ai-workbench--num-messages-to-send)
            (delete-file-local-variable 'ai-workbench--num-messages-to-send))
          (add-file-local-variable 'ai-workbench--bounds (ai-workbench--get-buffer-bounds)))))))


;;; Minor modes and UI

;; NOTE: It's not clear that this is the best strategy:
(cl-pushnew '(ai-workbench-engine . t) (default-value 'text-property-default-nonsticky)
            :test #'equal)

(defun ai-workbench--inherit-stickiness (beg end _pre)
  "Mark any change to an LLM response region as a response.

Intended to be added to `after-change-functions' in ai-workbench-engine chat buffers,
which see for BEG, END and PRE."
  (and (/= beg end) (< end (point-max))
       (and-let* ((val (get-text-property end 'ai-workbench-engine)))
         (add-text-properties
          beg end `(ai-workbench-engine ,val front-sticky (ai-workbench-engine))))))

(defun ai-workbench-markdown--annotate-links (beg end)
  "Annotate Markdown links whose sources will be sent with `ai-workbench-send'.

Search between BEG and END."
  (when ai-workbench-track-media
    (save-excursion
      (goto-char beg) (forward-line -1)
      (let ((link-ovs (cl-loop for o in (overlays-in (point) end)
                               if (overlay-get o 'ai-workbench-track-media)
                               collect o into os finally return os)))
        (while (re-search-forward ai-workbench-markdown--link-regex end t)
          (unless (ai-workbench--in-response-p (1- (point)))
            (let* ((link (markdown-link-at-pos (point)))
                   (from (car link)) (to (cadr link))
                   (link-status (ai-workbench-markdown--validate-link link))
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

;;;; Header line formatting
(defvar ai-workbench--header-line-info
  '(:eval
    (let* ((model (ai-workbench--model-name ai-workbench-model))
           (system
            (propertize
             (buttonize
              (format "[Prompt: %s]"
                      (or (car-safe (rassoc ai-workbench-system-prompt ai-workbench-directives))
                          (ai-workbench--describe-directive ai-workbench-system-prompt 15)))
              (lambda (&rest _) (ai-workbench-system-prompt)))
             'mouse-face 'highlight
             'help-echo "System message for session"))
           (context
            (and ai-workbench-context
                 (cl-loop
                  for entry in ai-workbench-context
                  if (bufferp (or (car-safe entry) entry)) count it into bufs
                  else count (stringp (or (car-safe entry) entry)) into files
                  finally return
                  (propertize
                   (buttonize
                    (concat "[" (and (> bufs 0) (format "%d buf" bufs))
                            (and (> bufs 1) "s")
                            (and (> bufs 0) (> files 0) ", ")
                            (and (> files 0) (format "%d file" files))
                            (and (> files 1) "s") "]")
                    (lambda (&rest _)
                      (require 'ai-workbench-context)
                      (ai-workbench-context--buffer-setup)))
                   'mouse-face 'highlight
                   'help-echo "Active ai-workbench-engine context"))))
           (toggle-track-media
            (lambda (&rest _)
              (setq-local ai-workbench-track-media (not ai-workbench-track-media))
              (if ai-workbench-track-media
                  (progn
                    (run-hooks 'ai-workbench-refresh-buffer-hook)
                    (message "Sending media from included links."))
                (without-restriction (ai-workbench--annotate-link-clear))
                (message "Ignoring links.  Only link text will be sent."))
              (run-at-time 0 nil #'force-mode-line-update)))
           (track-media
            (and (ai-workbench--model-capable-p 'media)
                 (if ai-workbench-track-media
                     (propertize
                      (buttonize "[Media: Send]" toggle-track-media)
                      'mouse-face 'highlight
                      'help-echo
                      "Sending media from links/urls when supported.\nClick to toggle")
                   (propertize
                    (buttonize "[Media: No]" toggle-track-media)
                    'mouse-face 'highlight
                    'help-echo
                    "Ignoring media from links/urls.\nClick to toggle"))))
           (toggle-tools (lambda (&rest _) (interactive)
                           (run-at-time 0 nil
                                        (lambda () (call-interactively #'ai-workbench-llm-tools)))))
           (tools (when (and ai-workbench-use-tools ai-workbench-llm-tools)
                    (propertize
                     (buttonize (pcase (length ai-workbench-llm-tools)
                                  (0 "[No tools]") (1 "[1 tool]")
                                  (len (format "[%d tools]" len)))
                                toggle-tools)
                     'mouse-face 'highlight
                     'help-echo "Select tools")))
           (usage
            (and-let* ((idx (car-safe ai-workbench--token-usage-strings))
                       (entry (or (nth (1+ idx) ai-workbench--token-usage-strings)
                                  "[usage...]"))
                       (noinfo "[No info]")
                       (toggle-usage
                        (lambda (strings) (interactive)
                          (and (car-safe strings)
                               (cl-callf (lambda (pos) (% (1+ pos) 2)) (car strings))))))
              (buttonize entry toggle-usage ai-workbench--token-usage-strings
                         (concat "Token usage (C = cached tokens)\nLast request: "
                                 (or (cadr ai-workbench--token-usage-strings) noinfo)
                                 "\nThis buffer:  "
                                 (or (caddr ai-workbench--token-usage-strings) noinfo))))))
      (let ((rhs (concat
                  usage (and usage " ") tools (and tools " ")
                  track-media (and track-media " ")
                  context (and context " ") system " "
                  (propertize
                   (buttonize (concat "[" model "]")
                              (lambda (&rest _) (ai-workbench-menu)))
                   'mouse-face 'highlight
                   'help-echo "Model in use"))))
        (concat
         (propertize
          " " 'display
          (if (and (fboundp 'string-pixel-width)
                   (display-graphic-p))
              `(space :align-to (- right (,(string-pixel-width rhs))))
            `(space :align-to (- right ,(+ 5 (string-width rhs))))))
         rhs))))
  "Information segment for the header-line in `ai-workbench-mode'.")

(defun ai-workbench-use-header-line ()
  "Set up the header-line for a ai-workbench-engine buffer.

It is composed of three segments: the backend name, the
status (Ready/Waiting etc) and the info segment, showing the current
context, tools, system prompt, model and more."
  (setq
   header-line-format
   (list '(:eval (concat (propertize " " 'display '(space :align-to 0))
                         (format "%s" (ai-workbench-backend-name ai-workbench-backend))))
         (propertize " Ready" 'face 'success)
         ai-workbench--header-line-info)))

;;;; Token usage display UI
(defvar-local ai-workbench--token-usage nil
  "Token usage details for ai-workbench-engine.

This is a list of two plists, the token usage for the ongoing/last
request and for the buffer/session:

  ((:input ... :output ... :cache ... :cached ...)
   (:input ... :output ... :cache ... :cached ...))")

(defvar-local ai-workbench--token-usage-strings nil
  "Token usage strings formatted for display.

This is a list (IDX REQUEST BUFFER), where IDX is the usage type to
display (0 or 1), and REQUEST and BUFFER are the token usage for the
last request and the buffer/session.")

(defun ai-workbench--format-token-usage (token-plist)
  "Format TOKEN-PLIST (token usage) for display."
  (when token-plist
    (let ((input (plist-get token-plist :input))
          (output (plist-get token-plist :output))
          (cached (plist-get token-plist :cached)))
      (concat
       "[" (and input
                (concat (file-size-human-readable input 'si)
                        (and (numberp cached) (/= cached 0)
                             (format ", C%s"
                                     (file-size-human-readable cached 'si)))
                        "↑"))
       (and output (concat " " (file-size-human-readable output 'si) "↓")) "]"))))

(defun ai-workbench--update-token-usage (tokens &optional tokens-full)
  "Update token usage information for buffer from TOKENS.

TOKENS is the token usage for the current turn.
TOKENS-FULL is the cumulative token usage for the request (so far)."
  (when tokens
    (let ((tokens-full (or tokens-full tokens)))
      (if (not ai-workbench--token-usage)
          (setq ai-workbench--token-usage (list tokens-full (copy-sequence tokens-full)))
        (setcar ai-workbench--token-usage tokens-full)
        (cl-callf ai-workbench--sum-plists (nth 1 ai-workbench--token-usage) tokens))
      (unless ai-workbench--token-usage-strings ;show buffer usage by default
        (setq ai-workbench--token-usage-strings (list 1)))
      (setcdr ai-workbench--token-usage-strings
              (mapcar #'ai-workbench--format-token-usage ai-workbench--token-usage)))))

;;;; Minor mode
;;;###autoload
(define-minor-mode ai-workbench-mode
  "Minor mode for interacting with LLMs."
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'ai-workbench-send)
    map)
  (if ai-workbench-mode
      (progn
        (unless (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
          (ai-workbench-mode -1)
          (user-error (format "`ai-workbench-mode' is not supported in `%s'." major-mode)))
        (add-hook 'before-save-hook #'ai-workbench--save-state nil t)
        (add-hook 'after-change-functions 'ai-workbench--inherit-stickiness nil t)
        (ai-workbench--prettify-preset)
        (cond
         ((derived-mode-p 'org-mode)
          (require 'ai-workbench-org)
          (jit-lock-register 'ai-workbench-org--annotate-links)
          ;; Work around bug in `org-fontify-extend-region'.
          (add-hook 'ai-workbench-post-response-functions #'font-lock-flush nil t))
         ((derived-mode-p 'markdown-mode)
          (font-lock-add-keywords ;keymap is a font-lock-managed property in markdown-mode
           nil '(("^```[ \t]*\\([[:alpha:]][^\n]*\\)?$" ;match code fences
                  0 (list 'face nil 'keymap ai-workbench--markdown-block-map))))
          (jit-lock-register 'ai-workbench-markdown--annotate-links)))
        (ai-workbench--restore-state)
        (if ai-workbench-use-header-line
            (progn (setq ai-workbench--old-header-line header-line-format)
                   (ai-workbench-use-header-line))
          (ai-workbench--update-status " Ready" 'success)))
    (remove-hook 'before-save-hook #'ai-workbench--save-state t)
    (remove-hook 'after-change-functions 'ai-workbench--inherit-stickiness t)
    (cond
     ((derived-mode-p 'org-mode)
      (jit-lock-unregister #'ai-workbench-org--annotate-links)
      (without-restriction (ai-workbench--annotate-link-clear)))
     ((derived-mode-p 'markdown-mode)
      (jit-lock-unregister #'ai-workbench-markdown--annotate-links)
      (without-restriction (ai-workbench--annotate-link-clear))))
    (ai-workbench--prettify-preset)
    (if ai-workbench-use-header-line
        (setq header-line-format ai-workbench--old-header-line
              ai-workbench--old-header-line nil)
      (setq mode-line-process nil))))

;; ;TODO(request-lib): Declaration no longer needed
(defvar ai-workbench--fsm-last)                ;Defined further below
(defun ai-workbench--update-status (msg &optional face)
  "Update status MSG with FACE."
  (when ai-workbench-mode
    (let* ((inspect (lambda (&rest _) (ai-workbench--inspect-fsm)))
           (button (propertize (buttonize msg inspect)
                              'mouse-face 'highlight)))
      (when face (setq button (propertize button 'face face)))
      (if ai-workbench-use-header-line
          (and (consp header-line-format) (setf (nth 1 header-line-format) button))
        (if (equal msg " Ready")
            (setq mode-line-process
                  `(:eval (concat " " (buttonize (ai-workbench--model-name ai-workbench-model)
                                                 ,inspect))))
          (setq mode-line-process button)
          (message msg))))
    (force-mode-line-update)))


;;;; ai-workbench-highlight-mode

(defcustom ai-workbench-highlight-methods '(margin)
  "Types of LLM response highlighting used by `ai-workbench-highlight-mode'.

This must be a list of symbols denoting types of highlighting for LLM responses:
- face: highlight LLM responses using face `ai-workbench-response-highlight'.
- fringe: highlight using a (left) fringe marker.
- margin: highlight in the (left) display margin.

margin and fringe markings are mutually exclusive, and use the
`ai-workbench-response-fringe-highlight' face."
  :type '(set (const :tag "Fringe marker" fringe)
              (const :tag "Face highlighting" face)
              (const :tag "Margin indicator" margin))
  :group 'ai-workbench-engine)

(defface ai-workbench-response-highlight
  '((((background light) (min-colors 88)) :background "linen" :extend t)
    (((background dark)  (min-colors 88)) :background "gray14" :extend t)
    (t :inherit mode-line))
  "Face used to highlight LLM responses when using `ai-workbench-highlight-mode'.

To enable this face for responses, `ai-workbench-highlight-methods' must be set."
  :group 'ai-workbench-engine)

(defface ai-workbench-response-fringe-highlight
  ;; NOTE: Remove conditional after we drop Emacs 28.1 (#1254)
  (if (< emacs-major-version 29)
      '((t :inherit outline-1 :height 1.0))
    '((t :inherit outline-1 :height reset)))
  "LLM response fringe/margin face when using `ai-workbench-highlight-mode'.

To enable response highlights in the fringe, `ai-workbench-highlight-methods'
must be set."
  :group 'ai-workbench-engine)

(define-fringe-bitmap 'ai-workbench-highlight-fringe
  (make-vector 28 #b01100000)
  nil nil 'center)

;; Common options for margin indicator:
;; BOX DRAWINGS LIGHT VERTICAL  0x002502
;; LEFT ONE QUARTER BLOCK       0x00258E
;; LEFT THREE EIGHTHS BLOCK     0x00258D
;; BOX DRAWINGS HEAVY VERTICAL  0x002503
;; VERTICAL ONE EIGHTH BLOCK-2  0x01FB70

(defun ai-workbench-highlight--margin-prefix (type)
  "Create margin prefix string for TYPE.

Supported TYPEs are response, ignore and tool calls."
  (propertize ">" 'display
              `( (margin left-margin)
                 ,(propertize "▎" 'face
                              (pcase type
                                ('response 'ai-workbench-response-fringe-highlight)
                                ('ignore 'shadow)
                                (`(tool . ,_) 'shadow))))))

(defun ai-workbench-highlight--fringe-prefix (type)
  "Create fringe prefix string for TYPE.

Supported TYPEs are response, ignore and tool calls."
  (propertize ">" 'display
              `( left-fringe ai-workbench-highlight-fringe
                 ,(pcase type
                    ('response 'ai-workbench-response-fringe-highlight)
                    ('ignore 'shadow)
                    (`(tool . ,_) 'shadow)))))

(defun ai-workbench-highlight--decorate (ov &optional val)
  "Decorate ai-workbench-engine indicator overlay OV whose type is VAL."
  (overlay-put ov 'evaporate t)
  (overlay-put ov 'ai-workbench-highlight t)
  (when (memq 'face ai-workbench-highlight-methods)
    (overlay-put ov 'font-lock-face
                 (pcase val
                   ('response 'ai-workbench-response-highlight)
                   ('ignore 'shadow)
                   (`(tool . ,_) 'shadow))))
  (when-let* ((prefix
               (cond ((memq 'margin ai-workbench-highlight-methods)
                      (ai-workbench-highlight--margin-prefix (or val 'response)))
                     ((memq 'fringe ai-workbench-highlight-methods)
                      (ai-workbench-highlight--fringe-prefix (or val 'response))))))
    (overlay-put ov 'line-prefix prefix)
    (overlay-put ov 'wrap-prefix prefix)))

(defun ai-workbench-highlight--update (beg end)
  "JIT-lock function: mark ai-workbench-engine response/reasoning regions.

BEG and END delimit the region to refresh."
  (save-excursion                ;Scan across region for the ai-workbench-engine text property
    (let ((prev-pt (goto-char end)))
      (while (and (goto-char (previous-single-property-change
                              (point) 'ai-workbench-engine nil beg))
                  (/= (point) prev-pt))
        (pcase (get-char-property (point) 'ai-workbench-engine)
          ((and (or 'response 'ignore `(tool . ,_)) val)
           (if-let* ((ov (or (cdr-safe (get-char-property-and-overlay
                                        (point) 'ai-workbench-highlight))
                             (cdr-safe (get-char-property-and-overlay
                                        prev-pt 'ai-workbench-highlight))))
                     (from (overlay-start ov)) (to (overlay-end ov)))
               (unless (<= from (point) prev-pt to)
                 (move-overlay ov (min from (point)) (max to prev-pt)))
             (ai-workbench-highlight--decorate ;Or make new overlay covering just region
              (make-overlay (point) prev-pt nil t) val)))
          ('nil                     ;If there's an overlay, we need to split it.
           (when-let* ((ov (cdr-safe (get-char-property-and-overlay
                                      (point) 'ai-workbench-highlight)))
                       (from (overlay-start ov)) (to (overlay-end ov)))
             (move-overlay ov from (point)) ;Move overlay to left side
             (ai-workbench-highlight--decorate     ;Make a new one on the right
              (make-overlay prev-pt to nil t)
              (get-char-property prev-pt 'ai-workbench-engine)))))
        (setq prev-pt (point)))))
  `(jit-lock-bounds ,beg . ,end))

(define-minor-mode ai-workbench-highlight-mode
  "Visually highlight LLM respones regions.

Highlighting is via fringe or margin markers, and optionally a response
face.  See `ai-workbench-highlight-methods' for highlighting methods, and
`ai-workbench-response-highlight' and `ai-workbench-response-fringe-highlight' for the
faces.

This minor mode can be used anywhere in Emacs, and not just ai-workbench-engine chat
buffers."
  :lighter nil
  :global nil
  (cond
   (ai-workbench-highlight-mode
    (when (memq 'margin ai-workbench-highlight-methods)
      (setq left-margin-width (1+ left-margin-width))
      (if-let* ((win (get-buffer-window (current-buffer))))
          (set-window-buffer win (current-buffer))))
    (jit-lock-register #'ai-workbench-highlight--update)
    (ai-workbench-highlight--update (point-min) (point-max)))
   (t (when (memq 'margin ai-workbench-highlight-methods)
        (setq left-margin-width (max (1- left-margin-width) 0))
        (if-let* ((win (get-buffer-window (current-buffer))))
            (set-window-buffer win (current-buffer))))
      (jit-lock-unregister #'ai-workbench-highlight--update)
      (without-restriction
        (remove-overlays nil nil 'ai-workbench-highlight t)))))


;;; State machine additions for `ai-workbench-send'.

(defvar ai-workbench-send--transitions
  `((INIT . ((t                       . WAIT)))
    (WAIT . ((t                       . TYPE)))
    (TYPE . ((,#'ai-workbench--error-p       . ERRS)
             (,#'ai-workbench--tool-use-p    . TPRE)
             (t                       . DONE)))
    (TPRE . ((,#'ai-workbench--error-p       . ERRS)
             (t                       . TOOL)))
    (TOOL . ((t                       . TRET)))
    (TRET . ((,#'ai-workbench--error-p       . ERRS)
             (,#'ai-workbench--tool-result-p . WAIT)
             (t                       . DONE))))
  "Alist specifying state transitions for `ai-workbench-send'.

See `ai-workbench-request--transitions' for details.")

(defvar ai-workbench-send--handlers
  `((WAIT ,#'ai-workbench--handle-wait ,#'ai-workbench--update-wait)
    (TYPE ,#'ai-workbench--handle-pre-insert)
    (ERRS ,#'ai-workbench--handle-error ,#'ai-workbench--fsm-last)
    (TPRE ,#'ai-workbench--handle-token-usage ,#'ai-workbench--handle-pre-tool
          ,#'ai-workbench--fsm-transition)
    (TOOL ,#'ai-workbench--update-tool-call ,#'ai-workbench--handle-tool-use
          ,#'ai-workbench--update-tool-ask)
    (TRET ,#'ai-workbench--handle-post-tool ,#'ai-workbench--handle-tool-result)
    (DONE ,#'ai-workbench--handle-post-insert ,#'ai-workbench--fsm-last)
    (ABRT ,#'ai-workbench--handle-abort))
  "Alist specifying handlers for `ai-workbench-send' state transitions.

See `ai-workbench-request--handlers' for details.")

(defvar-local ai-workbench--fsm-last nil
  "State machine for latest request in the buffer.")

(defun ai-workbench--fsm-last (fsm)
    "Capture the latest request state FSM for introspection."
    (let ((info (ai-workbench-fsm-info fsm)))
      (unless ai-workbench-log-level
        (let ((data (plist-get info :data)))
          (dolist (key '(:messages :contents :query))
            (setf (plist-get data key) nil))))
      (setf (ai-workbench-fsm-info fsm)
            (plist-put info :end-time (current-time-string)))
      (with-current-buffer (plist-get info :buffer)
        (setq ai-workbench--fsm-last fsm))))

(defun ai-workbench--inspect-fsm (&optional fsm)
  "Inspect ai-workbench-engine request state FSM.

FSM defaults to the state of the last request in the current
buffer."
  (unless fsm
    (setq fsm (or ai-workbench--fsm-last
                  (cadr (cl-find-if
                         (lambda (proc-list)
                           (eq (thread-first (cadr proc-list)
                                             (ai-workbench-fsm-info)
                                             (plist-get :buffer))
                               (current-buffer)))
                         ai-workbench--request-alist)))))
  (unless (cl-typep fsm 'ai-workbench-fsm)
    (user-error "No ai-workbench-engine request log in this buffer yet!"))
  (require 'tabulated-list)
  (with-current-buffer (get-buffer-create "*ai-workbench-diagnostic*")
    (setq tabulated-list-format [("Request attribute" 30 t) ("Value" 30)])
    (let* ((pb (lambda (s) (propertize s 'face 'font-lock-builtin-face)))
           (ps (lambda (s) (propertize s 'face 'font-lock-string-face)))
           (fmt (lambda (s) (cond ((memq (car-safe s) '(closure lambda))
                              (format "#<lambda %#x>" (sxhash s)))
                             ((byte-code-function-p s)
                              (format "#<compiled %#x>" (sxhash s)))
                             ((stringp s) (string-replace "\n" "⮐ " s))
                             (t (prin1-to-string s)))))
           (inhibit-read-only t)
           (info (ai-workbench-fsm-info fsm))
           (entries-info
            (cl-loop
             for idx upfrom 3
             for (key val) on info by #'cddr
             unless (memq key '(:data :history :tools
                                :partial_text :partial_json))
             collect
             (list idx `[,(funcall pb (symbol-name key))
                         ,(funcall ps (funcall fmt val))])))
           (entries-data
            (cl-loop
             for idx upfrom 50
             for (key val) on (plist-get info :data) by #'cddr
             unless (memq key '(:messages :stream :contents :query))
             collect
             (list idx `[,(funcall pb (symbol-name key))
                         ,(funcall ps (funcall fmt val))]))))
      (setq tabulated-list-entries
            (nconc (list `(2 [,(funcall pb ":state")
                              ,(funcall ps
                                (mapconcat
                                 fmt (reverse (cons (ai-workbench-fsm-state fsm)
                                               (plist-get info :history)))
                                 " → "))]))
                   entries-info
                   entries-data))
      (tabulated-list-print)
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (hl-line-mode 1)
      (display-buffer
       (current-buffer)
       '((display-buffer-in-side-window)
         (side . bottom)
         (window-height . fit-window-to-buffer)
         (slot . 10)
         (body-function . select-window))))))

;;;; State machine handlers
;; The next few functions are default state handlers for ai-workbench-send, see
;; `ai-workbench-send--handlers'.

(defun ai-workbench--handle-pre-insert (fsm)
  "Tasks before inserting the LLM response for state FSM.

Handle read-only buffers and run pre-response hooks (but only if
the request succeeded)."
  (let* ((info (ai-workbench-fsm-info fsm))
         (start-marker (plist-get info :position)))
    (when (memq (plist-get info :callback)
                '(ai-workbench--insert-response ai-workbench-curl--stream-insert-response))
      (with-current-buffer (marker-buffer start-marker)
        (when (or buffer-read-only (get-char-property start-marker 'read-only))
          (cond
           ((derived-mode-p 'vterm-mode)
            (require 'ai-workbench-integrations)
            (ai-workbench--vterm-pre-insert info))
           (t
            (message "Buffer is read only, displaying reply in buffer \"*LLM response*\"")
            (display-buffer
             (with-current-buffer (get-buffer-create "*LLM response*")
               (visual-line-mode 1)
               (goto-char (point-max))
               (move-marker start-marker (point) (current-buffer))
               (current-buffer))
             '((display-buffer-reuse-window
                display-buffer-pop-up-window)
               (reusable-frames . visible))))))))
    (with-current-buffer (marker-buffer start-marker)
      (when (plist-get info :stream)
        (ai-workbench--update-status " Typing..." 'success))
      (save-excursion
        (goto-char start-marker)
        (when (and (member (plist-get info :http-status) '("200" "100"))
                   ai-workbench-pre-response-hook)
          (run-hooks 'ai-workbench-pre-response-hook))))))

(defun ai-workbench--handle-post-insert (fsm)
  "Tasks after successfully inserting the LLM response with state FSM.

Indicate ai-workbench-engine status, pulse the inserted text and run post-response hooks.

No state transition here since that's handled by the process sentinels."
  (let* ((info (ai-workbench-fsm-info fsm))
         (start-marker (plist-get info :position))
         (tracking-marker (or (plist-get info :tracking-marker)
                              start-marker))
         ;; start-marker may have been moved if :buffer was read-only
         (ai-workbench-buffer (marker-buffer start-marker)))
    (with-current-buffer ai-workbench-buffer
      (if (not tracking-marker)         ;Empty response
          (when ai-workbench-mode (ai-workbench--update-status " Empty response" 'success))
        (set-marker-insertion-type tracking-marker nil) ;Lock tracking-marker
        (when ai-workbench-mode
          (unless (plist-get info :in-place)
            (save-excursion (goto-char tracking-marker)
                            (insert ai-workbench-response-separator
                                    (ai-workbench-prompt-prefix-string))))
          (ai-workbench--update-status  " Ready" 'success)
          (ai-workbench--update-token-usage (plist-get info :tokens)
                                     (plist-get info :tokens-full)))))
    ;; Run hook in visible window to set window-point, BUG #269
    (if-let* ((ai-workbench-window (get-buffer-window ai-workbench-buffer 'visible)))
        (with-selected-window ai-workbench-window
          (mapc (lambda (f) (funcall f info)) (plist-get info :post))
          (run-hook-with-args
           'ai-workbench-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer ai-workbench-buffer
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))
        (run-hook-with-args
         'ai-workbench-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))))

(defun ai-workbench--handle-error (fsm)
  "Check for errors in request state FSM.

Perform UI updates and run post-response hooks."
  (when-let* ((info (ai-workbench-fsm-info fsm))
              (error-data (plist-get info :error)))
    (let* ((status (plist-get info :status))
           (ai-workbench-buffer (plist-get info :buffer))
           (start-marker (plist-get info :position))
           (tracking-marker (or (plist-get info :tracking-marker)
                                start-marker))
           (backend-name
            (ai-workbench-backend-name
             (buffer-local-value 'ai-workbench-backend ai-workbench-buffer))))
      (if (stringp error-data)
          (message "%s error: (%s) %s" backend-name status (string-trim error-data))
        (when-let* ((error-type (plist-get error-data :type)))
          (setq status (concat "("  status ") "
                               (string-trim (ai-workbench--to-string error-type)))))
        (when-let* ((error-msg (plist-get error-data :message)))
          (message "%s error: (%s) %s" backend-name status
                   (string-trim (ai-workbench--to-string error-msg)))))
      (if-let* ((ai-workbench-window (get-buffer-window ai-workbench-buffer 'visible)))
          (with-selected-window ai-workbench-window
            (mapc (lambda (f) (funcall f info)) (plist-get info :post))
            (run-hook-with-args
             'ai-workbench-post-response-functions
             (marker-position start-marker) (marker-position tracking-marker)))
        (with-current-buffer ai-workbench-buffer
          (mapc (lambda (f) (funcall f info)) (plist-get info :post))
          (run-hook-with-args
           'ai-workbench-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker))))
      (with-current-buffer ai-workbench-buffer
        (when ai-workbench-mode
          (ai-workbench--update-status (format " Error: %s" status) 'error)
          (ai-workbench--update-token-usage (plist-get info :tokens)
                                     (plist-get info :tokens-full)))))))

(defun ai-workbench--handle-abort (fsm)
  "Perform UI update on `ai-workbench-abort' for FSM."
  (when-let* ((info (ai-workbench-fsm-info fsm))
              (ai-workbench-buffer (plist-get info :buffer))
              (start-marker (plist-get info :position))
              (tracking-marker (or (plist-get info :tracking-marker)
                                   start-marker)))
    (if-let* ((ai-workbench-window (get-buffer-window ai-workbench-buffer 'visible)))
        (with-selected-window ai-workbench-window
          (mapc (lambda (f) (funcall f info)) (plist-get info :post))
          (run-hook-with-args
           'ai-workbench-post-response-functions
           (marker-position start-marker) (marker-position tracking-marker)))
      (with-current-buffer ai-workbench-buffer
        (mapc (lambda (f) (funcall f info)) (plist-get info :post))
        (run-hook-with-args
         'ai-workbench-post-response-functions
         (marker-position start-marker) (marker-position tracking-marker))))
    (with-current-buffer ai-workbench-buffer
      (when ai-workbench-mode
        (ai-workbench--update-status  " Abort" 'error)
        (ai-workbench--update-token-usage (plist-get info :tokens)
                                   (plist-get info :tokens-full))))))

;; NOTE: Some other FSM handlers do this internally instead of calling this
;; dedicated function.
(defun ai-workbench--handle-token-usage (fsm)
  "Update token usage in ai-workbench-engine buffers for FSM."
  (let* ((info (ai-workbench-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'ai-workbench-mode buffer))
      (with-current-buffer buffer
        (ai-workbench--update-token-usage (plist-get info :tokens)
                                   (plist-get info :tokens-full))))))

(defun ai-workbench--handle-pre-tool (fsm)
  "Run `ai-workbench-pre-tool-call-functions' for FSM."
  (let* ((info (ai-workbench-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (when (buffer-local-value 'ai-workbench-pre-tool-call-functions buffer)
      ;; This function might run many times, so only act on the remaining tool calls.
      (let ((tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                    (plist-get info :tool-use)))
            (hook-func-args (list :buffer (buffer-name buffer)
                                  :backend (plist-get info :backend)
                                  :model (plist-get info :model))))
        (with-current-buffer buffer
          (run-hook-wrapped             ; Run pre tool call functions
           'ai-workbench-pre-tool-call-functions
           (lambda (hook-func)
             (prog1 nil
               (dolist (tool-call tool-use)
                 (let* ((name (plist-get tool-call :name))
                        (args (plist-get tool-call :args))
                        (hook-func-result
                         (with-demoted-errors "ai-workbench-pre-tool-call hook error: %S"
                           (funcall hook-func (nconc (list :name name :args args)
                                                     hook-func-args)))))
                   (if (plist-get hook-func-result :stop) ; Stop the request immediately
                       (let ((reason (or (plist-get hook-func-result :stop-reason)
                                         (concat "Request stopped by pre-tool-call hook "
                                                 (and (symbolp hook-func)
                                                      (symbol-name hook-func))
                                                 " (tool \"" name "\")"))))
                         (plist-put info :stop-reason reason)
                         (plist-put info :status "Stopped by hook")
                         (plist-put info :error reason))
                     ;; if hook-func returns :confirm, add the check
                     (when-let* ((confirm-tail
                                  (plist-member hook-func-result :confirm)))
                       (plist-put tool-call :confirm (cadr confirm-tail)))
                     ;; if hook-func returns :args or :name, replace in the call
                     (when (or (plist-get hook-func-result :args)
                               (plist-get hook-func-result :name))
                       ;; Merge with args in the messages array sent to the LLM
                       (ai-workbench--inject-tool-call
                        (plist-get info :backend) (plist-get info :data)
                        tool-call hook-func-result)
                       ;; Merge with the tool-call in tool-use that actually runs
                       (ai-workbench--merge-plists tool-call hook-func-result))
                     ;; TODO(tool-hooks): :block behavior not final!
                     (let ((blockp (plist-get hook-func-result :block))
                           (result (plist-get hook-func-result :result)))
                       (when blockp
                         (plist-put tool-call :error t)
                         (setq result
                               (concat "<tool_call_error>\n"
                                       (if (stringp blockp) blockp
                                         (format "Tool %s blocked by user" name))
                                       "\n</tool_call_error>")))
                       (when result
                         (ai-workbench--process-tool-call
                          fsm (cl-find-if (lambda (ts) (equal (ai-workbench-tool-name ts) name))
                                          (plist-get info :tools))
                          tool-call result))))))))))))))

(defun ai-workbench--handle-post-tool (fsm)
  "Run `ai-workbench-post-tool-call-functions for FSM."
  (let* ((info (ai-workbench-fsm-info fsm))
         (buffer (plist-get info :buffer)))
    (when (buffer-local-value 'ai-workbench-post-tool-call-functions buffer)
      (let ((hook-func-args (list :buffer (buffer-name buffer)
                                  :backend (plist-get info :backend)
                                  :model (plist-get info :model))))
        (with-current-buffer buffer
          (run-hook-wrapped             ; Run pre tool call functions
           'ai-workbench-post-tool-call-functions
           (lambda (hook-func)
             (prog1 nil
               (dolist (tool-call (plist-get info :tool-use))
                 (let* ((name (plist-get tool-call :name))
                        (args (plist-get tool-call :args))
                        (hook-func-result
                         (with-demoted-errors "ai-workbench-post-tool-call hook error: %S"
                           (funcall hook-func
                                    (nconc (list :name name :args args
                                                 :result (plist-get tool-call :result))
                                           hook-func-args)))))
                   (if (plist-get hook-func-result :stop)
                       (let ((reason (or (plist-get hook-func-result :stop-reason)
                                         (concat "Request stopped by post-tool-call hook "
                                                 (and (symbolp hook-func)
                                                      (symbol-name hook-func))
                                                 " (tool \"" name "\")"))))
                         (plist-put info :stop-reason reason)
                         (plist-put info :status "Stopped by hook")
                         (plist-put info :error reason))
                     ;; TODO(tool-hooks): :block behavior not final!
                     (let ((blockp (plist-get hook-func-result :block))
                           (result (plist-get hook-func-result :result)))
                       (when blockp
                         (plist-put tool-call :error t)
                         (setq result
                               (concat "<tool_call_error>\n"
                                       (if (stringp blockp) blockp
                                         (format "Tool %s blocked by user" name))
                                       "\n</tool_call_error>")))
                       (when result
                         (cl-loop       ; Update results sent to callback
                          for call in (plist-get info :tool-result)
                          for (spec stored-args _) = call
                          when (and (equal (ai-workbench-tool-name spec) name)
                                    (null (cl-set-difference
                                           stored-args args :test #'equal)))
                          do (setf (caddr call) result) and return nil
                          finally
                          (display-warning
                           '(ai-workbench-engine tools)
                           (format "Tool %s: Could not replace tool results" name)))
                         ;; Update results sent to LLM
                         (plist-put tool-call :result result))))))))))))))

(defun ai-workbench--update-wait (fsm)
  "Update ai-workbench-engine's status in FSM after sending a request."
  (with-current-buffer (plist-get (ai-workbench-fsm-info fsm) :buffer)
    (when ai-workbench-mode
      (ai-workbench--update-status " Waiting..." 'warning))))

(defun ai-workbench--update-tool-call (fsm)
  "Update ai-workbench-engine's status in FSM when calling a tool."
  (with-current-buffer (plist-get (ai-workbench-fsm-info fsm) :buffer)
    (setq ai-workbench--fsm-last fsm)
    (when ai-workbench-mode
      (if-let* ((info (ai-workbench-fsm-info fsm))
                (names (cl-loop for call in (plist-get info :tool-use)
                                collect (plist-get call :name))))
          (ai-workbench--update-status
           (concat
            (propertize
             (if (length> names 1) " Calling tools (" " Calling tool (")
             'face 'mode-line-emphasis)
            (mapconcat (lambda (name) (propertize name 'face 'font-lock-keyword-face))
                       names (propertize ", " 'face 'mode-line-emphasis))
            (propertize ")" 'face 'mode-line-emphasis)))
        ;; FIXME: Is this branch reachable?
        (ai-workbench--update-status " Calling tool..." 'mode-line-emphasis)))))

(defun ai-workbench--update-tool-ask (fsm)
  "Update ai-workbench-engine's status in FSM when there are pending tool-calls."
  (when (plist-get (ai-workbench-fsm-info fsm) :tool-pending)
    (plist-put (ai-workbench-fsm-info fsm) :tool-pending nil)
    (let* ((info (ai-workbench-fsm-info fsm))
           (buf (plist-get info :buffer)))
      (with-current-buffer buf
        (when ai-workbench-mode
          (ai-workbench--update-status " Run tools?" 'mode-line-emphasis))))))


;;; Send queries, handle responses
;;;###autoload
(defun ai-workbench-send (&optional arg)
  "Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response."
  (interactive "P")
  (if (and arg (require 'ai-workbench-transient nil t))
      (call-interactively #'ai-workbench-menu)
    (ai-workbench--sanitize-model)
    (let ((fsm (ai-workbench-make-fsm :table ai-workbench-send--transitions
                               :handlers ai-workbench-send--handlers)))
      (ai-workbench-request nil
        :stream ai-workbench-stream
        :transforms ai-workbench-prompt-transform-functions
        :fsm fsm)
      (message "Querying %s..."
               (thread-first (ai-workbench-fsm-info fsm)
                             (plist-get :backend)
                             (or ai-workbench-backend)
                             (ai-workbench-backend-name))))
    (ai-workbench--update-status " Waiting..." 'warning)))

(declare-function json-pretty-print-buffer "json")
(defun ai-workbench--inspect-query (&optional request-fsm format)
  "Show the full LLM query that will be sent in a buffer.

This functions as a dry run of `ai-workbench-send'.  The request data
may be edited and the query continued from this buffer.

REQUEST-FSM is the state of the request, as returned by
`ai-workbench-request'.  If FORMAT is the symbol json, show the encoded
JSON query instead of the Lisp structure ai-workbench-engine uses."
  (unless request-fsm (setq request-fsm ai-workbench--fsm-last))
  (if (bufferp (plist-get (ai-workbench-fsm-info request-fsm) :data))
      (letrec ((dry-run-poll
                (run-with-timer
                 0 1 (lambda (fsm form)
                       (unless (bufferp (plist-get (ai-workbench-fsm-info fsm) :data))
                         (cancel-timer dry-run-poll)
                         (ai-workbench--inspect-query fsm form)))
                 request-fsm format))))
    (with-current-buffer (plist-get (ai-workbench-fsm-info request-fsm) :buffer)
      (ai-workbench--update-status " Ready" 'success))
    (with-current-buffer (get-buffer-create "*ai-workbench-query*")
      (let* ((standard-output (current-buffer))
             (inhibit-read-only t)
             (request-data
              (plist-get (ai-workbench-fsm-info request-fsm) :data)))
        (buffer-disable-undo)
        (erase-buffer)
        (if (eq format 'json)
            (progn (fundamental-mode)
                   (insert (ai-workbench--json-encode request-data))
                   (json-pretty-print-buffer))
          (lisp-data-mode)
          (prin1 request-data)
          (pp-buffer))
        (setq-local ai-workbench--fsm-last request-fsm)
        (goto-char (point-min))
        (view-mode 1)
        (setq buffer-undo-list nil)
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c" #'ai-workbench--continue-query
            "C-c C-w" (lambda () "Copy Curl command for query."
                        (interactive) (ai-workbench--continue-query 'copy))
            "C-c C-k" #'quit-window)
          (current-local-map)))
        (unless header-line-format
          (setq header-line-format
                (substitute-command-keys
                 (concat
                  "Edit request: \\[read-only-mode],"
                  " Send request: \\[ai-workbench--continue-query],"
                  (format " Copy Curl: %s"
                          (propertize "C-c C-w" 'face 'help-key-binding))
                  " Quit: \\[quit-window]"))))
        (display-buffer (current-buffer) ai-workbench-display-buffer-action)))))

(defun ai-workbench--continue-query (&optional copy)
  "Continue sending the ai-workbench-engine query displayed in this buffer.

The request is continued with the same parameters as originally
specified.

With prefix arg COPY, copy the Curl command for the request to the
kill ring instead."
  (interactive "P" lisp-data-mode fundamental-mode)
  (unless (equal (buffer-name) "*ai-workbench-query*")
    (user-error "This command is meant for use in a ai-workbench-engine dry-run buffer"))
  (save-excursion
    (goto-char (point-min))
    (condition-case-unless-debug nil
        (when-let* ((data (if (eq major-mode 'lisp-data-mode)
                              (read (current-buffer))
                            (ai-workbench--json-read))))
          (cl-assert (cl-typep ai-workbench--fsm-last 'ai-workbench-fsm))
          (plist-put (ai-workbench-fsm-info ai-workbench--fsm-last) :data data)
          (if copy                 ;Copy Curl command instead of sending request
              (let ((args (ai-workbench-curl--get-args (ai-workbench-fsm-info ai-workbench--fsm-last)
                                                (md5 (format "%s" (random))) t)))
                (kill-new
                 (mapconcat #'shell-quote-argument
                            (cons (ai-workbench--curl-path) args) " \\\n"))
                (message "Curl command for request copied to kill-ring"))
            (ai-workbench--fsm-transition ai-workbench--fsm-last) ;INIT -> WAIT
            (quit-window)))
      (error
       (user-error "Can not resume request: could not read data from buffer!")))))

(defun ai-workbench--insert-response (response info &optional raw)
  "Insert the LLM RESPONSE into the ai-workbench-engine buffer.

INFO is a plist containing information relevant to this buffer.
See `ai-workbench--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (let* ((ai-workbench-buffer (plist-get info :buffer))
         (start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    (pcase response
      ((pred stringp)                ;Response text
       (with-current-buffer ai-workbench-buffer
         (when tracking-marker           ;separate from previous response
           (setq response (concat ai-workbench-response-separator response)))
         (save-excursion
           (with-current-buffer (marker-buffer start-marker)
             (goto-char (or tracking-marker start-marker))
             ;; (run-hooks 'ai-workbench-pre-response-hook)
             (unless (or (bobp) (plist-get info :in-place)
                         tracking-marker)
               (insert ai-workbench-response-separator)
               (when ai-workbench-mode
                 (insert (ai-workbench-response-prefix-string)))
               (move-marker start-marker (point)))
             (unless raw
               (when-let* ((transformer (plist-get info :transformer)))
                 (setq response (funcall transformer response)))
               (add-text-properties
                0 (length response) '(ai-workbench-engine response front-sticky (ai-workbench-engine)) response))
             (insert response)
             (plist-put info :tracking-marker (setq tracking-marker (point-marker)))
             ;; for uniformity with streaming responses
             (set-marker-insertion-type tracking-marker t)))))
      (`(reasoning . ,text)
       (when-let* ((include (plist-get info :include-reasoning)))
         (if (stringp include)
             (with-current-buffer (get-buffer-create
                                   (plist-get info :include-reasoning))
               (save-excursion (goto-char (point-max)) (insert text)))
           (with-current-buffer (marker-buffer start-marker)
             (let ((separator         ;Separate from response prefix if required
                    (and (not tracking-marker) ai-workbench-mode
                         (not (string-suffix-p "\n" (ai-workbench-response-prefix-string)))
                         "\n"))
                   (blocks (if (derived-mode-p 'org-mode)
                               `("#+begin_reasoning\n" . ,(concat "\n#+end_reasoning"
                                                           ai-workbench-response-separator))
                             ;; TODO(reasoning) remove properties and strip instead
                             (cons (propertize "``` reasoning\n" 'ai-workbench-engine 'ignore
                                               'keymap ai-workbench--markdown-block-map)
                                   (concat (propertize "\n```" 'ai-workbench-engine 'ignore
                                                       'keymap ai-workbench--markdown-block-map)
                                           ai-workbench-response-separator)))))
               (if (eq include 'ignore)
                   (progn
                     (add-text-properties
                      0 (length text) '(ai-workbench-engine ignore front-sticky (ai-workbench-engine)) text)
                     (ai-workbench--insert-response
                      (concat (car blocks) text (cdr blocks)) info t))
                 (ai-workbench--insert-response (concat separator (car blocks)) info t)
                 (ai-workbench--insert-response text info)
                 (ai-workbench--insert-response (cdr blocks) info t))
               (save-excursion
                 (goto-char (plist-get info :tracking-marker))
                 (if (derived-mode-p 'org-mode) ;fold block
                     (progn (search-backward "#+end_reasoning" start-marker t)
                            (when (looking-at "^#\\+end_reasoning")
                              (org-cycle)))
                   (when (re-search-backward "^```" start-marker t)
                     (ai-workbench-markdown-cycle-block)))))))))
      (`(tool-call . ,tool-calls)
       (ai-workbench--display-tool-calls tool-calls info))
      (`(tool-result . ,tool-results)
       (ai-workbench--display-tool-results tool-results info)))))

(defun ai-workbench-curl--stream-insert-response (response info &optional raw)
  "Insert streaming RESPONSE from an LLM into the ai-workbench-engine buffer.

INFO is a mutable plist containing information relevant to this buffer.
See `ai-workbench--url-get-response' for details.

Optional RAW disables text properties and transformation."
  (pcase response
    ((pred stringp)
     (let ((start-marker (plist-get info :position))
           (tracking-marker (plist-get info :tracking-marker))
           (transformer (plist-get info :transformer)))
       (with-current-buffer (marker-buffer start-marker)
         (save-excursion
           (unless tracking-marker
             (goto-char start-marker)
             (unless (or (bobp) (plist-get info :in-place))
               (insert ai-workbench-response-separator)
               (when ai-workbench-mode
                 ;; Put prefix before AI response.
                 (insert (ai-workbench-response-prefix-string)))
               (move-marker start-marker (point)))
             (setq tracking-marker (set-marker (make-marker) (point)))
             (set-marker-insertion-type tracking-marker t)
             (plist-put info :tracking-marker tracking-marker))
           (goto-char tracking-marker)
           (unless raw
             (when transformer
               (setq response (funcall transformer response)))
             (add-text-properties
              0 (length response) '(ai-workbench-engine response front-sticky (ai-workbench-engine))
              response))
           ;; (run-hooks 'ai-workbench-pre-stream-hook)
           (insert response)
           (run-hooks 'ai-workbench-post-stream-hook)))))
    (`(reasoning . ,text)
     (ai-workbench--display-reasoning-stream text info))
    (`(tool-call . ,tool-calls)
     (ai-workbench--display-tool-calls tool-calls info))
    (`(tool-result . ,tool-results)
     (ai-workbench--display-tool-results tool-results info)
     ;; Adjust for tool calls inside reasoning blocks
     (when (eq (plist-get info :reasoning-block) 'in)
       (when-let* ((rm (plist-get info :reasoning-marker))
                   (tm (plist-get info :tracking-marker)))
         (move-marker rm tm (marker-buffer tm)))))))

;;;###autoload
(defun ai-workbench-engine (name &optional _ initial interactivep)
  "Switch to or start a chat session with NAME.

Ask for API-KEY if `ai-workbench-api-key' is unset.

If region is active, use it as the INITIAL prompt.  Returns the
buffer created or switched to.

INTERACTIVEP is t when ai-workbench-engine is called interactively."
  (interactive
   (progn
     (ai-workbench--sanitize-model :backend (default-value 'ai-workbench-backend)
                            :shoosh t)
     (let* ((backend (default-value 'ai-workbench-backend))
            (backend-name
             (format "*%s*" (ai-workbench-backend-name backend))))
       (list (read-buffer
              "Create or choose ai-workbench-engine buffer: "
              backend-name nil          ; DEFAULT and REQUIRE-MATCH
              (lambda (b)                    ; PREDICATE
                ;; NOTE: buffer check is required (#450)
                (and-let* ((buf (get-buffer (or (car-safe b) b))))
                  (buffer-local-value 'ai-workbench-mode buf))))
             (condition-case nil
                 (ai-workbench--get-api-key
                  (ai-workbench-backend-key backend))
               ((error user-error)
                (setq ai-workbench-api-key
                      (read-passwd
                       (format "%s API key: " backend-name)))))
             (and (use-region-p)
                  (buffer-substring (region-beginning)
                                    (region-end)))
             t))))
  (with-current-buffer (get-buffer-create name)
    (cond                               ;Set major mode
     ((eq major-mode ai-workbench-default-mode))
     ((eq ai-workbench-default-mode 'text-mode)
      (text-mode)
      (visual-line-mode 1))
     (t (funcall ai-workbench-default-mode)))
    (ai-workbench--sanitize-model :backend (default-value 'ai-workbench-backend)
                           :model (default-value 'ai-workbench-model)
                           :shoosh nil)
    (unless ai-workbench-mode (ai-workbench-mode 1))
    (goto-char (point-max))
    (skip-chars-backward "\t\r\n")
    (if (bobp) (insert (or initial (ai-workbench-prompt-prefix-string))))
    (when interactivep
      (display-buffer (current-buffer) ai-workbench-display-buffer-action)
      (message "Send your query with %s!"
               (substitute-command-keys "\\[ai-workbench-send]")))
    (current-buffer)))


;;; Reasoning content UI
(defun ai-workbench--display-reasoning-stream (text info)
  "Show reasoning TEXT in an appropriate location.

INFO is the request INFO, see `ai-workbench--url-get-response'.  This is
for streaming responses only."
  (when-let* ((include (plist-get info :include-reasoning)))
    (if (stringp include)
        (unless (eq text t)
          (with-current-buffer (get-buffer-create include)
            (save-excursion (goto-char (point-max))
                            (insert text))))
      (let* ((reasoning-marker (plist-get info :reasoning-marker))
             (tracking-marker (plist-get info :tracking-marker))
             (start-marker (plist-get info :position)))
        (with-current-buffer (marker-buffer start-marker)
          (if (eq text t)               ;end of stream
              (progn
                (ai-workbench-curl--stream-insert-response
                 (concat (if (derived-mode-p 'org-mode)
                             "\n#+end_reasoning"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "\n```" 'ai-workbench-engine 'ignore
                                       'keymap ai-workbench--markdown-block-map))
                         ai-workbench-response-separator)
                 info t)
                (ignore-errors          ;fold block
                  (save-excursion
                    (goto-char tracking-marker)
                    (if (derived-mode-p 'org-mode)
                        (progn (search-backward "#+end_reasoning" start-marker t)
                               (when (looking-at "^#\\+end_reasoning")
                                 (org-cycle)))
                      (when (re-search-backward "^```" start-marker t)
                        (ai-workbench-markdown-cycle-block))))))
            (unless (and reasoning-marker tracking-marker
                         (= reasoning-marker tracking-marker))
              (let ((separator        ;Separate from response prefix if required
                     (and (not tracking-marker) ai-workbench-mode
                          (not (string-suffix-p
                                "\n" (ai-workbench-response-prefix-string)))
                          "\n")))
                (ai-workbench-curl--stream-insert-response
                 (concat separator
                         (if (derived-mode-p 'org-mode)
                             "#+begin_reasoning\n"
                           ;; TODO(reasoning) remove properties and strip instead
                           (propertize "``` reasoning\n" 'ai-workbench-engine 'ignore
                                       'keymap ai-workbench--markdown-block-map)))
                 info t)))
            (if (eq include 'ignore)
                (progn
                  (add-text-properties
                   0 (length text) '(ai-workbench-engine ignore front-sticky (ai-workbench-engine)) text)
                  (ai-workbench-curl--stream-insert-response text info t))
              (ai-workbench-curl--stream-insert-response text info)))
          (setq tracking-marker (plist-get info :tracking-marker))
          (if reasoning-marker
              (move-marker reasoning-marker tracking-marker)
            (plist-put info :reasoning-marker
                       (copy-marker tracking-marker nil))))))))

(defvar ai-workbench--tool-preview-alist nil
  "Alist mapping tool names to preview functions for tools.

Each key is a tool name (string) and value is a list of one or two
functions, for preview-setup and (optional) preview-teardown.

The preview-setup function is called with two arguments: a plist of the
corresponding tool call arguments and the request INFO plist.  It must
set up the preview for the tool call and return a handle to the preview,
which can be any object, but typically an overlay or a buffer.

The preview-setup can integrate with ai-workbench-engine's default previewer by
inserting at point (and moving point), or use a different preview method
entirely.

The preview-teardown function, if provided, is called with this handle
when the tool call is accepted or rejected, and it must clear the
preview.

Note: This tool call preview API is currently experimental.")


;;; Tool use UI
(defvar-keymap ai-workbench-tool-call-actions-map
  :doc "Keymap for actions on tool calls."
  "<mouse-1>" #'ai-workbench--dispatch-tool-calls
  "C-c C-c" #'ai-workbench--accept-tool-calls
  "C-c C-k" #'ai-workbench--reject-tool-calls
  "C-c C-i" #'ai-workbench--inspect-tool-calls)

(defun ai-workbench--display-tool-calls (tool-calls info &optional use-minibuffer)
  "Handle tool call confirmation.

TOOL-CALLS should be a list of tool call specifications or results,
structured as:

 ((tool args callback) ...)

for tool call specifications to be confirmed.  INFO contains the
state of the request.  To prompt for tool call confirmation, use
either an overlay in the request buffer or the minibuffer (if
USE-MINIBUFFER is non-nil)."
  (let* ((start-marker (plist-get info :position))
         (tracking-marker (plist-get info :tracking-marker)))
    ;; pending tool calls look like ((tool callback args) ...)
    (with-current-buffer (marker-buffer start-marker)
      (if (or use-minibuffer   ;prompt for confirmation from the minibuffer
              buffer-read-only ;TEMP(tool-preview) Handle read-only buffers better
              (get-char-property
               (max (point-min) (1- (or tracking-marker start-marker)))
               'read-only))
          (let* ((minibuffer-allow-text-properties t)
                 (backend-name (ai-workbench-backend-name (plist-get info :backend)))
                 (tool-call-names
                  (mapconcat (lambda (c) (propertize (ai-workbench-tool-name (car c))
                                                'face 'font-lock-keyword-face))
                             tool-calls ", "))
                 (len (length tool-calls))
                 (prompt (format "%s wants to run %s tool %s (%s). "
                                 backend-name len (if (> len 1) "calls" "call")
                                 tool-call-names))
                 (choices '((?y "Run tools") (?n "Cancel (resumable)")
                            (?i "Inspect or edit")))
                 (choice (read-multiple-choice prompt choices)))
            (pcase (car choice)
              (?y (ai-workbench--accept-tool-calls tool-calls))
              (?n (ai-workbench--reject-tool-calls))
              (?i (ai-workbench--inspect-tool-calls tool-calls info))))
        ;; Prompt for confirmation from the response buffer
        (let* ((backend-name (ai-workbench-backend-name (plist-get info :backend)))
               (actions-string
                (concat (propertize "Run tools: " 'face 'font-lock-string-face)
                        (propertize "C-c C-c" 'face 'help-key-binding)
                        (propertize ", Cancel request: " 'face 'font-lock-string-face)
                        (propertize "C-c C-k" 'face 'help-key-binding)
                        (propertize ", Inspect or Edit: " 'face 'font-lock-string-face)
                        (propertize "C-c C-i" 'face 'help-key-binding)))
               (confirm-strings)
               ;; FIXME(tool) use a wrapper instead of a manual text-property search,
               ;; this is fragile
               (ov-start (save-excursion
                           (goto-char start-marker)
                           (text-property-search-backward 'ai-workbench-engine 'response)
                           (point)))
               (preview-handlers)
               (ov (or (cdr-safe (get-char-property-and-overlay
                                  start-marker 'ai-workbench-tool))
                       (make-overlay ov-start (or tracking-marker start-marker)
                                     nil nil nil)))
               (arg-values)
               (prompt-ov))
          ;; If the cursor is at the overlay-end, it ends up outside, so move it back
          (unless tracking-marker
            (when (= (point) start-marker) (ignore-errors (backward-char))))
          (save-excursion
            (goto-char (overlay-end ov))
            (pcase-dolist (`(,tool-spec ,arg-plist _) tool-calls)
              ;; Call tool-specific confirmation prompt
              (setq arg-values (ai-workbench--map-tool-args tool-spec arg-plist))
              (if-let* ((funcs (cdr (assoc (ai-workbench-tool-name tool-spec)
                                           ai-workbench--tool-preview-alist)))
                        ((functionp (car-safe funcs))))
                  ;;preview-teardown func   preview-handle overlay/buffer
                  (push (list (cadr funcs) (funcall (car funcs) arg-values info))
                        preview-handlers)
                (push (ai-workbench--format-tool-call (ai-workbench-tool-name tool-spec) arg-values)
                      confirm-strings)))
            (and confirm-strings (apply #'insert (nreverse confirm-strings)))
            (add-text-properties (overlay-end ov) (1- (point))
                                 '(read-only t font-lock-fontified t))
            (setq prompt-ov (make-overlay (overlay-end ov) (point) nil t))
            (overlay-put
             prompt-ov 'before-string
             (concat
              "\n"
              (propertize " " 'display `(space :align-to (- right ,(length actions-string) 2))
                          'face '(:inherit font-lock-string-face :underline t :extend t))
              actions-string
              (format (propertize "\n%s wants to run:\n\n"
                                  'face 'font-lock-string-face)
                      backend-name)))
            (overlay-put
             prompt-ov 'after-string
             (concat (propertize "\n" 'face
                                 '(:inherit font-lock-string-face :underline t :extend t))))
            (overlay-put prompt-ov 'evaporate t)
            (overlay-put ov 'prompt (cons prompt-ov (overlay-get ov 'prompt)))
            (move-overlay ov ov-start (point)))
          ;; Add confirmation prompt to the overlay
          (when preview-handlers
            (overlay-put ov 'previews
                         (nconc (overlay-get ov 'previews) preview-handlers)))
          ;; Including INFO is required for tool call inspection (state
          ;; management and updates)
          (overlay-put ov 'info info)
          (overlay-put ov 'mouse-face 'highlight)
          (overlay-put ov 'ai-workbench-tool
                       (nconc (overlay-get ov 'ai-workbench-tool) tool-calls))
          (overlay-put ov 'help-echo
                       (concat "Tool call(s) requested: " actions-string))
          (overlay-put ov 'keymap ai-workbench-tool-call-actions-map)
          prompt-ov)))))

(defun ai-workbench--display-tool-results (tool-results info)
  "Insert TOOL-RESULTS into buffer.

TOOL-RESULTS is

 ((tool args result) ...)

for tool call results.  INFO contains the state of the request."
  (let* ((start-marker (plist-get info :position))
         (tool-marker (plist-get info :tool-marker))
         (tracking-marker (plist-get info :tracking-marker)))
    ;; Insert tool results
    (with-current-buffer (marker-buffer start-marker)
      (when ai-workbench-include-tool-results
        (cl-loop
         for (tool args result) in tool-results
         with include-names =
         (mapcar #'ai-workbench-tool-name
                 (cl-remove-if-not #'ai-workbench-tool-include (plist-get info :tools)))
         if (or (eq ai-workbench-include-tool-results t)
                (member (ai-workbench-tool-name tool) include-names))
         do (funcall
             (plist-get info :callback)
             (let* ((name (ai-workbench-tool-name tool))
                    (separator        ;Separate from response prefix if required
                     (cond ((not tracking-marker)
                            (and ai-workbench-mode
                                 (not (string-suffix-p
                                       "\n" (ai-workbench-response-prefix-string)))
                                 "\n")) ;start of response
                           ((not tool-marker) ai-workbench-response-separator)
                           ((and (not (= tracking-marker tool-marker))
                                 (not (eq (char-before tracking-marker) ?\n)))
                            ai-workbench-response-separator)))
                    (tool-use
                     ;; TODO(tool) also check args since there may be more than
                     ;; one call/result for the same tool
                     (cl-find-if
                      (lambda (tu) (equal (plist-get tu :name) name))
                      (plist-get info :tool-use)))
                    (id (plist-get tool-use :id))
                    (display-call (format "(%s %s)" name
                                          (string-trim (prin1-to-string args) "(" ")")))
                    (call (prin1-to-string `(:name ,name :args ,args)))
                    (truncated-call
                     (string-replace "\n" " "
                                     (truncate-string-to-width
                                      display-call
                                      (floor (* (window-width) 0.6)) 0 nil " ...)"))))
               (if (derived-mode-p 'org-mode)
                   (concat
                    separator
                    "#+begin_tool "
                    truncated-call
                    (propertize
                     (org-escape-code-in-string (concat "\n" call "\n\n" result))
                     'ai-workbench-engine `(tool . ,id))
                    "\n#+end_tool\n")
                 ;; TODO(tool) else branch is handling all front-ends as markdown.
                 ;; At least escape markdown.
                 (concat
                  separator
                  ;; TODO(tool) remove properties and strip instead of ignoring
                  (propertize (format "``` tool %s" truncated-call)
                              'ai-workbench-engine 'ignore 'keymap ai-workbench--markdown-block-map)
                  (propertize
                   ;; TODO(tool) escape markdown in result
                   (concat "\n" call "\n\n" result)
                   'ai-workbench-engine `(tool . ,id))
                  ;; TODO(tool) remove properties and strip instead of ignoring
                  (propertize "\n```\n" 'ai-workbench-engine 'ignore
                              'keymap ai-workbench--markdown-block-map))))
             info
             'raw)
         ;; tool-result insertion has updated the tracking marker
         (unless tracking-marker
           (setq tracking-marker (plist-get info :tracking-marker)))
         (if tool-marker
             (move-marker tool-marker tracking-marker)
           (setq tool-marker (copy-marker tracking-marker nil))
           (plist-put info :tool-marker tool-marker))
         (ignore-errors                 ;fold drawer
           (save-excursion
             (goto-char tracking-marker)
             (forward-line -1)
             (if (derived-mode-p 'org-mode)
                 (when (looking-at-p "^#\\+end_tool") (org-cycle))
               (when (looking-at-p "^```") (ai-workbench-markdown-cycle-block))))))))))

(defun ai-workbench--format-tool-call (name arg-values)
  "Format a tool call for display in the buffer.

NAME and ARG-VALUES are the name and arguments for the call."
  (format "(%s %s)\n"
          (propertize name 'font-lock-face 'font-lock-keyword-face)
          (propertize
           (mapconcat (lambda (arg)
                        (cond ((stringp arg)
                               (prin1-to-string
                                (replace-regexp-in-string
                                 "\n" "⮐" (truncate-string-to-width
                                           arg 256
                                           nil nil t))))
                              (t (prin1-to-string arg))))
                      arg-values " ")
           'font-lock-face 'font-lock-constant-face)))

(defun ai-workbench--accept-tool-calls (&optional tool-calls ov)
  "Run pending tool-calls.

TOOL-CALLS is the edited tool call list, OV is the tool call dispatch
overlay in the query buffer."
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'ai-workbench-tool)))
                 (list resp o)))
  (when (overlayp ov)                   ;Update UI indicator
    (with-current-buffer (overlay-buffer ov)
      (when ai-workbench-mode
        (let ((names (cl-loop for call in tool-calls
                              collect (ai-workbench-tool-name (car call)))))
          (ai-workbench--update-status
           (concat
            (propertize
             (if (length> names 1) " Calling tools (" " Calling tool (")
             'face 'mode-line-emphasis)
            (mapconcat (lambda (name) (propertize name 'face 'font-lock-keyword-face))
                       names (propertize ", " 'face 'mode-line-emphasis))
            (propertize ")" 'face 'mode-line-emphasis)))))))
  ;; Clear the overlays first, because we need the buffer to be cleaned up
  ;; before inserting synchronous tool results.
  (when (and (overlayp ov) (overlay-buffer ov))
    (with-current-buffer (overlay-buffer ov)
      (when-let* ((preview-handles (overlay-get ov 'previews)))
        (dolist (func-to-handle preview-handles)
          (when (car func-to-handle) (apply func-to-handle))))
      (dolist (prompt-ov (overlay-get ov 'prompt))
        (when-let* (((overlay-buffer prompt-ov))
                    (inhibit-read-only t))
          (delete-region (overlay-start prompt-ov)
                         (overlay-end prompt-ov)))))
    (delete-overlay ov))
  (message "Continuing query...")
  (cl-loop for (tool-spec arg-plist process-tool-result) in tool-calls
           for arg-values = (ai-workbench--map-tool-args tool-spec arg-plist)
           do
           (if (ai-workbench-tool-async tool-spec)
               (apply (ai-workbench-tool-function tool-spec)
                      process-tool-result arg-values)
             (let ((result
                    (condition-case errdata
                        (apply (ai-workbench-tool-function tool-spec) arg-values)
                      (error (mapconcat #'ai-workbench--to-string errdata " ")))))
               (funcall process-tool-result result)))))

(defun ai-workbench--reject-tool-calls (&optional _tool-calls ov)
  "Cancel pending tool-calls.

OV is the tool call dispatch overlay."
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'ai-workbench-tool)))
                 (list resp o)))
  (ai-workbench--update-status " Tools cancelled" 'error)
  (message (substitute-command-keys
            "Tool calls canceled.  \\[ai-workbench-menu] to continue them!"))
  (when (and (overlayp ov) (overlay-buffer ov))
    (with-current-buffer (overlay-buffer ov)
      (when-let* ((preview-handles (overlay-get ov 'previews)))
        (dolist (func-to-handle preview-handles)
          (when (car func-to-handle) (apply func-to-handle))))
      (dolist (prompt-ov (overlay-get ov 'prompt))
        (when-let* (((overlay-buffer prompt-ov))
                    (inhibit-read-only t))
          (delete-region (overlay-start prompt-ov)
                         (overlay-end prompt-ov)))))
    (delete-overlay ov)))

(defun ai-workbench--dispatch-tool-calls (choice)
  "Dispatch on tool-calls with CHOICE."
  (interactive
   (list
    (let ((choices '((?y "yes") (?n "do nothing")
                     (?k "cancel request") (?i "inspect call(s)"))))
      (read-multiple-choice "Run tool calls? " choices))))
  (pcase (car choice)
    (?y (call-interactively #'ai-workbench--accept-tool-calls))
    (?k (call-interactively #'ai-workbench--reject-tool-calls))
    (?i (call-interactively #'ai-workbench--inspect-tool-calls))))

;;;; Tool call inspection UI
(defvar-keymap ai-workbench-tool-call-inspection-map
  :doc "Actions in the ai-workbench-engine tool inspection buffer."
  "C-c C-c" #'ai-workbench--inspect-accept-tool-calls
  "C-c C-k" #'ai-workbench--inspect-reject-tool-calls
  "C-c C-i" #'ai-workbench--inspect-quit-tool-calls)

(defun ai-workbench--inspect-accept-tool-calls (&optional _)
  "Run possibly edited tool-calls read from the tool call inspection buffer."
  (interactive)
  (let ((call) (index)
        (read-error
         (lambda () (user-error
                "Cannot read modified arguments, please check modifications")))
        (apply-error
         (lambda () (message "Cannot apply argument modifications.  \
This is a bug, please report it!"))))
    (unless (ai-workbench-fsm-p ai-workbench--fsm-last) (funcall apply-error))
    (cond
     ((buffer-modified-p)
      (let* ((info (ai-workbench-fsm-info ai-workbench--fsm-last))
             (backend (plist-get info :backend))
             (tool-use (plist-get info :tool-use))
             (tool-spec-args-cb) (name)
             (tool-calls))
        (dolist (o (cl-remove-if-not (lambda (ov) (overlay-get ov 'ai-workbench-overlay))
                                     (overlays-in (point-min) (point-max))))
          (goto-char (overlay-start o))
          (condition-case nil
              (save-restriction
                (narrow-to-region (point) (overlay-end o))
                (skip-chars-forward "\n\r\ ")
                (unless (eobp)
                  (setq call (read (current-buffer))
                        name (plist-get call :name))))
            (error (funcall read-error)))
          (unless (integerp (setq index (overlay-get o 'ai-workbench-overlay)))
            (funcall read-error))
          ;; Merge with or remove from messages array...
          (ai-workbench--inject-tool-call
           backend (plist-get info :data) (nth index tool-use) call)
          (if (not call)
              ;; Remove from tool-use
              (plist-put info :tool-use
                         (append (cl-subseq tool-use 0 index)
                                 (cl-subseq tool-use (1+ index))))
            ;; ...before modifying the tool use block
            (ai-workbench--merge-plists (nth index tool-use) call)
            (setq tool-spec-args-cb (overlay-get o 'ai-workbench-tool))
            ;; and modifying the arguments sent to the callback
            (setf (nth 0 tool-spec-args-cb)
                  (or (cl-find name (plist-get info :tools)
                               :key #'ai-workbench-tool-name :test #'string=)
                      (ai-workbench-get-tool name)))
            (setf (nth 1 tool-spec-args-cb) (plist-get call :args))
            (push tool-spec-args-cb tool-calls)))
        (ai-workbench--accept-tool-calls   ;include overlay to clean up if there is one
         tool-calls (cadr (plist-get info :tool-display)))))
     (t (let* ((tool-display (plist-get (ai-workbench-fsm-info ai-workbench--fsm-last)
                                        :tool-display)))
          (apply #'ai-workbench--accept-tool-calls tool-display)))))
  (quit-window t))

(defun ai-workbench--inspect-reject-tool-calls (&optional _)
  "Cancel tool-calls and return to query buffer."
  (interactive)
  (apply #'ai-workbench--reject-tool-calls
   (thread-first (ai-workbench-fsm-info ai-workbench--fsm-last)
                 (plist-get :tool-display)))
  (quit-window t))

(defun ai-workbench--inspect-quit-tool-calls (&optional _)
  "Quit inspection window and return to query buffer."
  (interactive)
  (quit-window t))

(defalias 'ai-workbench--inspect-tool-post-command
  (let ((highlight-ov))
    (lambda ()
      (unless (memq highlight-ov (overlays-at (point)))
        (let ((context-ov
               (cl-loop for ov in (overlays-at (point))
                        thereis (and (overlay-get ov 'ai-workbench-overlay) ov)))
              (line (propertize "\n" 'font-lock-face
                                '(:inherit separator-line :extend t))))
          (when highlight-ov
            (overlay-put highlight-ov 'face nil)
            (overlay-put highlight-ov 'before-string nil)
            (overlay-put highlight-ov 'after-string nil))
          (when context-ov
            (overlay-put context-ov 'face 'ai-workbench-response-highlight)
            (overlay-put context-ov 'before-string line)
            (overlay-put context-ov 'after-string (concat "\n" line)))
          (setq highlight-ov context-ov)))))
  "Highlight tool call under cursor in ai-workbench-engine tool call inspection buffers.")

(defun ai-workbench--inspect-tool-calls (tool-calls info &optional tool-overlay)
  "Set up and switch to a buffer to inspect pending tool-calls.

TOOL-CALLS is the alist of tool calls.  INFO is the request context
plist.  TOOL-OVERLAY is the tool call dispatch overlay (if any) in the
query buffer."
  (interactive (pcase-let ((`(,resp . ,o) (get-char-property-and-overlay
                                           (point) 'ai-workbench-tool)))
                 (list resp (overlay-get o 'info) o)))
  (with-current-buffer (get-buffer-create "*ai-workbench-tool-calls*")
    (let ((inhibit-read-only t)
          (tool-use (plist-get info :tool-use)))
      (remove-overlays)
      (erase-buffer)
      (unless (derived-mode-p 'lisp-data-mode)
        (lisp-data-mode)
        (add-hook 'post-command-hook #'ai-workbench--inspect-tool-post-command nil t))
      ;; NOTE: This needs to be called after setting the major mode, as
      ;; buffer-local variables are wiped out.
      ;; Required to store state for accepting/rejecting calls
      (setq ai-workbench--fsm-last (ai-workbench-make-fsm :info info))
      (plist-put info :tool-display (list tool-calls tool-overlay)) ;NOTE: INFO is never nil
      (insert ";; Inspect or edit tool calls.
;; Adding or deleting tool calls is not supported.\n\n")
      (cl-loop for tool-spec-args-cb in tool-calls
               for (tool-spec arg-plist _process-tool-result) = tool-spec-args-cb
               with o
               for name = (ai-workbench-tool-name tool-spec)
               for pt = (point)
               for index =
               (cl-position-if
                (lambda (call) (and (not (plist-get call :result))
                               (string= (plist-get call :name) name)
                               (null (cl-set-difference
                                      (plist-get call :args) arg-plist
                                      :test #'equal))))
                tool-use)
               do (prin1 (list :name name :args arg-plist)
                         (current-buffer) '((length . nil) (level . nil)))
               (insert "\n\n")          ;Avoid extending the overlay
               (setq o (make-overlay pt (- (point) 2) nil nil t))
               (overlay-put o 'ai-workbench-tool tool-spec-args-cb)
               (overlay-put o 'ai-workbench-overlay index))
      (goto-char (point-min)) (forward-line 3))
    (use-local-map
     (make-composed-keymap
      ai-workbench-tool-call-inspection-map (current-local-map)))
    (unless header-line-format
      (setq header-line-format
            (substitute-command-keys
             (concat
              (propertize "Tool calls" 'face 'font-lock-string-face) ": "
              (buttonize "Confirm" #'ai-workbench--inspect-accept-tool-calls)
              " \\[ai-workbench--inspect-accept-tool-calls], "
              (buttonize "Cancel" #'ai-workbench--inspect-reject-tool-calls)
              " \\[ai-workbench--inspect-reject-tool-calls], "
              (buttonize "Return" #'ai-workbench--inspect-quit-tool-calls)
              " \\[ai-workbench--inspect-quit-tool-calls], "
              (buttonize "Edit" (lambda (_) (read-only-mode 'toggle)))
              " \\[read-only-mode]"))))
    (set-buffer-modified-p nil)
    (read-only-mode)
    (ai-workbench--inspect-tool-post-command)
    (display-buffer (current-buffer) ai-workbench-display-buffer-action)))


;;; Presets
;;;; Presets implementation
(defvar ai-workbench--known-presets
  '((ai-workbench-default
     :description "Use ai-workbench-engine's default configuration."
     :context nil :use-context system
     :tools nil :use-tools t
     :temperature nil :max-tokens nil
     :num-messages-to-send nil
     :request-params nil
     :org-convert-response t
     :track-media nil
     :track-response t
     :system nil
     :stream t
     :cache nil))
  "Alist of presets for ai-workbench-engine.

Each entry maps a preset name (a symbol) to a plist of
specifications (see `ai-workbench-make-preset').")

(defun ai-workbench-make-preset (name &rest keys)
  "Define a ai-workbench-engine preset with NAME.

A preset is a combination of ai-workbench-engine options intended to be applied and
used together.  Presets can make it less tedious to change ai-workbench-engine
settings on the fly.

Typically this will include a model, backend, system message and perhaps
some tools, but any set of ai-workbench-engine options can be set this way.

NAME must be a symbol.  KEYS is a plist corresponding to the options
being set.  All KEYS are optional.

Recognized keys:

DESCRIPTION is a description of the preset, used when selecting a
preset.

PARENTS is a preset name (or list of preset names) to apply before this
one.

PRE and POST are functions to run before and after the preset is
applied.  They take no arguments.

BACKEND is the `ai-workbench-backend' to set, or its name (like \"ChatGPT\").

MODEL is the `ai-workbench-model', a symbol.

SYSTEM is the directive.  It can be
- the system message (a string),
- a list of strings (a conversation template)
- or a function (dynamic system message).
- It can also be a symbol naming a directive in `ai-workbench-directives'.

TOOLS is a list of ai-workbench-engine tools or tool names, like
\\='(\"read_url\" \"read_buffer\" ...)

Recognized keys are not limited to the above.  Any other key (like
`:foo') corresponds to the value of either `ai-workbench-foo' (preferred) or
`ai-workbench--foo'.
- So TOOLS corresponds to option `ai-workbench-llm-tools',
- CONFIRM-TOOL-CALLS to `ai-workbench-confirm-tool-calls',
- TEMPERATURE to `ai-workbench-temperature' and so on.
See ai-workbench-engine's customization options for all available settings.

Specifying the value of a key will set the corresponding ai-workbench-engine option to
it.  For example,

  (ai-workbench-make-preset \\='websearch
    :tools \\='(\"search_web\" \"read_url\")
    :system \"Use the provided tools to search the web
              for up-to-date information\")

will replace the currently active option `ai-workbench-llm-tools' and the system
message.

Alternatively,

- You can require that the value be appended or prepended to the
  existing value instead of replacing it.  This can be done by
  specifying the value as a plist instead with the keys `:prepend' or
  `:append'.

  (ai-workbench-make-preset \\='websearch
    :tools  \\='(:append (\"search_web\" \"read_url\"))
    :system \\='(:prepend \"Use the provided tools to search the web
                        for up-to-date information.\"))

- You can dynamically compute the value for a key at the time the preset
  is applied with `:eval' or `:function'.  This is mostly useful when
  using presets in the prompt, as @preset-name.

  An `:eval' form is evaluated when the preset is applied:

  (ai-workbench-make-preset \\='visible-buffers
    :description \"Include the full text of all buffers visible in the
                 frame.\"
    :context \\='(:eval (mapcar #\\='window-buffer (window-list))))
    ▲                ▲
    │                ╰╴evaluated when preset is applied
    ╰╴sets `ai-workbench-context'

  `:function' should take the current value of the key as an input and
  return the new value.  Here we combine it with `:append' in the plist.

  (ai-workbench-make-preset \\='github-read-only
    :description \"Provide read-only GitHub tools\"
    :pre (lambda () (ai-workbench-mcp-connect \\='(\"github\") \\='sync))
    :tools
    \\='( :append (\"mcp-github\")       ;Adds all github MCP tools
       :function (lambda (tools)
                   (cl-delete-if    ;Remove \"write\" access to GitHub
                    (lambda (tool)
                      (string-match-p \"create_\" (ai-workbench-tool-name tool)))
                    tools))))

  NOTE: `:eval' and `:function' are evaluated in a temporary buffer, and
  not the buffer from which the request is sent."
  (declare (indent 1))
  (if-let* ((p (assoc name ai-workbench--known-presets)))
      (setcdr p keys)
    (setq ai-workbench--known-presets          ;Add at end of presets for menu ordering
          (nconc ai-workbench--known-presets (list (cons name keys))))))

(defun ai-workbench-get-preset (name)
  "Get the ai-workbench-engine preset spec with NAME."
  (alist-get name ai-workbench--known-presets nil nil #'equal))

(defun ai-workbench--save-preset (name &optional description)
  "Save ai-workbench-engine's current settings as a preset with NAME.

NAME must be a symbol.  DESCRIPTION is added if provided.  In addition
to registering the preset, elisp code to do the same is copied to the
`kill-ring'."
  (interactive
   (list (intern (completing-read "Save ai-workbench-engine settings to (existing or new) preset: "
                                  ai-workbench--known-presets))
         (read-string "Description (optional): ")))
  (let ((preset-code
         `(ai-workbench-make-preset ',name
           :description ,(when (and description
                                (not (string-blank-p description)))
                          description)
           :backend ,(ai-workbench-backend-name ai-workbench-backend)
           :model ',ai-workbench-model
           :system ,(if-let* ((directive (car-safe (rassoc ai-workbench-system-prompt
                                                           ai-workbench-directives))))
                         `',directive
                      ai-workbench-system-prompt)
           :tools ',(mapcar #'ai-workbench-tool-name ai-workbench-llm-tools)
           :stream ,ai-workbench-stream
           :temperature ,ai-workbench-temperature
           :max-tokens ,ai-workbench-max-tokens
           :use-context ',ai-workbench-use-context
           :track-media ,ai-workbench-track-media
           :include-reasoning ,(let ((reasoning ai-workbench-include-reasoning))
                                   (if (eq reasoning 'ignore)
                                       ''ignore reasoning)))))
    (kill-new (pp-to-string preset-code))
    (eval preset-code)
    (message "Preset %s saved. (Lisp expression for preset saved to kill-ring)"
             (propertize (symbol-name name) 'face 'highlight))))

(defvar ai-workbench--rewrite-directive)
(defun ai-workbench--apply-preset (preset &optional setter)
  "Apply ai-workbench-engine PRESET with SETTER.

PRESET is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...).

SETTER is the function used to set the ai-workbench-engine options.  It must accept
two arguments, the symbol being set and the value to set it to.  It
defaults to `set', and can be set to a different function to (for
example) apply the preset buffer-locally."
  (unless setter (setq setter #'set))
  (cl-flet ((preset-spec (preset)
              (if (memq (type-of preset) '(symbol string))
                  (or (ai-workbench-get-preset preset)
                      (user-error "ai-workbench-engine preset \"%s\": Cannot find preset"
                                  preset))
                preset)))
    ;; Record preset name for persistence and UI display
    (when (memq (type-of preset) '(string symbol))
      (funcall setter 'ai-workbench--preset preset))
    ;; Ensure that preset is a plist spec
    (setq preset (preset-spec preset))
    (when-let* ((func (plist-get preset :pre))) (funcall func))
    (when-let* ((parents (plist-get preset :parents)))
      (mapc (lambda (parent) (ai-workbench--apply-preset (preset-spec parent) setter))
            (ensure-list parents))))
  (map-do
   (lambda (key val)
     (pcase key
       ((or :parents :description :pre :post) nil)
       ;; TODO(v1.0): Remove :system-message from this list
       ((or :system :system-prompt :system-message :rewrite-directive)
        (let ((sym (if (eq key :rewrite-directive)
                       'ai-workbench--rewrite-directive 'ai-workbench-system-prompt)))
          (when (consp val)
            ;; Possibly complain about trying to compose a system message string
            ;; with a non-string
            ;; TODO(modify-list): Catch other incompatible combinations
            (and (or (plist-member val :append) (plist-member val :prepend))
                 (not (stringp (symbol-value sym)))
                 (user-error "Composing non-string system messages is not implemented"))
            (setq val (ai-workbench--modify-value (symbol-value sym) val)))
          (if (and val (symbolp val) (not (functionp val)))
              (if-let* ((directive (alist-get val ai-workbench-directives)))
                  (funcall setter sym directive)
                (user-error "ai-workbench-engine preset: Cannot find directive %s" val))
            (funcall setter sym val))))
       (:backend
        (when (consp val) (setq val (ai-workbench--modify-value 'ai-workbench-backend val)))
        (setq val (cl-etypecase val
                    (ai-workbench-backend val)
                    (string (ai-workbench-get-backend val))))
        (unless val
          (user-error "ai-workbench-engine preset: Cannot find backend %s" val))
        (funcall setter 'ai-workbench-backend val))
       (:tools                          ;TEMP Confirm this `:append' convention
        (setq val (ai-workbench--modify-value ai-workbench-llm-tools val))
        (let* ((tools
                (flatten-list
                 (cl-loop for tool-name in (ensure-list val)
                          for tool = (cl-etypecase tool-name
                                       (ai-workbench-tool tool-name)
                                       (string (ignore-errors
                                                 (ai-workbench-get-tool tool-name))))
                          do (unless tool
                               (user-error "ai-workbench-engine preset: Cannot find tool %S"
                                           tool-name))
                          collect tool))))
          (funcall setter 'ai-workbench-llm-tools (cl-delete-duplicates tools :test #'eq))))
       ((and (let sym (or (intern-soft
                           (concat "ai-workbench-" (substring (symbol-name key) 1)))
                          (intern-soft
                           (concat "ai-workbench--" (substring (symbol-name key) 1)))))
             (guard (and sym (boundp sym))))
        (funcall setter sym (if (consp val)
                                (ai-workbench--modify-value (symbol-value sym) val)
                              val)))
       (_ (display-warning
           '(ai-workbench-engine presets)
           (format "ai-workbench-engine preset: setting for %s not found, ignoring." key)))))
   preset)
  (when-let* ((func (plist-get preset :post))) (funcall func)))

(defun ai-workbench--preset-syms (preset)
  "Return a list of ai-workbench-engine variables (symbols) set by PRESET.

PRESET is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...)."
  (when (memq (type-of preset) '(string symbol))
    (let ((spec (or (ai-workbench-get-preset preset)
                    (user-error "ai-workbench-engine preset \"%s\": Cannot find preset"
                                preset))))
      (setq preset spec)))
  (let* ((index preset)
         syms key val)
    (while index
      (setq key (pop index) val (pop index))
      (pcase key
        ((or :description :pre :post))
        (:parents
         (setq syms
               (nconc syms (mapcan #'ai-workbench--preset-syms (ensure-list val)))))
        (:system (push 'ai-workbench-system-prompt syms))
        (_ (if-let* ((var (or (intern-soft
                               (concat "ai-workbench-" (substring (symbol-name key) 1)))
                              (intern-soft
                               (concat "ai-workbench--" (substring (symbol-name key) 1))))))
               (push var syms)
             (display-warning
              '(ai-workbench-engine presets)
              (format "ai-workbench-engine preset \"%s\": setting for %s not found, ignoring."
                      (car preset) key))))))
    (cl-delete-duplicates syms)))

;; This is identical to `cl-progv', only we let-bind symbols SYM from the preset
;; to their current values instead of evaluating the values explicitly. (#1005)
(defmacro ai-workbench-with-preset (name &rest body)
  "Run BODY with ai-workbench-engine preset NAME applied.

This macro can be used to create `ai-workbench-request' command with settings
from a ai-workbench-engine preset applied.

NAME is the name of a preset, or a spec (plist) of the form
 (:KEY1 VAL1 :KEY2 VAL2 ...).  It must be quoted."
  (declare (indent 1))
  (let ((syms (make-symbol "syms"))
        (binds (make-symbol "binds"))
        (bodyfun (make-symbol "body")))
    ;; Let-bind symbols that we want to modify with the presets.  Also include
    ;; `ai-workbench--preset' in this list as we don't want to change its value outside
    ;; of this macro's scope.
    `(let* ((,syms (cons 'ai-workbench--preset (ai-workbench--preset-syms ,name)))
            (,bodyfun (lambda () (ai-workbench--apply-preset ,name) ,@body))
            (,binds nil))
       (while ,syms (push (list (car ,syms) (pop ,syms)) ,binds))
       (eval (list 'let (nreverse ,binds) (list 'funcall (list 'quote ,bodyfun)))))))

(defun ai-workbench--preset-mismatch-value (preset-spec key val)
  "Determine if the value of KEY in PRESET-SPEC matches VAL.

This is an imperfect check for whether the value corresponding to KEY (a
keyword) in PRESET-SPEC (a plist) matches VAL.  This is required
primarily to identify which ai-workbench-engine variable values have changed since
PRESET-SPEC was applied, which is relevant when writing ai-workbench-engine metadata
to a chat file.

See also `ai-workbench--preset-mismatch-p'."
  ;; In all cases, assume a mismatch if the preset's value for KEY is a
  ;; modify-list spec, such as (:append ...)
  ;; Mismatches may not even be well-defined/determinable in these cases.
  (or (not preset-spec)
      (pcase key
        ;; special cases
        ((or :system :system-message)
         (let ((system (plist-get preset-spec :system)))
           (or (and (stringp system) (not (equal system val)))
               (functionp system)
               (and (consp system) (keywordp (car system)))
               (and (consp system)
                    (not (equal (car-safe (ai-workbench--parse-directive system))
                                val))))))
        (:backend
         (let ((backend (plist-get preset-spec :backend)))
           (or (and (consp backend) (keywordp (car-safe backend)))
               (not (equal (or (and (ai-workbench-backend-p val) (ai-workbench-backend-name val))
                               val)
                           (or (and (ai-workbench-backend-p backend) (ai-workbench-backend-name backend))
                               backend))))))
        ;; FIXME: We're assuming that val is a list of tool names, not tools
        (:tools
         (and-let* ((preset-tools (plist-get preset-spec :tools)))
           (or (keywordp (car-safe preset-tools))
               (cl-loop
                for tool in preset-tools
                for tool-name =
                (or (and (stringp tool) tool)
                    (ignore-errors (ai-workbench-tool-name tool)))
                if (not (member tool-name uniq-tool-names))
                collect tool-name into uniq-tool-names
                finally return
                (not (equal (sort uniq-tool-names #'string-lessp)
                            (sort (copy-sequence (ensure-list val)) #'string-lessp)))))))
        ;; Generic case
        (_ (let ((field-val (plist-get preset-spec key)))
             (or (and (consp field-val) (keywordp (car field-val)))
                 (not (equal field-val val))))))))

;;;; Presets in-buffer UI
(defun ai-workbench--transform-apply-preset (_fsm)
  "Apply a ai-workbench-engine preset to the buffer depending on the prompt.

If the last user prompt includes @foo, the preset foo is applied.
Before applying the preset, \"@foo\" is removed from the prompt and
point is placed at its position."
  (when ai-workbench--known-presets
    (text-property-search-backward 'ai-workbench-engine nil t)
    (while (re-search-forward "@\\([^[:space:]]+\\)\\_>" nil t)
      ;; The following convoluted check is because re-search is much faster if
      ;; the search pattern begins with a non-whitespace char.
      (when (or (= (match-beginning 0) (point-min))
                (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
        (when-let* ((name (match-string 1))
                    (preset (or (ai-workbench-get-preset (intern-soft name))
                                (ai-workbench-get-preset name))))
          (delete-region (match-beginning 0) (match-end 0))
          ;; Point must be after @foo when the preset is applied to allow for
          ;; more advanced transformations.
          (ai-workbench--apply-preset preset
                               (lambda (sym val)
                                 (set (make-local-variable sym) val))))))))

;; ;; Alternative approach with string search
;; (search-forward "@" nil t)
;; (if (and (memq (char-syntax (char-before (1- (point)))) '(32 62))
;;          (looking-at "\\([^[:blank:]]+?\\)[[:punct:]]?\\s-+"))
;;     do-stuff)

(defun ai-workbench--fontify-preset-keyword (end)
  "Font-lock function for preset indicators in chat buffers.

Return preset fontification info for text up to END."
  (and (re-search-forward "@\\([^[:space:]]+\\)\\_>" end t)
       (or (= (match-beginning 0) (point-min))
           (memq (char-syntax (char-before (match-beginning 0))) '(32 62)))
       (not (plist-get (text-properties-at (match-beginning 1)) 'ai-workbench-engine))))

(defun ai-workbench-preset-capf ()
  "Completion at point for ai-workbench-engine presets in `ai-workbench-mode'.

Add this to `completion-at-point-functions'."
  (and ai-workbench--known-presets
       (save-excursion
         (let ((num (- (skip-syntax-backward "w_"))))
           (when (eql (char-before) ?@)
             (list (point) (+ (point) num)
                   ai-workbench--known-presets
                   :exclusive 'no
                   :annotation-function
                   #'(lambda (c) (thread-first
                              (intern-soft c)
                              (assq ai-workbench--known-presets) (cdr)
                              (plist-get :description)))))))))

(defun ai-workbench--prettify-preset ()
  "Get visual and completion help with presets in ai-workbench-engine buffers.

Intended to be added to `ai-workbench-mode-hook'."
  (let ((keyword '((ai-workbench--fontify-preset-keyword
                    ;; subexp 0 here is not required, we retain it to make it
                    ;; easy to swtich to more complex patterns in the future
                    0 (when-let* ((comps (all-completions (match-string 1)
                                          ai-workbench--known-presets))
                                  ((member (match-string 1) comps)))
                       '(:box -1 :inherit secondary-selection))
                    prepend))))
    (cond
     (ai-workbench-mode
      (font-lock-add-keywords nil keyword t)
      (add-hook 'completion-at-point-functions #'ai-workbench-preset-capf nil t))
     (t (font-lock-remove-keywords nil keyword)
        (remove-hook 'completion-at-point-functions #'ai-workbench-preset-capf t)))))


;;; Response tweaking commands

(defun ai-workbench--attach-response-history (history &optional buf)
  "Attach HISTORY to the next ai-workbench-engine response in buffer BUF.

HISTORY is a list of strings typically containing text replaced
by ai-workbench-engine.  BUF is the current buffer if not specified.

This is used to maintain variants of prompts or responses to diff
against if required."
  (with-current-buffer (or buf (current-buffer))
    (letrec ((ai-workbench--attach-after
              (lambda (b e)
                (when (and b e)
                  (add-text-properties
                   b e `(ai-workbench-history
                         ,(append (ensure-list history)
                           (get-char-property (1- e) 'ai-workbench-history))
                         front-sticky (ai-workbench-engine ai-workbench-history))))
                (remove-hook 'ai-workbench-post-response-functions
                             ai-workbench--attach-after 'local))))
      (add-hook 'ai-workbench-post-response-functions ai-workbench--attach-after
                nil 'local))))

(defun ai-workbench--ediff (&optional arg bounds-func)
  "Ediff response at point against previous ai-workbench-engine responses.

If prefix ARG is non-nil, select the previous response to ediff
against interactively.

If specified, use BOUNDS-FUNC to compute the bounds of the
response at point.  This can be used to include additional
context for the ediff session."
  (interactive "P")
  (when (ai-workbench--at-response-history-p)
    (pcase-let* ((`(,beg . ,end) (funcall (or bounds-func #'ai-workbench--get-response-bounds)))
                 (prev-response
                  (if arg
                      (completing-read "Choose response variant to diff against: "
                                       (get-char-property (point) 'ai-workbench-history)
                                       nil t)
                    (car-safe (get-char-property (point) 'ai-workbench-history))))
                 (buffer-mode major-mode)
                 (bufname (buffer-name))
                 (`(,new-buf ,new-beg ,new-end)
                  (with-current-buffer
                      (get-buffer-create (concat bufname "-PREVIOUS-*"))
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (delay-mode-hooks (funcall buffer-mode))
                      (visual-line-mode)
                      (insert prev-response)
                      (goto-char (point-min))
                      (list (current-buffer) (point-min) (point-max))))))
      (unless prev-response (user-error "ai-workbench-engine response is additive: no changes to ediff"))
      (require 'ediff)
      (letrec ((cwc (current-window-configuration))
               (ai-workbench--ediff-restore
                (lambda ()
                  (when (window-configuration-p cwc)
                    (set-window-configuration cwc))
                  (kill-buffer (get-buffer (concat bufname "-PREVIOUS-*")))
                  (kill-buffer (get-buffer (concat bufname "-CURRENT-*")))
                  (remove-hook 'ediff-quit-hook ai-workbench--ediff-restore))))
        (add-hook 'ediff-quit-hook ai-workbench--ediff-restore)
        (apply
         #'ediff-regions-internal
         (get-buffer (ediff-make-cloned-buffer (current-buffer) "-CURRENT-*"))
         beg end new-buf new-beg new-end
         nil
         (list 'ediff-regions-wordwise 'word-wise nil)
         ;; (if (transient-arg-value "-w" args)
         ;;     (list 'ediff-regions-wordwise 'word-wise nil)
         ;;   (list 'ediff-regions-linewise nil nil))
         )))))

(defun ai-workbench--mark-response ()
  "Mark ai-workbench-engine response at point, if any."
  (interactive)
  (unless (ai-workbench--in-response-p) (user-error "No ai-workbench-engine response at point"))
  (pcase-let ((`(,beg . ,end) (ai-workbench--get-response-bounds)))
    (goto-char beg) (push-mark) (goto-char end) (activate-mark)))

(defun ai-workbench--previous-variant (&optional arg)
  "Switch to ARG previous ai-workbench-response at this point, if it exists."
  (interactive "p")
  (pcase-let* ((`(,beg . ,end) (ai-workbench--get-response-bounds))
               (history (get-char-property (point) 'ai-workbench-history))
               (alt-response (car-safe history))
               (offset))
    (unless (and history alt-response)
      (user-error "No variant responses available"))
    (if (> arg 0)
        (setq history (append (cdr history)
                              (list (buffer-substring-no-properties beg end))))
      (setq
       alt-response (car (last history))
       history (cons (buffer-substring-no-properties beg end)
                     (nbutlast history))))
    (add-text-properties
             0 (length alt-response)
             `(ai-workbench-engine response ai-workbench-history ,history)
             alt-response)
    (setq offset (min (- (point) beg) (1- (length alt-response))))
    (delete-region beg end)
    (insert alt-response)
    (goto-char (+ beg offset))
    (pulse-momentary-highlight-region beg (+ beg (length alt-response)))))

(defun ai-workbench--next-variant (&optional arg)
  "Switch to ARG next ai-workbench-response at this point, if it exists."
  (interactive "p")
  (ai-workbench--previous-variant (- arg)))

(provide 'ai-workbench-engine)
;;; ai-workbench-engine.el ends here

;; Local Variables:
;; bug-reference-url-format: "https://github.com/karthink/ai-workbench-engine/issues/%s"
;; End:
