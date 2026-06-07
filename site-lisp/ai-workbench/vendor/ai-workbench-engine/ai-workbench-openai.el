;;; ai-workbench-openai.el ---  ChatGPT suppport for ai-workbench-engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>

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

;; This file adds support for the ChatGPT API to ai-workbench-engine

;;; Code:
(require 'cl-generic)
(eval-when-compile (require 'cl-lib))
(require 'map)
(eval-and-compile (require 'ai-workbench-request))

(defvar json-object-type)
(defvar ai-workbench-mode)
(declare-function ai-workbench-context--collect-media "ai-workbench-context")
(declare-function json-read "json")
(declare-function ai-workbench-context--wrap "ai-workbench-context")

;; OpenAI Completions Backend
(cl-defstruct (ai-workbench-openai (:constructor ai-workbench--make-openai)
                            (:copier nil)
                            (:include ai-workbench-backend)))

;; OpenAI Responses Backend
(cl-defstruct (ai-workbench-openai-responses (:constructor ai-workbench--make-openai-responses)
                                      (:copier nil)
                                      (:include ai-workbench-backend)))


(defun ai-workbench--openai-update-tokens (usage info)
  "Update token usage information from USAGE.
USAGE is part of the response, INFO is the request plist.

This function accumulates token counts across multiple turns in a
multi-turn request (e.g., during tool use where results are fed
back to the LLM)."
  (when usage
    (let ((input (or (plist-get usage :prompt_tokens) 0))
          (output (or (plist-get usage :completion_tokens) 0))
          (cached (or (map-nested-elt
                       usage '(:prompt_tokens_details :cached_tokens))
                      (plist-get usage :prompt_cache_hit_tokens) 
                      0)))
      ;; prompt_tokens includes the cached tokens, but we capture and display
      ;; the two exclusively in the UI.
      (let ((tokens (list :input (- input cached) :output output :cached cached)))
        (plist-put info :tokens tokens) ;Tokens for this turn
        (plist-put info :tokens-full    ;Tokens for full request
                   (ai-workbench--sum-plists (plist-get info :tokens-full) tokens))))))

;; How the following function works:
;;
;; The OpenAI API returns a stream of data chunks.  Each data chunk has a
;; component that can be parsed as JSON.  Besides metadata, each chunk has
;; either some text or part of a tool call.
;;
;; If we find text, we collect it in a list, concat them at the end and return
;; it.
;;
;; If we find part of a tool call, we begin collecting the pieces in
;; INFO -> :tool-use.
;;
;; Tool call arguments are themselves JSON encoded strings can be spread across
;; chunks.  We collect them in INFO -> :partial_json.  The end of a tool call
;; chunk is marked by the beginning of another, or by the end of the stream.  In
;; either case we flaten the :partial_json we have thus far, add it to the tool
;; call spec in :tool-use and reset it.
;;
;; If we find reasoning text, collect it in INFO -> :reasoning, to be consumed
;; by the stream filter (and eventually the callback).  We also collect it in
;; INFO -> :reasoning-chunks, in case we need to send it back along with tool
;; call results.
;;
;; Finally we append any tool calls and accumulated reasoning text (from
;; :reasoning-chunks) to the (INFO -> :data -> :messages) list of prompts.

(cl-defmethod ai-workbench-curl--parse-stream ((_backend ai-workbench-openai) info)
  "Parse an OpenAI API data stream.

Return the text response accumulated since the last call to this
function.  Additionally, mutate state INFO to add tool-use
information if the stream contains it."
  (let* ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (save-match-data
            (if (looking-at " *\\[DONE\\]")
                ;; The stream has ended, so we do the following thing (if we found tool calls)
                ;; - pack tool calls into the messages prompts list to send (INFO -> :data -> :messages)
                ;; - collect tool calls (formatted differently) into (INFO -> :tool-use)
                ;; - Clear any reasoning content chunks we've captured
                (progn
                  (when-let* ((tool-use (plist-get info :tool-use))
                              (args (apply #'concat (nreverse (plist-get info :partial_json))))
                              (func (plist-get (car tool-use) :function)))
                    (plist-put func :arguments args) ;Update arguments for last recorded tool
                    (ai-workbench--inject-prompt
                     (plist-get info :backend) (plist-get info :data)
                     `( :role "assistant" :content :null :tool_calls ,(vconcat tool-use) ; :refusal :null
                        ;; Return reasoning if available
                        ,@(and-let* ((chunks (nreverse (plist-get info :reasoning-chunks)))
                                     (reasoning-field (pop chunks))) ;chunks is (:reasoning.* "chunk1" "chunk2" ...)
                            (list reasoning-field (apply #'concat chunks)))))
                    (cl-loop
                     for tool-call in tool-use ; Construct the call specs for running the function calls
                     for spec = (plist-get tool-call :function)
                     collect (list :id (plist-get tool-call :id)
                                   :name (plist-get spec :name)
                                   :args (ignore-errors (ai-workbench--json-read-string
                                                         (plist-get spec :arguments))))
                     into call-specs
                     finally (plist-put info :tool-use call-specs)))
                  ;; Update token usage if present
                  (when-let* ((last-resp (save-excursion
                                           (forward-line -1)
                                           (and (re-search-backward "^data:" nil t)
                                                (goto-char (match-end 0))
                                                (ignore-errors (ai-workbench--json-read)))))
                              (usage (plist-get last-resp :usage)))
                    (ai-workbench--openai-update-tokens usage info))
                  (when (plist-member info :reasoning-chunks) (plist-put info :reasoning-chunks nil)))
              (when-let* ((response (ai-workbench--json-read))
                          (delta (map-nested-elt response '(:choices 0 :delta))))
                (if-let* ((content (plist-get delta :content))
                          ((not (or (eq content :null) (string-empty-p content)))))
                    (push content content-strs)
                  ;; No text content, so look for tool calls
                  (when-let* ((tool-call (map-nested-elt delta '(:tool_calls 0)))
                              (func (plist-get tool-call :function)))
                    (if (and-let* ((func-name (plist-get func :name)) ((not (eq func-name :null))))
                          ;; TEMP: This check is for litellm compatibility, should be removed
                          (not (equal func-name "null"))) ; new tool block begins
                        (progn
                          (when-let* ((partial (plist-get info :partial_json)))
                            (let* ((prev-tool-call (car (plist-get info :tool-use)))
                                   (prev-func (plist-get prev-tool-call :function)))
                              (plist-put prev-func :arguments ;update args for old tool block
                                         (apply #'concat (nreverse (plist-get info :partial_json)))))
                            (plist-put info :partial_json nil)) ;clear out finished chain of partial args
                          ;; Start new chain of partial argument strings
                          (plist-put info :partial_json (list (plist-get func :arguments)))
                          ;; NOTE: Do NOT use `push' for this, it prepends and we lose the reference
                          (plist-put info :tool-use (cons tool-call (plist-get info :tool-use))))
                      ;; old tool block continues, so continue collecting arguments in :partial_json 
                      (push (plist-get func :arguments) (plist-get info :partial_json)))))
                ;; Check for reasoning blocks, currently only used by Openrouter
                (unless (eq (plist-get info :reasoning-block) 'done)
                  (if-let* ((reasoning-plist ;reasoning-plist is (:reasoning.* "chunk" ...) or nil
                             (or (plist-member delta :reasoning) ;for Openrouter and co
                                 (plist-member delta :reasoning_content))) ;for Deepseek, Llama.cpp
                            (reasoning-chunk (cadr reasoning-plist))
                            ((not (or (eq reasoning-chunk :null) (string-empty-p reasoning-chunk)))))
                      (progn (plist-put info :reasoning ;For stream filter consumption
                                        (concat (plist-get info :reasoning) reasoning-chunk))
                             (plist-put info :reasoning-chunks ;To include with tool call results, if any
                                        (cons reasoning-chunk (or (plist-get info :reasoning-chunks)
                                                                  (list (car reasoning-plist))))))
                    ;; Done with reasoning if we get non-empty content
                    (if-let* (((plist-member info :reasoning)) ;Is this a reasoning model?
                              (c (plist-get delta :content)) ;Started receiving text content?
                              ((not (or (eq c :null) (string-blank-p c)))))
                        (plist-put info :reasoning-block t)))))))) ;Signal end of reasoning block
      (error (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod ai-workbench--parse-response ((_backend ai-workbench-openai) response info)
  "Parse an OpenAI (non-streaming) RESPONSE and return response text.

Mutate state INFO with response metadata."
  (let* ((choice0 (map-nested-elt response '(:choices 0)))
         (message (plist-get choice0 :message))
         (content (plist-get message :content)))
    (plist-put info :stop-reason
               (plist-get choice0 :finish_reason))
    (ai-workbench--openai-update-tokens (map-nested-elt response '(:usage)) info)
    ;; OpenAI returns either non-blank text content or a tool call, not both.
    ;; However OpenAI-compatible APIs like llama.cpp can include both (#819), so
    ;; we check for both tool calls and responses independently.
    (when-let* ((tool-calls (plist-get message :tool_calls))
                ((not (eq tool-calls :null))))
      (ai-workbench--inject-prompt        ; First add the tool call to the prompts list
       (plist-get info :backend) (plist-get info :data) message)
      (cl-loop             ;Then capture the tool call data for running the tool
       for tool-call across tool-calls  ;replace ":arguments" with ":args"
       for call-spec = (copy-sequence (plist-get tool-call :function))
       do (ignore-errors (plist-put call-spec :args
                                    (ai-workbench--json-read-string
                                     (plist-get call-spec :arguments))))
       (plist-put call-spec :arguments nil)
       (plist-put call-spec :id (plist-get tool-call :id))
       collect call-spec into tool-use
       finally (plist-put info :tool-use tool-use)))
    (when-let* ((reasoning (or (plist-get message :reasoning) ;for Openrouter and co
                               (plist-get message :reasoning_content))) ;for Deepseek, Llama.cpp
                ((and (stringp reasoning) (not (string-empty-p reasoning)))))
      (plist-put info :reasoning reasoning))
    (when (and content (not (or (eq content :null) (string-empty-p content))))
      content)))

(cl-defmethod ai-workbench--request-data ((backend ai-workbench-openai) prompts)
  "JSON encode PROMPTS for sending to ChatGPT."
  (when ai-workbench-system-prompt
    (push (list :role "system"
                :content ai-workbench-system-prompt)
          prompts))
  (let ((prompts-plist
         (list :model (ai-workbench--model-name ai-workbench-model)
               :messages (vconcat prompts)
               :stream (or ai-workbench-stream :json-false))))
    (when ai-workbench-stream
      (plist-put prompts-plist :stream_options '(:include_usage t)))
    (when ai-workbench-temperature
      (plist-put prompts-plist :temperature ai-workbench-temperature))
    (when ai-workbench-use-tools
      (when (eq ai-workbench-use-tools 'force)
        (plist-put prompts-plist :tool_choice "required"))
      (when ai-workbench-llm-tools
        (plist-put prompts-plist :tools
                   (ai-workbench--parse-tools backend ai-workbench-llm-tools))
        (plist-put prompts-plist :parallel_tool_calls t)))
    (when ai-workbench-max-tokens
      ;; HACK: The OpenAI API has deprecated max_tokens, but we still need it
      ;; for OpenAI-compatible APIs like GPT4All (#485)
      (plist-put prompts-plist :max_tokens ai-workbench-max-tokens))
    (when ai-workbench--schema
      (plist-put prompts-plist
                 :response_format (ai-workbench--parse-schema backend ai-workbench--schema)))
    ;; Merge request params with model and backend params.
    (ai-workbench--merge-plists
     prompts-plist
     ai-workbench--request-params
     (ai-workbench-backend-request-params ai-workbench-backend)
     (ai-workbench--model-request-params  ai-workbench-model))))

(cl-defmethod ai-workbench--parse-schema ((_backend ai-workbench-openai) schema)
  (list :type "json_schema"
        :json_schema
        (list :name (md5 (format "%s" (random)))
              :schema (ai-workbench--preprocess-schema
                       (ai-workbench--dispatch-schema-type schema))
              :strict t)))

;; NOTE: No `ai-workbench--parse-tools' method required for ai-workbench-openai, since this is
;; handled by its defgeneric implementation

(cl-defmethod ai-workbench--inject-tool-call ((_backend ai-workbench-openai) data tool-call new-call)
  "Replace TOOL-CALL in query DATA with NEW-CALL.

BACKEND is the `ai-workbench-backend'.  See the generic function documentation
for details.  This implementation handles the OpenAI-compatible
Completions API."
  ;; FIXME: We currently assume that the tool call being modified is in the last
  ;; position in the messages array.
  (if-let* ((messages (plist-get data :messages))
            (entry (aref messages (1- (length messages))))
            (calls (plist-get entry :tool_calls))
            (id (plist-get tool-call :id))
            (indexed-call
             (cl-loop for chunk across calls
                      for i upfrom 0
                      if (equal (plist-get chunk :id) id)
                      return (cons i (plist-get chunk :function))
                      finally return nil))
            (index (car indexed-call))
            (call (cdr indexed-call)))
      (if (null new-call)               ;delete tool call if new-call is nil
          (if (= (length calls) 1)      ;only tool call, delete message entirely
              (plist-put data :messages (substring messages nil -1))
            ;; delete new-call selectively from parallel tool calls
            (plist-put entry :tool_calls
                       (vconcat (substring calls 0 index)
                                (substring calls (1+ index)))))
        (when-let* ((args (plist-get new-call :args)))
          (plist-put call :arguments (ai-workbench--json-encode args)))
        (when-let* ((name (plist-get new-call :name)))
          (plist-put call :name name)))
    (display-warning
     '(ai-workbench-engine tool-call)
     (format "Could not inject updated tool-call arguments for tool call %s, %s"
             (plist-get tool-call :name)
             (truncate-string-to-width (prin1-to-string new-call) 50 nil nil t)))))

(cl-defmethod ai-workbench--parse-tool-results ((_backend ai-workbench-openai) tool-use)
  "Return a prompt containing tool call results in TOOL-USE."
  ;; (declare (side-effect-free t))
  (mapcar
   (lambda (tool-call)
     (list
      :role "tool"
      :tool_call_id (plist-get tool-call :id)
      :content (plist-get tool-call :result)))
   tool-use))

;; TODO: Remove these functions (#792)
(defun ai-workbench--openai-format-tool-id (tool-id)
  "Format TOOL-ID for OpenAI.

If the ID has the format used by a different backend, use as-is."
  (unless tool-id
    (setq tool-id (substring
                   (md5 (format "%s%s" (random) (float-time)))
                   nil 24)))
  (if (or (string-prefix-p "toolu_" tool-id) ;#747
          (string-prefix-p "call_"  tool-id))
      tool-id
    (format "call_%s" tool-id)))

(defun ai-workbench--openai-unformat-tool-id (tool-id)
  "Return the raw tool ID for TOOL-ID."
  (or (and (string-match "call_\\(.+\\)" tool-id)
           (match-string 1 tool-id))
      tool-id))

;; NOTE: No `ai-workbench--inject-prompt' method required for ai-workbench-openai, since this
;; is handled by its defgeneric implementation

(cl-defmethod ai-workbench--parse-list ((backend ai-workbench-openai) prompt-list)
  (if (consp (car prompt-list))
      (let ((full-prompt))              ; Advanced format, list of lists
        (dolist (entry prompt-list)
          (pcase entry
            (`(prompt . ,msg)
             (push (list :role "user" :content (or (car-safe msg) msg)) full-prompt))
            (`(response . ,msg)
             (push (list :role "assistant" :content (or (car-safe msg) msg)) full-prompt))
            (`(tool . ,call)
             (unless (plist-get call :id)
               (plist-put call :id (ai-workbench--openai-format-tool-id nil)))
             (push
              (list
               :role "assistant"
               :tool_calls
               (vector
                (list :type "function"
                      :id (plist-get call :id)
                      :function `( :name ,(plist-get call :name)
                                   :arguments ,(decode-coding-string
                                                (ai-workbench--json-encode (plist-get call :args))
                                                'utf-8 t)))))
              full-prompt)
             (push (car (ai-workbench--parse-tool-results backend (list (cdr entry)))) full-prompt))))
        (nreverse full-prompt))
    (cl-loop for text in prompt-list    ; Simple format, list of strings
             for role = t then (not role)
             if text collect
             (list :role (if role "user" "assistant") :content text))))

(cl-defmethod ai-workbench--parse-buffer ((backend ai-workbench-openai) &optional max-entries)
  (let ((prompts) (prev-pt (point)))
    (if (or ai-workbench-mode ai-workbench-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'ai-workbench-engine nil (point-min))))
          (pcase (get-char-property (point) 'ai-workbench-engine)
            ('response
             (when-let* ((content (ai-workbench--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :content content) prompts)))
            (`(tool . ,id)
             (save-excursion
               (condition-case nil
                   (let* ((tool-call (read (current-buffer)))
                          (name (plist-get tool-call :name))
                          (arguments (decode-coding-string
                                      (ai-workbench--json-encode (plist-get tool-call :args))
                                      'utf-8 t)))
                     (setq id (ai-workbench--openai-format-tool-id id))
                     (plist-put tool-call :id id)
                     (plist-put tool-call :result
                                (string-trim (buffer-substring-no-properties
                                              (point) prev-pt)))
                     (push (car (ai-workbench--parse-tool-results backend (list tool-call)))
                           prompts)
                     (push (list :role "assistant"
                                 :tool_calls
                                 (vector (list :type "function"
                                               :id id
                                               :function `( :name ,name
                                                            :arguments ,arguments))))
                           prompts))
                 ((end-of-file invalid-read-syntax)
                  (message (format "Could not parse tool-call %s on line %s"
                                   id (line-number-at-pos (point))))))))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (if ai-workbench-track-media
                 (when-let* ((content (ai-workbench--openai-parse-multipart
                                       (ai-workbench--parse-media-links major-mode
                                                                 (point) prev-pt))))
                   (when (> (length content) 0)
                     (push (list :role "user" :content content) prompts)))
               (when-let* ((content (ai-workbench--trim-prefixes (buffer-substring-no-properties
                                                           (point) prev-pt))))
                 (push (list :role "user" :content content) prompts)))))
          (setq prev-pt (point)))
      (let ((content (string-trim (buffer-substring-no-properties
                                    (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

;; TODO This could be a generic function
(defun ai-workbench--openai-parse-multipart (parts)
  "Convert a multipart prompt PARTS to the OpenAI API format.

The input is an alist of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\")).

The output is a vector of entries in a backend-appropriate
format."
  (cl-loop
   for part in parts
   for n upfrom 1
   with last = (length parts)
   for text = (plist-get part :text)
   for media = (plist-get part :media)
   if text do
   (and (or (= n 1) (= n last)) (setq text (ai-workbench--trim-prefixes text)))
   and if text
   collect `(:type "text" :text ,text) into parts-array end
   else if media collect
   `(:type "image_url"
     :image_url (:url ,(concat "data:" (plist-get part :mime)
                        ";base64," (ai-workbench--base64-encode media))))
   into parts-array
   else if (plist-get part :textfile) collect
   `(:type "text"
     :text ,(with-temp-buffer
              (ai-workbench--insert-file-string (plist-get part :textfile))
              (buffer-string)))
   into parts-array end and
   if (plist-get part :url)
   collect
   `(:type "image_url"
     :image_url (:url ,(plist-get part :url)))
   into parts-array
   finally return (vconcat parts-array)))

(cl-defmethod ai-workbench--inject-media ((_backend ai-workbench-openai) prompts)
  "Wrap the first user prompt in PROMPTS with included media files.

Media files, if present, are placed in `ai-workbench-context'."
  (when-let* ((media-list (ai-workbench-context--collect-media)))
    (cl-callf (lambda (current)
                (vconcat
                 (ai-workbench--openai-parse-multipart media-list)
                 (cl-typecase current
                   (string `((:type "text" :text ,current)))
                   (vector current)
                   (t current))))
        (plist-get (car prompts) :content))))

(defconst ai-workbench--openai-models
  '((gpt-5.4-mini
     :description "Faster, more cost-efficient version of GPT-5.4"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.75
     :output-cost 4.50
     :cutoff-date "2025-08")
    (gpt-5.4-nano
     :description "Fastest, cheapest version of GPT-5.4"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.20
     :output-cost 1.25
     :cutoff-date "2025-08")
    (gpt-5.4
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1050
     :input-cost 2.50
     :output-cost 15
     :cutoff-date "2025-08")
    (gpt-5.4-pro
     :description "Maximum performance model for reasoning tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1050
     :input-cost 30
     :output-cost 180
     :cutoff-date "2025-08")
    (gpt-5.5
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1050
     :input-cost 5
     :output-cost 30
     :cutoff-date "2025-12")
    (gpt-5.5-pro
     :description "Maximum performance model for reasoning tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1050
     :input-cost 30
     :output-cost 180
     :cutoff-date "2025-12")
    (gpt-5.3-chat-latest
     :description "Answers right away"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.75
     :output-cost 14
     :cutoff-date "2025-08")
    (gpt-5.2
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.75
     :output-cost 14
     :cutoff-date "2025-08")
    (gpt-5.1
     :description "The best model for coding and agentic tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.25
     :output-cost 2.0
     :cutoff-date "2024-09")
    (gpt-5-nano
     :description "Fastest, cheapest version of GPT-5"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.05
     :output-cost 0.40
     :cutoff-date "2024-09")
    (gpt-5
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")
    (gpt-4.1-mini
     :description "Balance between intelligence, speed and cost"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1024
     :input-cost 0.4
     :output-cost 1.6)
    (gpt-4.1-nano
     :description "Fastest, most cost-effective GPT-4.1 model"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1024
     :input-cost 0.10
     :output-cost 0.40
     :cutoff-date "2024-05")
    (gpt-4.1
     :description "Flagship model for complex tasks"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1024
     :input-cost 2.0
     :output-cost 8.0
     :cutoff-date "2024-05")
    (gpt-4o-mini
     :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 0.15
     :output-cost 0.60
     :cutoff-date "2023-10")
    (gpt-4o
     :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
     :capabilities (media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 2.50
     :output-cost 10
     :cutoff-date "2023-10")
    (gpt-4.5-preview
     :description "DEPRECATED: Use gpt-4.1 instead"
     :capabilities (media tool-use url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 75
     :output-cost 150
     :cutoff-date "2023-10")
    (gpt-4-turbo
     :description "Previous high-intelligence model"
     :capabilities (media tool-use url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 128
     :input-cost 10
     :output-cost 30
     :cutoff-date "2023-11")
    (gpt-4
     :description "GPT-4 snapshot from June 2023 with improved function calling support"
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :capabilities (media url responses-api)
     :context-window 8.192
     :input-cost 30
     :output-cost 60
     :cutoff-date "2023-11")
    (o4-mini
     :description "Fast, effective reasoning with efficient performance in coding and visual tasks"
     :capabilities (reasoning media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 1.10
     :output-cost 4.40
     :cutoff-date "2024-05")
    (o3-mini
     :description "High intelligence at the same cost and latency targets of o1-mini"
     :context-window 200
     :input-cost 1.10
     :output-cost 4.40
     :cutoff-date "2023-10"
     :capabilities (reasoning tool-use json responses-api))
    (o3
     :description "Well-rounded and powerful model across domains"
     :capabilities (reasoning media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 2
     :output-cost 8
     :cutoff-date "2024-05")
    (o3-pro
     :description "Maximum performance model for reasoning tasks"
     :capabilities (reasoning media tool-use json url responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 20
     :output-cost 80
     :cutoff-date "2024-05")
    (o1-mini
     :description "Faster and cheaper reasoning model good at coding, math, and science"
     :context-window 128
     :input-cost 1.10
     :output-cost 4.40
     :cutoff-date "2023-10"
     :capabilities (nosystem reasoning responses-api))
    (o1
     :description "Reasoning model designed to solve hard problems across domains"
     :capabilities (media reasoning responses-api)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 200
     :input-cost 15
     :output-cost 60
     :cutoff-date "2023-10"))
  "List of available OpenAI models and associated properties.

Each model symbol is associated with the following keys, all optional:

- `:description': a brief description of the model.

- `:capabilities':  a list of capabilities supporte responses-apid by the model.

- `:mime-types': a list of supported MIME types for media files.

- `:context-window': the context window size, in thousands of tokens.

- `:input-cost': the input cost, in US dollars per million tokens.

- `:output-cost': the output cost, in US dollars per million tokens.

- `:cutoff-date': the knowledge cutoff date.

- `:request-params': a plist of additional request parameters to
  include when using this model.

Information about the OpenAI models was obtained from the following
sources:

- <https://platform.openai.com/docs/pricing>
- <https://platform.openai.com/docs/models>")

;;;###autoload
(cl-defun ai-workbench-make-openai
    (name &key curl-args (models ai-workbench--openai-models)
          stream key request-params
          (header
           (lambda (_info)
             (when-let* ((key (ai-workbench--get-api-key)))
               `(("Authorization" . ,(concat "Bearer " key))))))
          (host "api.openai.com")
          (protocol "https")
          endpoint)
  "Register an OpenAI API-compatible backend for ai-workbench-engine with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`ai-workbench--openai-models'.  An example of a model specification
including both kinds of specs:

:models
\\='(gpt-3.5-turbo                         ;Simple specs
  gpt-4-turbo
  (gpt-4o-mini                          ;Full spec
   :description
   \"Affordable and intelligent small model for lightweight tasks\"
   :capabilities (media tool json url)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\")))

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that returns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that ai-workbench-engine does not provide user options
for."
  (declare (indent 1))
  (let* ((responses-api (string-match-p "api\\.openai\\.com" host))
         ;; Use the OpenAI Responses API if required
         ;; TODO: Find a more reliable way to dispatch.  Checking the host isn't
         ;; reliable.  For example, it won't work when using the Responses API
         ;; via a proxy.
         (constructor (if (not responses-api)
                          #'ai-workbench--make-openai
                        (require 'ai-workbench-openai-responses)
                        #'ai-workbench--make-openai-responses))
         (endpoint (or endpoint
                       (if responses-api "/v1/responses"  "/v1/chat/completions")))
         (backend (funcall constructor
                           :curl-args curl-args
                           :name name
                           :host host
                           :header header
                           :key key
                           :models (ai-workbench--process-models models)
                           :protocol protocol
                           :endpoint endpoint
                           :stream stream
                           :request-params request-params
                           :url (if protocol
                                    (concat protocol "://" host endpoint)
                                  (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name ai-workbench--known-backends
                       nil nil #'equal)
                  backend))))

;;; Azure
;;;###autoload
(cl-defun ai-workbench-make-azure
    (name &key curl-args host
          (protocol "https")
          (header (lambda (_info) `(("api-key" . ,(ai-workbench--get-api-key)))))
          (key 'ai-workbench-api-key)
          models stream endpoint request-params)
  "Register an Azure backend for ai-workbench-engine with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the API host.

MODELS is a list of available model names, as symbols.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that ai-workbench-engine does not provide user options
for.

Example:
-------

 (ai-workbench-make-azure
  \"Azure-1\"
  :protocol \"https\"
  :host \"RESOURCE_NAME.openai.azure.com\"
  :endpoint
  \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
  :stream t
  :models \\='(gpt-3.5-turbo gpt-4))"
  (declare (indent 1))
  (let ((backend (ai-workbench--make-openai
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (ai-workbench--process-models models)
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :request-params request-params
                  :url (if protocol
                           (concat protocol "://" host endpoint)
                         (concat host endpoint)))))
    (prog1 backend
      (setf (alist-get name ai-workbench--known-backends
                       nil nil #'equal)
            backend))))

;; GPT4All
;;;###autoload
(defalias 'ai-workbench-make-gpt4all 'ai-workbench-make-openai
  "Register a GPT4All backend for ai-workbench-engine with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where GPT4All runs (with port), typically localhost:4891

MODELS is a list of available model names, as symbols.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v1/completions\"

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like GPT4All.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that ai-workbench-engine does not provide user options
for.

Example:
-------

(ai-workbench-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(mistral-7b-openorca.Q4_0.gguf))")

(provide 'ai-workbench-openai)
;;; ai-workbench-openai.el ends here

;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
