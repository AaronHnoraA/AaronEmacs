;;; ai-workbench-request.el --- LLM request library for ai-workbench-engine         -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur;; <karthikchikmagalur@gmail.com>
;; Keywords: convenience

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

;;; Commentary:

;; LLM querying library used by ai-workbench-engine.  This file provides the basic data
;; structures (models, backends) and prompt construction functions used by
;; ai-workbench-engine, along with the `ai-workbench-request' API.
;;
;; This is everything required to use `ai-workbench-request' to write custom commands,
;; UIs or packages that use ai-workbench-engine to implement custom workflows.  To use ai-workbench-engine's
;; LLM querying API, you can
;;
;; (require 'ai-workbench-request)
;;
;; and make calls to `ai-workbench-request'.
;;
;; Note that this file does not provide any of the UI components used by ai-workbench-engine
;; (chat buffers, tool use prompts, transient menus), nor does it provide the
;; default response callbacks used by `ai-workbench-request'.  You will need to provide
;; your own callback to `ai-workbench-request' to act on the LLM response, or require
;; the larger `ai-workbench-engine' feature.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'compat nil t)
(require 'cl-lib)
(require 'url)
(require 'text-property-search)
(require 'cl-generic)
(require 'map)
(require 'mailcap)                    ;FIXME Avoid this somehow

(declare-function json-read "json" ())
(defvar json-object-type)

(declare-function ai-workbench--stream-convert-markdown->org "ai-workbench-org")
(declare-function ai-workbench--convert-markdown->org "ai-workbench-org")
(declare-function ai-workbench-org--create-prompt-buffer "ai-workbench-org")
(declare-function ai-workbench-context--wrap "ai-workbench-context")
(declare-function ai-workbench--transform-apply-preset "ai-workbench-engine")
(declare-function ai-workbench--insert-response "ai-workbench-engine")
(declare-function ai-workbench-curl--stream-insert-response "ai-workbench-engine")
(declare-function ai-workbench-make-openai "ai-workbench-openai")


;;; User options
(defgroup ai-workbench-engine nil
  "Interact with LLMs from anywhere in Emacs."
  :group 'hypermedia)

(defcustom ai-workbench-proxy ""
  "Path to a proxy to use for ai-workbench-engine interactions.
Passed to curl via --proxy arg, for example \"proxy.yourorg.com:80\"
Leave it empty if you don't use a proxy."
  :type 'string)

(defcustom ai-workbench-api-key #'ai-workbench-api-key-from-auth-source
  "An API key (string) for the default LLM backend.

OpenAI by default.

Can also be a function of no arguments that returns an API
key (more secure) for the active backend."
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom ai-workbench-stream t
  "Stream responses from the LLM as they are received.

This option is ignored unless
- the LLM backend supports streaming, and
- Curl is in use (see `ai-workbench-use-curl')

When set to nil, Emacs waits for the full response and inserts it
all at once.  This wait is asynchronous.

\='tis a bit silly."
  :type 'boolean)

(defcustom ai-workbench-use-curl (and (executable-find "curl") t)
  "Whether ai-workbench-engine should prefer Curl when available.

Can be set to t, nil, or a string path to the curl executable."
  :type '(choice
          (const :tag "Do not use Curl" nil)
          (const :tag "Use Curl" t)
          (string :tag "Specify path to the Curl executable")))

(defcustom ai-workbench-org-convert-response t
  "Whether ai-workbench-engine should convert Markdown responses to Org markup.

This only affects requests originating from Org mode buffers."
  :type 'boolean)

(defcustom ai-workbench-curl-file-size-threshold
  (if (memq system-type '(windows-nt ms-dos)) #x6ffe 130000)
  "Size threshold for using file input with Curl.

Specifies the size threshold for when to use a temporary file to pass data to
Curl in ai-workbench-engine queries.  If the size of the data to be sent exceeds this
threshold, the data is written to a temporary file and passed to Curl using the
`--data-binary' option with a file reference.  Otherwise, the data is passed
directly as a command-line argument.

The value is an integer representing the number of bytes.

Adjusting this value may be necessary depending on the environment
and the typical size of the data being sent in ai-workbench-engine queries.
A larger value may improve performance by avoiding the overhead of creating
temporary files for small data payloads, while a smaller value may be needed
if the command-line argument size is limited by the operating system.

The default of #x8000 for windows comes from Microsoft documentation
located here:
https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessa

It is set to (#x8000 - #x1000 - 2) to account for other (non-data) Curl
command line arguments."
  :type 'natnum)

(define-obsolete-variable-alias 'ai-workbench-prompt-filter-hook
  'ai-workbench-prompt-transform-functions "0.9.9")

(defcustom ai-workbench-prompt-transform-functions
  '(ai-workbench--transform-apply-preset ai-workbench--transform-add-context)
  "Handlers to augment or transform a query before sending it.

This hook is called in a temporary buffer containing the text to
be sent, with the cursor at the end of the prompt.  You can use
it to modify the buffer or buffer-local variables as required.

Since these functions modify the prompt construction buffer, the order
in which they run is significant!  In particular, you may want to add
your function before (the default) or after
`ai-workbench--transform-add-context', which adds ai-workbench-engine's context (other
buffers, files etc) to this buffer.

Example: A typical use case might be to search for occurrences of $(cmd)
and replace it with the output of the shell command cmd, making it easy
to send the output of shell commands to the LLM.

Transform functions can be synchronous or asynchronous.

Synchronous hook functions must accept zero or one argument: the INFO
plist for the current request.

Asynchronous hook functions must accept two arguments: a callback to
call after the transformation is complete, and the INFO plist for the
current request.

Note that while this set of handlers can certainly be set with a global
value to be applied to all queries in all buffers, it meant to be set
locally for a specific buffer, or chat topic, or only the context of a
certain task."
  :type 'hook)

(defcustom ai-workbench-post-request-hook nil
  "Hook run after sending a ai-workbench-engine request.

This runs (possibly) before any response is received."
  :type 'hook)

;; TODO(v1.0): Remove this.
(defvar ai-workbench-response-filter-functions nil)
(make-obsolete-variable
 'ai-workbench-response-filter-functions
 "Response filtering is no longer supported in ai-workbench-engine.  To toggle
markdown to Org conversion, see `ai-workbench-org-convert-response'.  To
filter LLM response text, either use `ai-workbench-request' with a
custom callback, or use `ai-workbench-post-response-functions'."
 "0.9.7")

;; TODO: Handle `prog-mode' using the `comment-start' variable
(defcustom ai-workbench-prompt-prefix-alist
  '((markdown-mode . "### ")
    (org-mode . "*** ")
    (text-mode . "### "))
  "String used as a prefix to the query being sent to the LLM.

This is meant for the user to distinguish between queries and
responses, and is removed from the query before it is sent.

This is an alist mapping major modes to the prefix strings.  This
is only inserted in dedicated ai-workbench-engine buffers."
  :type '(alist :key-type symbol :value-type string))

(defcustom ai-workbench-response-prefix-alist
  '((markdown-mode . "")
    (org-mode . "")
    (text-mode . ""))
  "String inserted before the response from the LLM.

This is meant for the user to distinguish between queries and
responses.

This is an alist mapping major modes to the reply prefix strings.  This
is only inserted in dedicated ai-workbench-engine buffers before the AI's response."
  :type '(alist :key-type symbol :value-type string))

(defcustom ai-workbench-response-separator "\n\n"
  "String inserted before responses.

Also inserted before and after non-consecutive tool calls."
  :type 'string)

;; Model and interaction parameters
(defcustom ai-workbench-directives
  '((default     . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing     . "You are a large language model and a writing assistant. Respond concisely.")
    (chat        . "You are a large language model and a conversation partner. Respond concisely."))
  "System prompts or directives for the LLM.

Each entry in this alist maps a symbol naming the prompt/directive to
the directive itself.  By default, ai-workbench-engine uses the directive with the key
\\+`default'.

To set the system prompt for a chat session from this list
interactively, call `ai-workbench-send' with a prefix argument, or call
`ai-workbench-menu'.

A \"directive\" is typically the system prompt (also called system
message or system instruction) sent at the beginning of each request to
the LLM.  It is used to set general instructions, expectations and the
overall tone.

A directive in `ai-workbench-directives' can be a string (included as-is), a
list of strings (system prompt + conversation template) or a function
returning a string or list of strings.  See `ai-workbench-system-prompt' for
details."
  :safe #'always
  :type '(alist :key-type symbol :value-type string))

(define-obsolete-variable-alias 'ai-workbench--system-message 'ai-workbench-system-prompt
  "0.9.9.6")
(put 'ai-workbench--system-message 'safe-local-variable
     #'(lambda (v) (or (string-or-null-p v)
                  (and (listp v)
                       (cl-every #'string-or-null-p v)))))

(defcustom ai-workbench-system-prompt
  (or (alist-get 'default ai-workbench-directives)
      "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
  "System prompt used by ai-workbench-engine.

A system prompt is used to provide the LLM with general instructions,
set expectations, or establish an overall tone.  It is also commonly
referred to as the \"system message\" or \"directive\".

This is typically a string, but can be specified more generally as well.
It can be

- A string, included with ai-workbench-engine requests as-is.

- nil, to not include any system prompt.

- A list of strings, whose first (possibly nil) element is interpreted
  as the system prompt, and the remaining elements as (possibly nil)
  alternating user prompts and LLM responses.  This can be used to
  template the initial part of a conversation.

- A function of no arguments that returns a string or a list of strings,
  interpreted as the above.  When sending a request, this will be used
  to dynamically generate a system message and/or conversation template
  based on the current context.  See the definition of
  `ai-workbench--rewrite-directive-default' for an example.

If you alternate between a few different system prompts, you can save
them all in `ai-workbench-directives' and pick between them in `ai-workbench-menu'.
This way system prompts can be set globally, per-buffer or for the next
request only.

The system prompt is typically one part of the configuration required
for custom LLM tasks.  You can include system prompts along with other
settings in ai-workbench-engine presets (see `ai-workbench-make-preset') with the :system
key.

By default this system prompt is used when using ai-workbench-engine programmatically
with `ai-workbench-request'.  It can be overridden by supplying an explicit
`:system' argument to this function."
  :group 'ai-workbench-engine
  :type '(choice (string :tag "Prompt string")
                 (const :tag "No system prompt" nil)
                 (repeat :tag "Conversation template" string)
                 (function :tag "Dynamic prompt (function)"))
  :safe #'(lambda (v) (or (string-or-null-p v)
                     (and (listp v)
                          (cl-every #'string-or-null-p v)))))

(defcustom ai-workbench-max-tokens nil
  "Max tokens per response.

This is roughly the number of words in the response.  100-300 is a
reasonable range for short answers, 400 or more for longer
responses.

To set the target token count for a chat session interactively
call `ai-workbench-send' with a prefix argument."
  :safe #'always
  :type '(choice (natnum :tag "Specify Token count")
                 (const :tag "Default" nil)))

(defcustom ai-workbench-temperature nil
  "\"Temperature\" of the LLM response.

This is a number between 0.0 and 2.0 that controls the randomness of the
response, with 2.0 being the most random.  It can also be nil for the
LLM API's default value.

To set the temperature for a chat session interactively call
`ai-workbench-send' with a prefix argument."
  :safe (lambda (v) (or (null v) (numberp v)))
  :type '(choice (number :tag "Temperature value")
                 (const :tag "Use default" nil)))

(defcustom ai-workbench-cache nil
  "Whether the LLM should cache request content.

Some LLM backends can cache content sent to it by ai-workbench-engine, so that
only the newly included part of the text needs to be processed on
subsequent conversation turns.  This results in faster and
significantly cheaper processing.

NOTE: Manual or client-configurable caching is currently
supported only by the Anthropic API and thus the
`ai-workbench-anthropic' backend.  This variable has no effect on the
behavior of other backends.

This variable controls which parts of the query will be cached,
and can be the symbols t or nil to cache everything or nothing
respectively.  It can also be a list of symbols:

- message: Cache conversation messages
- system: Cache the system message
- tool: Cache tool definitions

Examples:

Setting it to (message system) will cache the system message and
the conversation text.

Setting it to (message system tool) will cache everything and is
the same as t."
  :type '(choice
          (const :tag "Cache everything" t)
          (const :tag "Do not cache" nil)
          (repeat symbol))
  :group 'ai-workbench-engine)

(defvar ai-workbench--known-backends nil
  "Alist of LLM backends known to ai-workbench-engine.

This is an alist mapping user-provided names to backend structs,
see `ai-workbench-backend'.

You can have more than one backend pointing to the same resource
with differing settings.")

(defvar ai-workbench--openai nil)
(make-obsolete-variable 'ai-workbench--openai "No longer used" "v0.9.9.5")

(defcustom ai-workbench-backend nil
  "LLM backend to use.

This is the default \"backend\" used by ai-workbench-engine, an object specifying
connection, authentication and model information required to send LLM
requests.

There are two ways to set `ai-workbench-backend':

1. with `setopt' or the Customize interface,
2. and via constructors (such as `ai-workbench-make-openai') in Elisp code.

When using `setopt' or the customize interface, a backend may be
specified in a list such as

  (BACKEND-TYPE NAME . PLIST)

where:

BACKEND-TYPE is one of `ai-workbench-openai' (all OpenAI-compatible
services), `ai-workbench-anthropic', `ai-workbench-gemini', `ai-workbench-ollama',
`ai-workbench-kagi', `ai-workbench--gh' (GitHub Copilot), `ai-workbench-bedrock' (AWS
Bedrock), `ai-workbench-perplexity', `ai-workbench-deepseek' or `ai-workbench-privategpt'.

NAME is a string, any backend name of your choosing.

PLIST is an optional plist specifying connection and authentication
information, with keys

:protocol       - \"http\" or \"https\"
:host           - Host, such as \"api.openai.com\" or \"localhost:1616\"
:endpoint       - connection endpoint, such as \"/v1/chat/completions\"
:header         - HTTP header to inclue with the request, a string
                  or function
:key            - API key, if required for authentication
                  String, symbol or function, see `ai-workbench-api-key'
:models         - List of supported models (symbols, see
                  `ai-workbench--openai-models' for an example)
:stream         - Whether to stream responses, boolean
:request-params - Additional parameters to send with backend queries, as
                  a plist.  This plist is converted to JSON when sending.
                  This is meant for request parameters that ai-workbench-engine does not
                  provide user options for.
:curl-args      - list of strings representing additional Curl arguments (if
                  `ai-workbench-use-curl' is set)

When using the OpenAI, Anthropic, Gemini, Kagi, Github Copilot,
Perplexity or Deepseek backends, all plist keys are optional.  For other
services, specifying some fields may be required.  Examples:

  (setopt ai-workbench-backend \\='(ai-workbench-openai \"OpenAI\"
                          :key ai-workbench-api-key-from-auth-source
                          :stream t))

  (setopt ai-workbench-backend \\='(ai-workbench-anthropic \"Claude\" :key \"sk-...\"))

  (setopt ai-workbench-backend \\='(ai-workbench-ollama \"Ollama\"
                          :host \"localhost:11434\"
                          :models (qwen3:4b llama3.1:8b)
                          :stream t))

This list of keys is non-exhaustive.  Some backends (such as
`ai-workbench-bedrock') recognize or require additional keys.  To see what
other keys are available, check the corresponding constructor (such as
`ai-workbench-make-bedrock').

When not using `setopt', backends for LLM providers (local or remote)
may be constructed and registered using one of the available backend
constructor functions:

- `ai-workbench-make-openai'
- `ai-workbench-make-anthropic'
- `ai-workbench-make-gemini'
- `ai-workbench-make-ollama'
- `ai-workbench-make-azure'
- `ai-workbench-make-gpt4all'
- `ai-workbench-make-kagi'
- `ai-workbench-make-privategpt'
- `ai-workbench-make-perplexity'
- `ai-workbench-make-deepseek'
- `ai-workbench-make-xai'
- `ai-workbench-make-gh-copilot'
- `ai-workbench-make-bedrock'

In addition, `ai-workbench-backend' can be assigned to them.  Examples:

  (setq ai-workbench-backend (ai-workbench-make-openai \"llamacpp\"
                        :host \"localhost:8080\"
                        :protocol \"http\"
                        :models \\='(gpt-oss-120b glm-4.7-flash)))

  (setq ai-workbench-backend (ai-workbench-make-gemini \"Gemini\"
                        :key ai-workbench-api-key :stream t))

  (setq ai-workbench-backend (ai-workbench-make-anthropic \"Claude-think\"
                        :key ai-workbench-api-key
                        :request-params
                        \\='(:thinking (:type \"enabled\" :budget_tokens 1024)
                          :max_tokens 2048)))

See their documentation for more information and the package README for
examples.  Once registered, backends may be retrieved using
`ai-workbench-get-backend' or switched to interactively from ai-workbench-engine's menu (see
`ai-workbench-menu')."
  :safe #'always
  :type
  (let ((types '( choice :tag "Type"
                  (const :tag "OpenAI compatible" ai-workbench-openai)
                  (const ai-workbench-anthropic)
                  (const ai-workbench-gemini)
                  (const ai-workbench-ollama)
                  (const ai-workbench-kagi)
                  (const :tag "GitHub Copilot" ai-workbench-gh)
                  (const ai-workbench-bedrock)
                  (const ai-workbench-perplexity)
                  (const ai-workbench-deepseek)
                  (const ai-workbench-privategpt))))
    `(choice
      (restricted-sexp :match-alternatives (ai-workbench-backend-p 'nil)
                       :tag "No backend")
      (cons :tag "(BACKEND-TYPE NAME . PLIST)" ;accommodate (ai-workbench-openai "chatgpt" . plist)
            ,types (cons string
                         (plist :value-type (choice string symbol function
                                                    (repeat symbol)))))
      (cons :tag "(BACKEND-TYPE . PLIST)" ;accommodate (ai-workbench-openai :name "chatgpt" . plist)
            ,types (plist :value-type (choice string symbol function
                                              (repeat symbol))))))
  :get
  (lambda (sym)
    (when-let* ((backend (default-toplevel-value sym))
                (type (type-of backend))
                (plist (list :protocol (ai-workbench-backend-protocol backend)
                             :host (ai-workbench-backend-host backend)
                             :endpoint (ai-workbench-backend-endpoint backend)
                             :header (ai-workbench-backend-header backend)
                             :key (ai-workbench-backend-key backend)
                             :models (ai-workbench-backend-models backend)
                             :stream (ai-workbench-backend-stream backend)
                             :curl-args (ai-workbench-backend-curl-args backend)
                             :request-params (ai-workbench-backend-request-params backend))))
      (apply #'list type (ai-workbench-backend-name backend)
             (cl-loop for (k v) on plist by #'cddr
                      if (and (readablep v) (not (null v)))
                      collect k and collect v))))
  :set
  (lambda (sym val)
    (cond
     ((null val) (set-default-toplevel-value sym val))
     ((listp val)
      (let* ((name (if (stringp (cadr val)) ;explicit and implicit :name specification
                       (cadr val) (plist-get (cdr val) :name)))
             (args (if name (cddr val) (cdr val)))
             type)
        (cl-remf args :name)
        (if (memq (car val) '(ai-workbench-gh ai-workbench--gh))
            (setq type 'ai-workbench-gh-copilot)
          (setq type (car val)))
        (set-default-toplevel-value
         sym (apply (intern (concat "ai-workbench-make-"
                                    (substring (symbol-name type) 6)))
                    name args))))
     ((ai-workbench-backend-p val) (set-default-toplevel-value sym val)))))

(defcustom ai-workbench-model nil
  (concat
   "Model for ai-workbench-engine queries.

The name of the model, as a symbol.  This is the name as expected
by the LLM provider's API.

To set the model for a chat session interactively call
`ai-workbench-send' with a prefix argument.")
  :safe #'always
  :type `(choice
	  (symbol :tag "Specify model name")
	  ,@(cl-loop
             for (_name . backend) in ai-workbench--known-backends
             append (mapcar
                     (lambda (model) (list 'const :tag (symbol-name model)
			              model))
                     (ai-workbench-backend-models backend)))))

(defvar ai-workbench-expert-commands nil
  "Whether experimental ai-workbench-engine options should be enabled.

This opens up advanced options in `ai-workbench-menu'.")

(defvar ai-workbench--num-messages-to-send nil)
(put 'ai-workbench--num-messages-to-send 'safe-local-variable #'integer-or-null-p)

(defcustom ai-workbench-log-level nil
  "Logging level for ai-workbench-engine.

This is one of nil or the symbols info and debug:

nil: Don't log responses
info: Log request and response bodies
debug: Log request/response bodies, headers and all other
       connection settings.

When non-nil, information is logged to `ai-workbench--log-buffer-name',
which see."
  :type '(choice
          (const :tag "No logging" nil)
          (const :tag "Limited" info)
          (const :tag "Full" debug)))

(defcustom ai-workbench-track-response t
  "Distinguish between user messages and LLM responses.

When creating a prompt to send to the LLM, ai-workbench-engine distinguishes
between text entered by the user and past LLM responses.  This
distinction is necessary for back-and-forth conversation with an
LLM.

In regular Emacs buffers you can turn this behavior off by
setting `ai-workbench-track-response' to nil.  All text, including
past LLM responses, is then treated as user input when sending
queries.

This variable has no effect in dedicated chat buffers (buffers
with `ai-workbench-mode' enabled), where user prompts and responses are
always handled separately."
  :type 'boolean)

(defcustom ai-workbench-track-media nil
  "Whether links to supported media types should be followed.

When this is non-nil, ai-workbench-engine will send text, images or other media from
links in Org and Markdown buffers to the LLM.

Sending images or other binary media from links requires the
active `ai-workbench-model' to support it.  See `ai-workbench-make-openai',
`ai-workbench-make-anthropic', `ai-workbench-make-ollama' or `ai-workbench-make-gemini' for
details on how to specify media support for models.

To include media (including binary formats like images) more generally,
you can also use `ai-workbench-add' or `ai-workbench-add-file' instead."
  :type 'boolean)

(defcustom ai-workbench-use-context 'system
  "Where in the request to inject ai-workbench-engine's additional context.

ai-workbench-engine always includes the active region or the buffer up to the
cursor in the request to the LLM.  Additionally, you can add
other buffers or their regions to the context with
`ai-workbench-add-context', or from ai-workbench-engine's menu.  This data will be
sent with every request.

This option controls whether and where this additional context is
included in the request.

Currently supported options are:

    nil     - Do not use the context.
    system  - Include the context with the system message.
    user    - Include the context with the user prompt."
  :group 'ai-workbench-engine
  :type '(choice
          (const :tag "Don't include context" nil)
          (const :tag "With system message" system)
          (const :tag "With user prompt" user)))

(defcustom ai-workbench-include-reasoning 'ignore
  "How to handle LLM reasoning or \"thinking\" text blocks.

Some LLMs include in their response a \"thinking\" section.  This
text improves the quality of the LLM's final output, but may not
be interesting to you by itself.

Supported options are the symbols

    ignore  - Include in the response but ignore on subsequent
              conversation turns (default)
    t       - Include in the response
    nil     - Do not include

It can also be a string naming a buffer, in which case the
reasoning text will be inserted at the end of that buffer."
  :group 'ai-workbench-engine
  :type '(choice
          (const :tag "Include with response" t)
          (const :tag "Don't include" nil)
          (const :tag "Include but ignore" ignore)
          (string :tag "Include in buffer")))

(define-obsolete-variable-alias 'ai-workbench-context--alist 'ai-workbench-context
  "0.9.9.3")

(defcustom ai-workbench-context nil
  "List of ai-workbench-engine's context sources.

The items in this list (file names or buffers) are included with ai-workbench-engine
queries as additional context.

Each entry can be a file path (string) or a buffer (object, not buffer
name):

 \\='(\"~/path/to/file1\"
   \"./file2\"
   #<buffer *scratch*>
   ...)

The above covers the most common cases.  You can also specify context
sources in a more targeted way, with entries of the form

  (<buffer> . spec)
  (\"/path/to/file\" . spec)

where spec is a plist declaring specific parts of the buffer/file to
include instead of the entire text.

For buffers, you can specify regions to include using buffer spans and
line number ranges as conses, and overlays as a list:

  (<buffer> :bounds ((start1 . end1) (start2 . end2) ...)
            :lines  ((from1 . to1) (from2 . end2) ...)
            :overlays (ov1 ov2 ...))

For files, spec can include buffer spans and line number ranges, as well as
the MIME type of the file:

  (\"/path/to/file\" :bounds ((start1 . end1) (start2 . end2) ...)
                   :lines  ((from1 . to1) (from2 . end2) ...)
                   :mime \"image/png\")

ai-workbench-engine tries to guess file MIME types, but is not always successful, so
it is recommended to provide it with non-text files.

Usage of context commands (such as `ai-workbench-add' and `ai-workbench-add-file')
will modify this variable.  You can also set this variable
buffer-locally, or let-bind it around calls to ai-workbench-engine queries, or via
ai-workbench-engine presets with the :context key."
  :type '(repeat string))

(defcustom ai-workbench-markdown-validate-link #'always
  "Validate links to be sent as context with ai-workbench-engine queries.

When `ai-workbench-track-media' is enabled, this option determines if a
supported link will be followed and its source included with ai-workbench-engine
queries from Markdown buffers.  Currently only links to files are
supported (along with web URLs if the model supports them).

It should be a function that accepts a Markdown link and return non-nil
if the link should be followed.  See `markdown-link-at-pos' for the
structure of a Markdown link object.

By default, all links are considered valid.

Set this to `ai-workbench--link-standalone-p' to only follow links placed on a
line by themselves, separated from surrounding text."
  :type '(choice
          (const :tag "All links" always)
          (const :tag "Standalone links" ai-workbench--link-standalone-p)
          (function :tag "Function"))
  :group 'ai-workbench-engine)

(defvar ai-workbench--request-alist nil
  "Alist of active ai-workbench-engine requests.
Each entry has the form (PROCESS . (FSM ABORT-CLOSURE))
If the ABORT-CLOSURE is called, it must abort the PROCESS.")

(defvar ai-workbench--request-params nil
  "Extra parameters sent with each ai-workbench-engine request.

These parameters are combined with model-specific and backend-specific
:request-params before sending a request, which see.  Warning: values
incompatible with the active backend can break ai-workbench-engine.  Do not use this
variable unless you know what you're doing!")

(defconst ai-workbench--ersatz-json-tool "response_json"
  "Name of ersatz tool used to force JSON output.

Some APIs, like Anthropic, use a tool to produce structured JSON output.")

(defcustom ai-workbench-curl-extra-args nil
  "Extra arguments to pass to Curl when sending queries.

This should be a list of strings, each one a Curl command line
argument.  Note that these should not conflict with the options
in `ai-workbench-curl--common-args', which ai-workbench-engine requires for correct
functioning.

If you want to specify extra arguments only when using a specific
ai-workbench-engine backend, use the `:curl-args' slot of the backend instead.
See `ai-workbench-backend'."
  :group 'ai-workbench-engine
  :type '(repeat string))

(defconst ai-workbench-curl--common-args
  (cond
   ((memq system-type '(windows-nt ms-dos))
    '("--disable" "--location" "--silent" "-XPOST"
      "-y7200" "-Y1" "-N" "-D-"))
   ((eq system-type 'darwin)
    '("--disable" "--location" "--silent" "--compressed"
      "-XPOST" "-y7200" "-Y1" "-N" "-D-"))
   (t
    '("--disable" "--location" "--silent" "--compressed"
      "-XPOST" "-y7200" "-Y1" "-N" "-D-")))
  "Arguments always passed to Curl for ai-workbench-engine queries.")

(defvar ai-workbench--link-type-cache nil
  "Cache of checks for binary files.

Each alist entry maps an absolute file path to a cons cell of the
form (t . binaryp), where binaryp is non-nil if the file is
binary-encoded.")

;; The following is derived from:
;;
;; (concat "\\(?:" markdown-regex-link-inline "\\|" markdown-regex-angle-uri "\\)")
;;
;; Since we want this known at compile time, when markdown-mode is not
;; guaranteed to be available, we have to hardcode it.
(defconst ai-workbench-markdown--link-regex
  "\\(?:\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\s-*\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\s-*\\(?8:)\\)\\|\\(<\\)\\([a-z][a-z0-9.+-]\\{1,31\\}:[^]	\n<>,;()]+\\)\\(>\\)\\)"
  "Link regex for `ai-workbench-mode' in Markdown mode.")

(defvar ai-workbench--mode-description-alist
  '((js2-mode      . "Javascript")
    (sh-mode       . "Shell")
    (enh-ruby-mode . "Ruby")
    (yaml-mode     . "Yaml")
    (yaml-ts-mode  . "Yaml")
    (rustic-mode   . "Rust")
    (tuareg-mode   . "OCaml"))
  "Mapping from unconventionally named major modes to languages.

This is used when generating system prompts for rewriting and
when including context from these major modes.")


;;; Utility functions

;;;; JSON parsing helpers
;; JSON conversion semantics used by ai-workbench-engine
;; empty object "{}" => empty list '() == nil
;; null              => :null
;; false             => :json-false

;; TODO(tool) Except when reading JSON from a string, where null => nil

(defmacro ai-workbench--json-read ()
  "Parse JSON at point in buffer."
  (if (fboundp 'json-parse-buffer)
      `(json-parse-buffer
        :object-type 'plist
        :null-object :null
        :false-object :json-false)
    (require 'json)
    (defvar json-object-type)
    (defvar json-null)
    (declare-function json-read "json" ())
    `(let ((json-object-type 'plist)
           (json-null :null))
       (json-read))))

(defmacro ai-workbench--json-read-string (str)
  "Pasre JSON string STR."
  (if (fboundp 'json-parse-string)
      `(json-parse-string ,str
        :object-type 'plist
        :null-object nil
        :false-object :json-false)
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read-from-string "json" ())
    `(let ((json-object-type 'plist))
      (json-read-from-string ,str))))

(defmacro ai-workbench--json-encode (object)
  "Serialize OBJECT as JSON."
  (if (fboundp 'json-serialize)
      `(json-serialize ,object
        :null-object :null
        :false-object :json-false)
    (require 'json)
    (defvar json-false)
    (defvar json-null)
    (declare-function json-encode "json" (object))
    `(let ((json-false :json-false)
           (json-null  :null))
      (json-encode ,object))))

(defmacro ai-workbench--maybe-funcall (func-or-sym &rest args)
  "If FUNC-OR-SYM is a function, call it with ARGS.

Otherwise, evaluate it as a variable."
  `(if (functionp ,func-or-sym)
       ;; TODO(v1.0) Remove this condition-case.  This arity check is for
       ;; benefit of users who have personal customizations touching ai-workbench-engine's
       ;; internal API re: backend header and url functions.
       (condition-case nil
           (apply ,func-or-sym (list ,@args))
         (wrong-number-of-arguments
          (message "Displaying warning")
          (display-warning
           'ai-workbench-engine (format "%s calling convention has changed: \
Called with %d arguments but accept %d.  \
Please update them, and see NEWS (0.9.9.5) for details."
                          (if (symbolp ,func-or-sym) (format "Function %s" ,func-or-sym)
                            "ai-workbench-backend-header/ai-workbench-backend-url function")
                          (length ',args) (car (func-arity ,func-or-sym))))
          (funcall ,func-or-sym)))
     ,func-or-sym))

(defun ai-workbench--process-models (models)
  "Convert items in MODELS to symbols with appropriate properties."
  (let ((models-processed))
    (dolist (model models)
      (cl-etypecase model
        (string (push (intern model) models-processed))
        (symbol (push model models-processed))
        (cons
         (cl-destructuring-bind (name . props) model
           (setf (symbol-plist name)
                 ;; MAYBE: Merging existing symbol plists is safer, but makes it
                 ;; difficult to reset a symbol plist, since removing keys from
                 ;; it (as opposed to setting them to nil) is more work.
                 ;;
                 ;; (map-merge 'plist (symbol-plist name) props)
                 props)
           (push name models-processed)))))
    (nreverse models-processed)))

;;;; Backend interface
(defun ai-workbench-get-backend (name)
  "Return ai-workbench-engine backend with NAME.

Throw an error if there is no match."
  (or (alist-get name ai-workbench--known-backends nil nil #'equal)
      (user-error "Backend %s is not known to be defined"
                  name)))

(gv-define-setter ai-workbench-get-backend (val name)
  `(setf (alist-get ,name ai-workbench--known-backends
          nil t #'equal)
    ,val))

(cl-defstruct
    (ai-workbench-backend (:constructor ai-workbench--make-backend)
                   (:copier ai-workbench--copy-backend))
  name host header protocol stream
  endpoint key models url request-params
  curl-args
  (coding-system
   nil :documentation "Can be set to `binary' if the backend expects non UTF-8 output."))

;;;; Misc utilities
(defun ai-workbench-api-key-from-auth-source (&optional host user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let* ((secret
             (plist-get
              (car (auth-source-search
                    :host (or host (ai-workbench-backend-host ai-workbench-backend))
                    :user (or user "apikey")
                    :require '(:secret)))
              :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `ai-workbench-api-key' found in the auth source")))

;; FIXME Should we utf-8 encode the api-key here?
(defun ai-workbench--get-api-key (&optional key)
  "Get api key from KEY, or from `ai-workbench-api-key'."
  (when-let* ((key-sym (or key (ai-workbench-backend-key ai-workbench-backend))))
    (cl-typecase key-sym
      (function (string-trim-right (funcall key-sym) "[\n\r]+"))
      (string (string-trim-right key-sym "[\n\r]+"))
      (symbol (if-let* ((val (symbol-value key-sym)))
                  (ai-workbench--get-api-key val)
                (error "`ai-workbench-api-key' is not valid")))
      (t (error "`ai-workbench-api-key' is not valid")))))

(defsubst ai-workbench--to-number (val)
  "Ensure VAL is a number."
  (cond
   ((numberp val) val)
   ((stringp val) (string-to-number val))
   ((error "%S cannot be converted to a number" val))))

(defsubst ai-workbench--to-string (s)
  "Convert S to a string, if possible."
  (cl-typecase s
    (symbol (symbol-name s))
    (string s)
    (otherwise (prin1-to-string s))))

(defsubst ai-workbench--intern (s)
  "Intern S, if possible."
  (cl-etypecase s
    (symbol s)
    (string (intern s))))

(defun ai-workbench--merge-plists (&rest plists)
  "Merge PLISTS, altering the first one.

Later plists in the sequence take precedence over earlier ones."
  (let (;; (rtn (copy-sequence (pop plists)))
        (rtn (pop plists))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

;; MAYBE: Can be generalized to ai-workbench--combine-plists, taking a "combiner"
;; function and default-value as arguments.
(defun ai-workbench--sum-plists (&rest plists)
  "Sum the values of keys across PLISTS.

All values must be numeric or nil.  Returns a new plist."
  (let ((rtn (copy-sequence (pop plists)))
        k v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq k (pop ls) v (pop ls))
        (setq rtn (plist-put rtn k (+ (or (plist-get rtn k) 0)
                                      (or v 0))))))
    rtn))

(defun ai-workbench--file-binary-p (path)
  "Check if file at PATH is readable and binary."
  ;; HACK Image files with ICC color profiles are characterized as ASCII
  ;; (#1223), so until we find a better solution we just match these files by
  ;; extension.
  (or (string-match-p "\\.\\(jpe?g\\|png\\|gif\\|webp\\)\\'" path)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path nil 1 512 'replace)
            (memq buffer-file-coding-system
                  '(no-conversion no-conversion-multibyte)))
        (file-missing (message "File \"%s\" is not readable." path)
                      nil))))

(defun ai-workbench--insert-file-string (path)
  "Insert at point the contents of the file at PATH as context."
  (insert (format "In file `%s`:" (abbreviate-file-name path))
          "\n\n```\n")
  (let ((pm (point-marker)))
    (set-marker-insertion-type pm t)
    (insert-file-contents path)
    (goto-char pm))
  (insert "\n```\n"))

(defun ai-workbench--strip-mode-suffix (mode-sym)
  "Remove the -mode suffix from MODE-SYM.

MODE-SYM is typically a major-mode symbol."
  (or (alist-get mode-sym ai-workbench--mode-description-alist)
      (let ((mode-name (thread-last
                         (symbol-name mode-sym)
                         (string-remove-suffix "-mode")
                         (string-remove-suffix "-ts"))))
        ;; NOTE: The advertised calling convention of provided-mode-derived-p
        ;; has changed in Emacs 30, this needs to be updated eventually
        (if (provided-mode-derived-p
             mode-sym 'prog-mode 'text-mode 'tex-mode)
            mode-name ""))))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
;; TODO: Handle and return HTTP errors
(cl-defun ai-workbench--url-retrieve (url &key method data headers
                                   (content-type "application/json"))
  "Retrieve URL synchronously with METHOD, DATA and HEADERS."
  (declare (indent 1))
  (let ((url-request-method (if (eq method 'post) "POST" "GET"))
        (url-request-data
         (when (eq method 'post)
           (encode-coding-string
            (pcase content-type
              ("application/json" (ai-workbench--json-encode data))
              (_ data))
            'utf-8)))
        (url-mime-accept-string "application/json")
        (url-request-extra-headers
         `(("content-type" . ,content-type) ,@headers)))
    (with-current-buffer (url-retrieve-synchronously url 'silent)
      (goto-char url-http-end-of-headers)
      (ai-workbench--json-read))))

(defsubst ai-workbench-prompt-prefix-string ()
  "Prefix before user prompts in `ai-workbench-mode'."
  (declare (side-effect-free t))
  (or (alist-get major-mode ai-workbench-prompt-prefix-alist) ""))

(defsubst ai-workbench-response-prefix-string ()
  "Prefix before LLM responses in `ai-workbench-mode'."
  (declare (side-effect-free t))
  (or (alist-get major-mode ai-workbench-response-prefix-alist) ""))

(defmacro ai-workbench--at-word-end (&rest body)
  "Execute BODY at end of the current word or punctuation."
  `(save-excursion
     (skip-syntax-forward "w.")
     ,(macroexp-progn body)))

;; NOTE: Remove after we drop Emacs 27.1 (#724)
(defmacro ai-workbench--temp-buffer (buf)
  "Generate a temp buffer BUF.

Compatibility macro for Emacs 27.1."
  (if (< emacs-major-version 28)
      `(generate-new-buffer ,buf)
    `(generate-new-buffer ,buf t)))

;; This is defined in ai-workbench-engine, but we define it here as well as it's required by
;; `ai-workbench--with-buffer-copy'.
(defvar ai-workbench-mode nil)

(defmacro ai-workbench--with-buffer-copy (buf start end &rest body)
  "Copy ai-workbench-engine's local variables from BUF to a temp buffer and run BODY.

If positions START and END are provided, insert that part of BUF first."
  (declare (indent 3))
  `(ai-workbench--with-buffer-copy-internal ,buf ,start ,end (lambda () ,@body)))

(defun ai-workbench--with-buffer-copy-internal (buf start end body-thunk)
  "Prepare a temp buffer for a ai-workbench-engine request.

For BUF, START, END and BODY-THUNK see `ai-workbench--with-buffer-copy'."
  (let ((temp-buffer (ai-workbench--temp-buffer " *ai-workbench-prompt*")))
    (with-current-buffer temp-buffer
      (dolist (sym '( ai-workbench-backend ai-workbench-system-prompt ai-workbench-model
                      ai-workbench-mode ai-workbench-track-response ai-workbench-track-media
                      ai-workbench-use-tools ai-workbench-llm-tools ai-workbench-use-curl ai-workbench--schema
                      ai-workbench-use-context ai-workbench-context ai-workbench--num-messages-to-send
                      ai-workbench-stream ai-workbench-include-reasoning ai-workbench--request-params
                      ai-workbench-temperature ai-workbench-max-tokens ai-workbench-cache))
        (set (make-local-variable sym) (buffer-local-value sym buf)))
      (when (and start end) (insert-buffer-substring buf start end))
      (setq major-mode (buffer-local-value 'major-mode buf))
      (funcall body-thunk))))

(defsubst ai-workbench--trim-prefixes (s)
  "Remove prompt/response prefixes from string S.

Return nil if string collapses to empty string."
  (let* ((trimmed (string-trim-left
                   s (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                             (regexp-quote
                              (ai-workbench-prompt-prefix-string)))))
         (trimmed (string-trim-right
                   trimmed (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                                   (regexp-quote
                                    (ai-workbench-response-prefix-string))))))
    (unless (string-empty-p trimmed)
      trimmed)))

(defun ai-workbench--link-standalone-p (link)
  "Return non-nil if Markdown LINK is isolated.

This means the extent from the link beginning to end is the only
non-whitespace content on its line."
  (let ((beg (car link)) (end (cadr link)))
    (save-excursion
      (and (= beg (progn (goto-char beg) (beginning-of-line)
                         (skip-chars-forward "\t ")
                         (point)))
           (= end (progn (goto-char end) (end-of-line)
                         (skip-chars-backward "\t ")
                         (point)))))))

(defsubst ai-workbench--curl-path ()
  "Curl executable to use."
  (if (stringp ai-workbench-use-curl) ai-workbench-use-curl "curl"))

(defun ai-workbench--transform-add-context (callback fsm)
  (if (and ai-workbench-use-context ai-workbench-context)
      (ai-workbench-context--wrap callback (plist-get (ai-workbench-fsm-info fsm) :data))
    (funcall callback)))

;;;; Model interface
;; NOTE: This interface would be simpler to implement as a defstruct.  But then
;; users cannot set `ai-workbench-model' to a symbol/string directly, or we'd need
;; another map from these symbols to the actual model structs.

(defsubst ai-workbench--model-name (model)
  "Get name of ai-workbench-engine MODEL."
  (ai-workbench--to-string model))

(defsubst ai-workbench--model-capabilities (model)
  "Get MODEL capabilities."
  (get model :capabilities))

(defsubst ai-workbench--model-mimes (model)
  "Get supported mime-types for MODEL."
  (get model :mime-types))

(defsubst ai-workbench--model-capable-p (cap &optional model)
  "Return non-nil if MODEL supports capability CAP."
  (memq cap (ai-workbench--model-capabilities
             (or model ai-workbench-model))))

;; TODO Handle model mime specifications like "image/*"
(defsubst ai-workbench--model-mime-capable-p (mime &optional model)
  "Return non nil if MODEL can understand MIME type."
  (car-safe (member mime (ai-workbench--model-mimes
                          (or model ai-workbench-model)))))

(defsubst ai-workbench--model-request-params (model)
  "Get model-specific request parameters for MODEL."
  (get model :request-params))

;;;; File handling
(defun ai-workbench--base64-encode (file)
  "Encode FILE as a base64 string.

FILE is assumed to exist and be a regular file."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (base64-encode-region (point-min) (point-max)
                          :no-line-break)
    (buffer-string)))

;;;; Directive handling

(defun ai-workbench--describe-directive (directive width &optional replacement)
  "Find description for DIRECTIVE, truncated  to WIDTH.

DIRECTIVE is a ai-workbench-engine directive, and can be a string, a function
or a list of strings.  See `ai-workbench-directives'.

The result is a string intended for display.  Newlines are
replaced with REPLACEMENT."
  (cl-typecase directive
    (string
     (string-replace
      "\n" (or replacement " ")
      (substring directive 0 (min width (length directive)))))
    (function
     (concat
      "λ: "
      (string-replace
       "\n" (or replacement " ")
       (truncate-string-to-width
        (or (and-let* ((doc (documentation directive)))
              (substring doc nil (string-match-p "\n" doc)))
            "[Dynamically generated; no preview available]")
        width nil nil t))))
    (list (and-let* ((from-template (car directive)))
            (ai-workbench--describe-directive
             from-template width)))
    (t "")))

(defun ai-workbench--parse-directive (directive &optional raw)
  "Parse DIRECTIVE into a backend-appropriate form.

DIRECTIVE is a ai-workbench-engine directive: it can be a string, a list or a
function that returns either, see `ai-workbench-directives'.

Return a cons cell consisting of the system message (a string)
and a template consisting of alternating user/LLM
records (a list of strings or nil).

If RAW is non-nil, the user/LLM records are not processed and are
returned as a list of strings."
  (and directive
       (cl-etypecase directive
         (string   (list directive))
         (function (ai-workbench--parse-directive (funcall directive) raw))
         (cons     (if raw directive
                     (cons (car directive)
                           ;; FIXME(augment) do this elsewhere
                           (ai-workbench--parse-list
                            ai-workbench-backend (cdr directive))))))))


;;; Logging

(defconst ai-workbench--log-buffer-name "*ai-workbench-log*"
  "Log buffer for ai-workbench-engine.")

(declare-function json-pretty-print "json")

(defun ai-workbench--log (data &optional type no-json)
  "Log DATA to `ai-workbench--log-buffer-name'.

TYPE is a label for data being logged.  DATA is assumed to be
Valid JSON unless NO-JSON is t."
  (with-current-buffer (get-buffer-create ai-workbench--log-buffer-name)
    (let ((p (goto-char (point-max))))
      (unless (bobp) (insert "\n"))
      (insert (format "{\"ai-workbench-engine\": \"%s\", " (or type "none"))
              (format-time-string "\"timestamp\": \"%Y-%m-%d %H:%M:%S\"}\n")
              data)
      (unless no-json (ignore-errors (json-pretty-print p (point)))))))


;;; Structured output
(defvar ai-workbench--schema nil
  "Response output schema for backends that support it.")

(cl-defgeneric ai-workbench--parse-schema (_backend _schema)
  "Parse JSON schema in a backend-appropriate way.")

(defun ai-workbench--dispatch-schema-type (schema)
  "Convert SCHEMA to a valid elisp representation.

SCHEMA can be specified in several ways:
- As a plist readable by `ai-workbench--json-encode'
  Ex: (:type object :properties (:key1 (:type number :description \"...\")
                                 :key2 (:type string)))

- As a serialized JSON string, which will be passed as-is.

- In shorthand form #1, a single-line comma-separated string with object
  keys and (optionally) types:
  Ex: \"key1, key2 number\"
  Ex: \"key1 string, key2 int\"
  The default type is string, and types can be shortened (integer -> int) as
  long as they match a JSON schema type uniquely.

- In shorthand form #2, a multi-line string with keys, (optionally) types and
  (optionally) descriptions
  Ex: \"key1: description 1 here
       key2 integer: description 2 here\"

- Shorthand forms can be placed inside [ and ] to specify an array of
  objects:
  Ex: \"[key1, key2 number]\"
  Ex: \"[key1: description 1 here
        key2 int: description 2 here]\""
  (when (stringp schema)  ;Two possibilities: serialized JSON, or shorthand form
    (let (wrap-in-array)  ;Flag to wrap the object type in an array
      (with-temp-buffer   ;Parser for (possibly) shorthand forms
        (insert schema)
        (goto-char (point-min)) (skip-chars-forward " \n\r\t")
        (if (= (char-after) ?{)
            (setq schema (ai-workbench--json-read)) ;Assume serialized JSON schema, we're done
          (when (= (char-after) ?\[)    ;Shorthand: assume array top-level type
            (save-excursion
              (goto-char (point-max)) (skip-chars-backward " \n\r\t") (delete-char -1))
            (delete-char 1)             ;Delete array markers [ and ]
            (setq wrap-in-array t))
          (let ( props types descriptions ;Nested object and array types are disallowed in shorthand
                 (all-types '("number" "string" "integer" "boolean" "null")))
            (if (= (point-max) (line-end-position)) ; Single or multi-line?
                ;; Single line format (type optional): "key1 type, key2, ..."
                (while (re-search-forward ",?\\([^ ,]+\\) *\\([^ ,]*\\]?\\)" nil t)
                  (push (match-string 1) props)
                  (push (if (string-empty-p (match-string 2))
                            "string" (car (all-completions (match-string 2) all-types)))
                        types)
                  (push nil descriptions))
              ;; Multi-line format (type, description optional):
              ;; "key1 type: description1 \n key2: description2..."
              (while (re-search-forward "\\([^ :]+\\) *\\([^ :]*\\):?"
                                        (line-end-position) t)
                (push (match-string 1) props)
                (push (if (string-empty-p (match-string 2))
                          "string" (car (all-completions (match-string 2) all-types)))
                      types)
                (skip-chars-forward " \t")
                (push (if (eolp) nil (buffer-substring-no-properties
                                      (point) (line-end-position)))
                      descriptions)
                (forward-line 1)))
            (let ((object
                   (list :type "object"
                         :properties
                         (cl-mapcan
                          (lambda (prop type desc)
                            `(,(intern (concat ":" prop))
                              (:type ,type ,@(when desc
                                               (list :description (string-trim desc))))))
                          (nreverse props) (nreverse types) (nreverse descriptions)))))
              (setq schema
                    (if wrap-in-array (list :type "array" :items object) object))))))))
  ;; The OpenAI and Anthropic APIs don't allow arrays at the root of the schema.
  ;; Work around this by wrapping it in an object with the field "items".
  ;; TODO(schema): Find some way to strip this extra layer from the response.
  (if (member (plist-get schema :type) '("array" array))
      (list :type "object"
            :properties (list :items schema)
            :required ["items"]
            :additionalProperties :json-false)
    schema))

(defun ai-workbench--preprocess-schema (spec)
  "Set additionalProperties for objects in SPEC destructively.

Convert symbol :types to strings."
  ;; NOTE: Do not use `sequencep' here, as that covers strings too and breaks
  ;; things.
  (when (or (listp spec) (vectorp spec))
    (cond
     ((vectorp spec)
      (cl-loop for element across spec
               for idx upfrom 0
               do (aset spec idx (ai-workbench--preprocess-schema element))))
     ((keywordp (car spec))
      (let ((tail spec))
        (while tail
          (when (eq (car tail) :type)
            (when (symbolp (cadr tail)) ;Convert symbol :type to string
              (setcar (cdr tail) (symbol-name (cadr tail))))
            (when (equal (cadr tail) "object") ;Add additional object fields
              (plist-put tail :additionalProperties :json-false)
              (let ((vprops (vconcat
                             (cl-loop
                              for prop in (plist-get tail :properties) by #'cddr
                              collect (substring (symbol-name prop) 1)))))
                (plist-put tail :required vprops)
                (plist-put tail :propertyOrdering vprops))))
          (when (or (listp (cadr tail)) (vectorp (cadr tail)))
            (ai-workbench--preprocess-schema (cadr tail)))
          (setq tail (cddr tail)))))
     ((listp spec) (dolist (element spec)
                     (when (listp element)
                       (ai-workbench--preprocess-schema element))))))
  spec)


;;; Tool use

(defcustom ai-workbench-use-tools t
  "Whether ai-workbench-engine should use tools.

Tools are capabilities provided by you to the LLM as functions an
LLM can choose to call.  ai-workbench-engine runs the function call on your
machine.

If set to t, any tools selected in variable `ai-workbench-llm-tools' will be made
available to the LLM.  This is the default.  It has no effect if no
tools are selected.

If set to force, ai-workbench-engine will try to force the LLM to call one or
more of the provided tools.  Support for this feature depends on
the backend/API, and ai-workbench-engine will fall back to the default behavior
when forcing tool use is unsupported.

If nil, tool use is turned off."
  :type '(choice
          (const :tag "Enable" t)
          (const :tag "Force tool use" force)
          (const :tag "Turn Off" nil)))

(defcustom ai-workbench-confirm-tool-calls 'auto
  "Whether tool calls should wait for the user to run them.

If set to t or nil, tool calls always or never seek confirmation
from the user before running.

If set to the symbol auto (the default), a tool call will seek
confirmation only when the corresponding tool spec has a non-nil
:confirm slot.  See `ai-workbench-make-tool'."
  :type '(choice
          (const :tag "Tool decides" auto)
          (const :tag "Always" t)
          (const :tag "Never" nil)))

(defcustom ai-workbench-include-tool-results 'auto
  "Whether tool call results should be included in the buffer.

If set to t or nil, results of tool calls are always or never
included in the LLM response, respectively.

If set to the symbol auto (the default), a tool call result is
included only when the corresponding tool spec has a non-nil
:include slot.  See `ai-workbench-make-tool'."
  :type '(choice
          (const :tag "Tool decides" auto)
          (const :tag "Always" t)
          (const :tag "Never" nil)))

(defcustom ai-workbench-llm-tools nil
  "A list of tools to include with ai-workbench-engine requests.

Each tool should be a `ai-workbench-tool' struct, which see.  To specify
a tool, use `ai-workbench-make-tool', which see."
  :group 'ai-workbench-engine
  :type '(repeat ai-workbench-tool))

(cl-defstruct (ai-workbench-tool (:constructor nil)
                          (:constructor ai-workbench--make-tool-internal
                                        (&key function name description args
                                              async category confirm include
                                              &allow-other-keys))
                          (:copier ai-workbench--copy-tool))
  "Struct to specify tools for LLMs to run.

A tool is a function specification sent to the LLM along with
a (plain language) task.  If the LLM decides to use the tool to
accomplish the task, ai-workbench-engine will run the tool and (optionally)
feed the LLM the results.  You can add tools via
`ai-workbench-make-tool', which see."
  (function nil :type function :documentation "Function that runs the tool")
  (name nil :type string :documentation "Tool name, snake_case recommended")
  (description nil :type string :documentation "What the tool does, intended for the LLM")
  (args nil :type list :documentation "List of plists specifying function arguments")
  (async nil :type boolean :documentation "Whether the function runs asynchronously")
  (category nil :type string :documentation "Use to group tools by purpose")
  (confirm nil :type boolean :documentation "Seek confirmation before running tool?")
  (include t :type boolean :documentation "Include tool results in buffer?"))

(defun ai-workbench--preprocess-tool-args (spec)
  "Convert symbol :type values in tool SPEC to strings destructively."
  ;; NOTE: Do not use `sequencep' here, as that covers strings too and breaks
  ;; things.
  (when (or (listp spec) (vectorp spec))
    (cond
     ((vectorp spec)
      (cl-loop for element across spec
               for idx upfrom 0
               do (aset spec idx (ai-workbench--preprocess-tool-args element))))
     ((keywordp (car spec))
      (let ((tail spec))
        (while tail
          (when (and (eq (car tail) :type) (symbolp (cadr tail)))
            (setcar (cdr tail) (symbol-name (cadr tail))))
          ;; TODO: Handle :enum ("provided" "as" "list") here, convert to
          ;; :enum ["provided" "as" "array"]
          (when (or (listp (cadr tail)) (vectorp (cadr tail)))
            (ai-workbench--preprocess-tool-args (cadr tail)))
          (setq tail (cddr tail)))))
     ((listp spec) (dolist (element spec)
                     (when (listp element)
                       (ai-workbench--preprocess-tool-args element))))))
  spec)

(defun ai-workbench--make-tool (&rest spec)
  "Construct a ai-workbench-tool according to SPEC."
  (ai-workbench--preprocess-tool-args (plist-get spec :args))
  (apply #'ai-workbench--make-tool-internal spec))

(defvar ai-workbench--known-tools nil
  "Alist of ai-workbench-engine tools arranged by category.

A \"tool\" is a function spec (definition and description)
provided by ai-workbench-engine to an LLM.  See `ai-workbench-tool'.  Each tool is
assigned a category when it is created, with a category of
\"misc\" if none is specified.

This is a two-level alist mapping categories and tool names to
the tool itself.  It is used as a global register of available
tools and in ai-workbench-engine's UI, see variable `ai-workbench-llm-tools'.

In this example structure, cat-tool and the rest are cl-structs
of type `ai-workbench-tool':

   CATEGORY         TOOL NAME          TOOL
 ((\"filesystem\" . ((\"read_file\"      . cat-tool)
                   (\"list_directory\" . ls-tool)))
  (\"emacs\"      . ((\"read_buffer\"    . buffer-substring-tool)
                   (\"send_message\"   . message-tool))))

This variable is for internal use only, to define a tool use
`ai-workbench-make-tool'.")

(defun ai-workbench-get-tool (path)
  "Find tool in ai-workbench-engine's tool registry at PATH.

PATH can be specified
- as a string representing the tool name, like \"search_db\",
- or as a list representing a category and tool name,
  like \\='(\"emacs\" \"read_buffer\").
In both cases, the first matching ai-workbench-tool is returned.

- as a string representing a category, like \"filesystem\".
In this case a list of all ai-workbench-llm-tools with this category is
returned."
  (or (cl-etypecase path
        (cons (let ((tc (map-nested-elt ai-workbench--known-tools path)))
                (if (consp tc) (map-values tc) tc)))
        (string (if-let* ((category (assoc path ai-workbench--known-tools)))
                    (map-values (cdr category))
                  (cl-loop for (_ . tools) in ai-workbench--known-tools
                           if (assoc path tools)
                           return (cdr it)))))
      (error "No tool matches for %S" path)))

(defun ai-workbench-make-tool (&rest slots)
  "Make a ai-workbench-engine tool for LLM use.

The following keyword arguments are available, of which the first
four SLOTS are required.

NAME: The name of the tool, recommended to be in Javascript style snake_case.

FUNCTION: The function itself (lambda or symbol) that runs the tool.

DESCRIPTION: A verbose description of what the tool does, how to
call it and what it returns.

ARGS: A list of plists specifying the arguments, or nil for a function that
takes no arguments.  Each plist in ARGS requires the following keys:
- argument :name and :description, as strings.
- argument :type, as a symbol.  Allowed types are those understood by the JSON
  schema: string, number, integer, boolean, array, object or null

The following plist keys are conditional/optional:
- :optional, boolean indicating if argument is optional
- :enum for enumerated types, whose value is a vector of strings representing
  allowed values.  Note that :type is still required for enums.
- :items, if the :type is array.  Its value must be a plist including at least
  the item's :type.
- :properties, if the type is object.  Its value must be a plist that can be
  serialized into a JSON object specification by `json-serialize'.

ASYNC: boolean indicating if the elisp function is asynchronous.
If ASYNC is t, the function should take a callback as its first
argument, along with the arguments specified in ARGS, and run the
callback with the tool call result when it's ready.  The callback
itself is an implementation detail and must not be included in
ARGS.

The following keys are optional

CATEGORY: A string indicating a category for the tool.  This is
used only for grouping in ai-workbench-engine's UI.  Defaults to \"misc\".

CONFIRM: Whether the tool call should wait for the user to run it.  If
true, the user will be prompted with the proposed tool call, which can
be examined, accepted, deferred or canceled.  It can also be a function
that receives the same arguments as FUNCTION and returns true if the
user should be prompted.

INCLUDE: Whether the tool results should be included as part of
the LLM output.  This is useful for logging and as context for
subsequent requests in the same buffer.  This is primarily useful
in chat buffers.

Here is an example definition:

  (ai-workbench-make-tool
   :function (lambda (location unit)
                (url-retrieve-synchronously \"api.weather.com/...\"
                                            location unit))
   :name \"get_weather\"
   :description \"Get the current weather in a given location\"
   :args (list \\='(:name \"location\"
                 :type string
                 :description \"The city and state, e.g. San Francisco, CA\")
               \\='(:name \"unit\"
                 :type string
                 :enum [\"celsius\" \"farenheit\"]
                 :description
                 \"The unit of temperature, either \\='celsius\\=' or \\='fahrenheit\\='\"
                 :optional t)))

If the tool is asynchronous, the function is modified to take a
callback as its first argument, which it runs with the result:

   (lambda (callback location unit)
     (url-retrieve \"api.weather.com/...\"
                   (lambda (_)
                     (let ((result (parse-this-buffer)))
                       (funcall callback result)))))"
  (let* ((tool (apply #'ai-workbench--make-tool slots))
         (category (or (ai-workbench-tool-category tool) "misc")))
    (setf (alist-get
           (ai-workbench-tool-name tool)
           (alist-get category ai-workbench--known-tools nil nil #'equal)
           nil nil #'equal)
          tool)))

(cl-defgeneric ai-workbench--parse-tools (_backend tools)
  "Parse TOOLS and return a list of prompts.

TOOLS is a list of `ai-workbench-tool' structs, which see.

_BACKEND is the LLM backend in use.  This is the default
implementation, used by OpenAI-compatible APIs and Ollama."
  (vconcat
   (mapcar
    (lambda (tool)
      (list
       :type "function"
       :function
       (append
        (list
         :name (ai-workbench-tool-name tool)
         :description (ai-workbench-tool-description tool))
        (if (ai-workbench-tool-args tool)
            (list
             :parameters
             (list :type "object"
                   ;; ai-workbench-engine's tool args spec is close to the JSON schema, except
                   ;; that we use (:name "argname" ...)
                   ;; instead of  (:argname (...)), and
                   ;; (:optional t) for each arg instead of (:required [...])
                   ;; for all args at once.  Handle this difference by
                   ;; modifying a copy of the ai-workbench-engine tool arg spec.
                   :properties
                   (cl-loop
                    for arg in (ai-workbench-tool-args tool)
                    for argspec = (copy-sequence arg)
                    for name = (plist-get arg :name) ;handled differently
                    for newname = (or (and (keywordp name) name)
                                      (make-symbol (concat ":" name)))
                    do                ;ARGSPEC is ARG without unrecognized keys
                    (cl-remf argspec :name)
                    (cl-remf argspec :optional)
                    if (equal (plist-get arg :type) "object")
                    do (unless (plist-member argspec :required)
                         (plist-put argspec :required []))
                    (plist-put argspec :additionalProperties :json-false)
                    append (list newname argspec))
                   :required
                   (vconcat
                    (delq nil (mapcar
                               (lambda (arg) (and (not (plist-get arg :optional))
                                             (plist-get arg :name)))
                               (ai-workbench-tool-args tool))))
                   :additionalProperties :json-false))
          (list :parameters (list :type "object" :properties nil))))))
    (ensure-list tools))))

(cl-defgeneric ai-workbench--parse-tool-results (backend results)
  "Return a BACKEND appropriate prompt containing tool call RESULTS.

This will be injected into the messages list in the prompt to
send to the LLM.")

;; FIXME(fsm) unify this with `ai-workbench--inject-media', which is a mess
(cl-defgeneric ai-workbench--inject-prompt
    (_backend data new-prompt &optional position)
  "Inject NEW-PROMPT into existing prompts in query DATA.

NEW-PROMPT can be a single message or a list of messages.

If POSITION is
- nil, append NEW-PROMPT at the end of DATA
- a non-negative integer, insert it at that position in DATA.
- a negative integer, insert it there counting from the end.

- Not implemented: a list of accessors, inject it at that position.

This generic implementation handles the Anthropic,
OpenAI-compatible and Ollama message formats."
  (when (keywordp (car-safe new-prompt)) ;Is new-prompt one or many?
    (setq new-prompt (list new-prompt)))
  (let ((prompts (plist-get data :messages)))
    (pcase position
      ('nil (plist-put data :messages (vconcat prompts new-prompt)))
      ((pred integerp)
       (when (< position 0) (setq position (+ (length prompts) position)))
       (plist-put data :messages (vconcat (substring prompts 0 position)
                                          new-prompt
                                          (substring prompts position)))))))

(cl-defgeneric ai-workbench--inject-tool-call (backend _data _tool-call new-call)
  "Replace TOOL-CALL in query DATA with NEW-CALL.

DATA is the request payload containing the array of user and assistant
messages, typically available as (plist-get INFO :data).  TOOL-CALL is
the call plist as recorded by ai-workbench-engine's response parser(s).  NEW-CALL is
the replacement plist containing the new tool name and arguments, in the
form

  (:name \"newName\" :args (:arg1 \"newArg\" :arg2 ...))

:name and :args are both optional.

If NEW-CALL is nil, the tool call is removed from DATA and thus the turn
history.

BACKEND is the `ai-workbench-backend'."
  (display-warning
   '(ai-workbench-engine tool-call)
   (format "Editing tool call arguments is not implemented for %s.\
  Ignoring new arguments %s"
           (type-of backend)
           (truncate-string-to-width (prin1-to-string new-call) 50 nil nil t))))


;;; State machine for driving requests
(defvar ai-workbench-request--transitions
  `((INIT . ((t                       . WAIT)))
    (WAIT . ((t                       . TYPE)))
    (TYPE . ((,#'ai-workbench--error-p       . ERRS)
             (,#'ai-workbench--tool-use-p    . TOOL)
             (t                       . DONE)))
    (TOOL . ((t                       . TRET)))
    (TRET . ((,#'ai-workbench--error-p       . ERRS)
             (,#'ai-workbench--tool-result-p . WAIT)
             (t                       . DONE))))
  "Alist specifying ai-workbench-engine's default state transition table for requests.

Each entry is a list whose car is a request state (any symbol)
and whose cdr is an alist listing possible next states.  Each key
is either a predicate function or t.  When `ai-workbench--fsm-next' is
called, the predicates are called in the order they appear here
to find the next state.  Each predicate is called with the state
machine's INFO, see `ai-workbench-fsm'.  A predicate of t is
considered a success and acts as a default.")

(defvar ai-workbench-request--handlers
  `((WAIT ,#'ai-workbench--handle-wait)
    (TOOL ,#'ai-workbench--handle-tool-use)
    (TRET ,#'ai-workbench--handle-tool-result)
    (DONE ,#'ai-workbench--handle-post)
    (ERRS ,#'ai-workbench--handle-post)
    (ABRT ,#'ai-workbench--handle-post))
  "Alist specifying handlers for ai-workbench-engine's default state transitions.

Each entry is a list whose car is a request state (a symbol) and
whose cdr is a list of handler functions called when
transitioning to that state.  The handlers are called in the
sequence that they appear in the list, and each function receives
the state machine as its only argument.  Information about the
request state can be retrieved via the machine's INFO slot, see
`ai-workbench-fsm'.

Handlers are responsible for doing state-related tasks (like
logging errors or inserting responses) and transitioning to the
next state by calling `ai-workbench--fsm-transition'.

Handlers can be asynchronous, in which case the transition call
should typically be placed in its callback.")

(cl-defstruct (ai-workbench-fsm (:constructor ai-workbench-make-fsm)
                         (:copier ai-workbench-copy-fsm))
  "State machine for ai-workbench-engine requests.

STATE: The current state of the machine, can be any symbol.

TABLE: Alist mapping states to possible next states
along with predicates to determine the next state.  See
`ai-workbench-request--transitions' for an example.

HANDLERS: Alist mapping states to state handler functions.
Handlers are called when entering each state.  See
`ai-workbench-request--handlers' for an example

INFO: The state machine's current context.  This is a plist
holding all the information required for the ongoing request, and
can be used to tweak and resume a paused request.  This should be
called \"context\", but context means too many things already in
ai-workbench-engine's code!

Each ai-workbench-engine request is passed an instance of this
state machine and driven by it."
  (state 'INIT)
  (table ai-workbench-request--transitions)
  (handlers ai-workbench-request--handlers) info)

(defun ai-workbench--fsm-transition (machine &optional new-state)
  "Move MACHINE to its next state.

MACHINE is an instance of `ai-workbench-fsm'.

The next state is NEW-STATE if given.  Otherwise it is determined
automatically from MACHINE's transition table."
  (unless new-state (setq new-state (ai-workbench--fsm-next machine)))
  (push (ai-workbench-fsm-state machine)
        (plist-get (ai-workbench-fsm-info machine) :history))
  (setf (ai-workbench-fsm-state machine) new-state)
  (when-let* ((handlers (alist-get new-state (ai-workbench-fsm-handlers machine))))
    (mapc (lambda (h) (funcall h machine)) handlers)))

(defun ai-workbench--fsm-next (machine)
  "Determine MACHINE's next state according to its transition table.

MACHINE is an instance of `ai-workbench-fsm'"
  (let* ((current (ai-workbench-fsm-state machine))
         (transitions (alist-get current (ai-workbench-fsm-table machine))))
    (cl-loop
     with info = (ai-workbench-fsm-info machine)
     for (pred . next) in transitions
     when (or (eq pred t) (funcall pred info))
     return next)))

;;;; State machine handlers
;; The next few functions are default state handlers for ai-workbench-engine's state machine,
;; see `ai-workbench-request--handlers'.

(cl-defgeneric ai-workbench--get-response (backend fsm)
  "Fetch the response for the request in state machine FSM.

BACKEND is the ai-workbench-engine backend in use.  The default method uses the HTTP
transport (Curl or url.el).  Specialized backends that talk to a local
subprocess or other non-HTTP source may override this method to supply
their own transport while reusing the rest of ai-workbench-engine's request pipeline."
  (ignore backend)
  (funcall
   (if ai-workbench-use-curl
       #'ai-workbench-curl-get-response
     #'ai-workbench--url-get-response)
   fsm))

(defun ai-workbench--handle-wait (fsm)
  "Fire the request contained in state machine FSM's info."
  ;; Reset some flags in info.  This is necessary when reusing fsm's context for
  ;; a second network request: ai-workbench-engine tests for the presence of these flags to
  ;; handle state transitions.  (NOTE: Don't add :uuid to this.)
  (let ((info (ai-workbench-fsm-info fsm)))
    (dolist (key '(:tool-result :tool-use :error :http-status :reasoning :tokens))
      (when (plist-get info key)
        (plist-put info key nil))))
  (ai-workbench--get-response (plist-get (ai-workbench-fsm-info fsm) :backend) fsm)
  (run-hooks 'ai-workbench-post-request-hook))

(defun ai-workbench--process-tool-call (fsm tool-spec tool-call result)
  "Add tool RESULT to a TOOL-CALL and transition FSM if required.

TOOL-CALL is a plist with the tool :name, :args and other metadata.
TOOL-SPEC is the `ai-workbench-tool' object, and FSM is the request state.

If all pending tool calls in the current request have finished, it
injects the results into the prompt data and transitions the FSM."
  (let* ((info (ai-workbench-fsm-info fsm))
         (tool-result-alist (plist-get info :tool-result))
         ;; MAYBE(tool-hooks): Use plist-member for valid nil :result?
         (remaining (cl-loop for call in (plist-get info :tool-use)
                             count (not (plist-get call :result)))))
    (let ((result (ai-workbench--to-string result)))
      ;; FIXME(tool-hooks): If a hook has changed the tool that was called
      ;; tool-spec needs to be updated.
      (push (list tool-spec (plist-get tool-call :args) result)
            tool-result-alist)
      (plist-put info :tool-result tool-result-alist) ;for the callback
      ;; NOTE: tool-call is a member of (plist-get info :tool-use), so :tool-use
      ;; is modified by side effect.
      ;; FIXME: Make the implicit addition to :tool-use explicit
      (plist-put tool-call :result result)) ;for the LLM
    ;; All tools have run
    (when (<= (cl-decf remaining) 0) (ai-workbench--fsm-transition fsm))))

(defun ai-workbench--handle-tool-use (fsm)
  "Run tool calls captured in FSM, and advance the state machine with the results."
  (when-let* ((info (ai-workbench-fsm-info fsm))
              (backend (plist-get info :backend))
              ;; This function might run many times, so only act on the remaining tool calls.
              (tool-use (cl-remove-if (lambda (tc) (plist-get tc :result))
                                      (plist-get info :tool-use))))
    (with-current-buffer (plist-get info :buffer)
      (let ((pending-calls))
        (mapc                           ; Construct function calls
         (lambda (tool-call)
           (letrec ((args (plist-get tool-call :args))
                    (name (plist-get tool-call :name))
                    (tool-spec (cl-find-if (lambda (ts) (equal (ai-workbench-tool-name ts) name))
                                           (plist-get info :tools)))
                    (process-tool-result (apply-partially #'ai-workbench--process-tool-call
                                                          fsm tool-spec tool-call)))
             (if (null tool-spec)
                 (if (equal name ai-workbench--ersatz-json-tool) ;Could be a JSON response
                     ;; Handle structured JSON output supplied as tool call
                     (funcall (plist-get info :callback)
                              (ai-workbench--json-encode (plist-get tool-call :args))
                              info)
                   (message "Unknown tool called by model: %s" name))
               (let ((confirm))         ;Check if tool requires confirmation
                 (cond      ;:confirm in tool-call (from hooks) takes precedence
                  ((and-let* ((call-confirm (plist-member tool-call :confirm)))
                     (prog1 t (setq confirm (cadr call-confirm)))))
                  ((and ai-workbench-confirm-tool-calls ;global and tool-specific setting
                        (or (eq ai-workbench-confirm-tool-calls t) ;always confirm, or
                            (and-let* ((confirm (ai-workbench-tool-confirm tool-spec)))
                              (or (not (functionp confirm))
                                  (apply confirm (ai-workbench--map-tool-args tool-spec args))))))
                   (setq confirm t)))
                 (if confirm  ;To send to callback for confirmation
                     (push (list tool-spec args process-tool-result) pending-calls)
                   (let ((arg-values (ai-workbench--map-tool-args tool-spec args)))
                     (if (ai-workbench-tool-async tool-spec) ;If not, run the tool
                         (apply (ai-workbench-tool-function tool-spec)
                                process-tool-result arg-values)
                       (let ((result (condition-case errdata
                                         (apply (ai-workbench-tool-function tool-spec) arg-values)
                                       (error (mapconcat #'ai-workbench--to-string errdata " ")))))
                         (funcall process-tool-result result)))))))))
         tool-use)
        (when pending-calls
          (plist-put info :tool-pending t)
          (funcall (plist-get info :callback)
                   (cons 'tool-call pending-calls) info))))))

(defun ai-workbench--map-tool-args (tool-spec args)
  "Create a tool call argument list from TOOL-SPEC and ARGS.

TOOL-SPEC is a `ai-workbench-tool' and ARGS is a plist of arguments for a tool
call.  The argument list is suitable for supplying to the tool function."
  (mapcar
   (lambda (arg)
     (let ((key (intern (concat ":" (plist-get arg :name)))))
       (plist-get args key)))
   (ai-workbench-tool-args tool-spec)))

(defun ai-workbench--handle-tool-result (fsm)
  "Handle the results of tool execution in FSM.

Inject tool results into into the prompt data (for the LLM), run the
callback (for the user), and transition the request state."
  (let ((info (ai-workbench-fsm-info fsm)))
    (ai-workbench--inject-prompt
     (plist-get info :backend) (plist-get info :data)
     (ai-workbench--parse-tool-results (plist-get info :backend)
                                (plist-get info :tool-use)))
    (funcall (plist-get info :callback)
             (cons 'tool-result (plist-get info :tool-result)) info))
  (ai-workbench--fsm-transition fsm))

(defun ai-workbench--handle-post (fsm)
  "Run cleanup for `ai-workbench-request' with FSM."
  (when-let* ((info (ai-workbench-fsm-info fsm))
              (post (plist-get info :post)))
    (mapc (lambda (f) (funcall f info)) post)))

;;;; State machine predicates
;; Predicates used to find the next state to transition to, see
;; `ai-workbench-request--transitions'.

(defun ai-workbench--error-p (info) (plist-get info :error))

(defun ai-workbench--tool-use-p (info) (plist-get info :tool-use))

(defun ai-workbench--tool-result-p (info) (plist-get info :tool-result))


;;; Send ai-workbench-engine requests
(cl-defun ai-workbench-request
    (&optional prompt &key callback
               (buffer (current-buffer))
               position context dry-run
               (stream nil) (in-place nil)
               (system ai-workbench-system-prompt)
               schema transforms (fsm (ai-workbench-make-fsm)))
  "Request a response from the `ai-workbench-backend' for PROMPT.

The request is asynchronous, this function returns immediately.

If PROMPT is
- a string, it is used to create a full prompt suitable for
  sending to the LLM.
- A list of strings, it is interpreted as a conversation, i.e. a
  series of alternating user prompts and LLM responses.
  (\"user msg 1\" \"llm msg 1\" \"user msg 2\" \"llm msg 2\" ...)
- nil but region is active, the region contents are used.
- nil, the current buffer's contents up to (point) are used.
  Previous responses from the LLM are identified as responses.

Keyword arguments:

CALLBACK, if supplied, is a function of two arguments, called
with the RESPONSE (usually a string) and INFO (a plist):

 (funcall CALLBACK RESPONSE INFO)

RESPONSE is

- A string if the request was successful
- nil if there was no response or an error.

These are the only two cases you typically need to consider,
unless you need to clean up after aborted requests, use LLM
tools, handle \"reasoning\" content specially or stream
responses (see STREAM).  In these cases, RESPONSE can be

- The symbol `abort' if the request is aborted, see `ai-workbench-abort'.

- A cons cell of the form

  (tool-call . ((TOOL ARGS CB) ...))

  where TOOL is a ai-workbench-tool struct, ARGS is a plist of
  arguments, and CB is a function for handling the results.  You
  can call CB with the result of calling the tool to continue the
  request.

- A cons cell of the form

  (tool-result . ((TOOL ARGS RESULT) ...))

  where TOOL is a ai-workbench-tool struct, ARGS is a plist of
  arguments, and RESULT was returned from calling the tool
  function.

- A cons cell of the form

  (reasoning . text)

  where text is the contents of the reasoning block.  (Also see
  STREAM if you are using streaming.)

See `ai-workbench--insert-response' for an example callback handling all
cases.

The INFO plist has (at least) the following keys:
:data         - The request data included with the query
:position     - marker where the response will (nominally) be inserted.
                Of course, the insertion is left to the CALLBACK.
:buffer       - The buffer current when the request was sent,
                unless BUFFER is specified.
:status       - Short string describing the result of the request,
                including possible HTTP errors.

Example of a callback that messages the user with the response
and info:

 (lambda (response info)
  (if (stringp response)
      (let ((posn (marker-position (plist-get info :position)))
            (buf  (buffer-name (plist-get info :buffer))))
        (message \"Response for request from %S at %d: %s\"
                 buf posn response))
    (message \"ai-workbench-request failed with message: %s\"
             (plist-get info :status))))

Or, for just the response:

 (lambda (response _)
  ;; Do something with response
  (message (rot13-string response)))

If CALLBACK is omitted, the response is inserted at the point the
request was sent.

STREAM is a boolean that determines if the response should be
streamed, as in `ai-workbench-stream'.  If the model or the backend does
not support streaming, this will be ignored.

When streaming responses

- CALLBACK will be called repeatedly with each RESPONSE text
  chunk (a string) as it is received.
- When the HTTP request ends successfully, CALLBACK will be
  called with a RESPONSE argument of t to indicate success.
- Similarly, CALLBACK will be called with
  (reasoning . text-chunk) for each reasoning chunk, and
  (reasoning . t) to indicate the end of the reasoning block.

BUFFER and POSITION are the buffer and position (integer or
marker) at which the response is inserted.  If a CALLBACK is
specified, no response is inserted and these arguments are
ignored, but they are still available in the INFO plist passed
to CALLBACK for you to use.

BUFFER defaults to the current buffer, and POSITION to the value
of (point) or (region-end), depending on whether the region is
active.

CONTEXT is any additional data needed for the callback to run. It
is included in the INFO argument to the callback.
Note: This is intended for storing Emacs state to be used by
CALLBACK, and unrelated to the context supplied to the LLM.

SYSTEM is the system message or extended chat directive sent to
the LLM.  This can be a string, a list of strings or a function
that returns either; see `ai-workbench-directives' for more
information. If SYSTEM is omitted, the value of
`ai-workbench-system-prompt' in the current buffer is used.

The following keywords are mainly for internal use:

IN-PLACE is a boolean used by the default callback when inserting
the response to determine if delimiters are needed between the
prompt and the response.

If DRY-RUN is non-nil, do not send the request.  Construct and
return a state machine object that can be introspected and
resumed.

TRANSFORMS is a list of functions used to transform the prompt or query
parameters dynamically.  Each function is called in a temporary buffer
containing the prompt to be sent, and can conditionally modify this
buffer.  This can include changing the (buffer-local) values of the
model, backend or system prompt, or augmenting the prompt with
additional information (such as from a RAG engine).

- Synchronous transformers are called with zero or one argument, the
  state machine for the request.

- Asynchronous transformers are called with two arguments, a callback
  and the state machine.  It should run the callback after finishing its
  transformation.

See `ai-workbench-prompt-transform-functions' for more.

If provided, SCHEMA forces the LLM to generate JSON output.  Its value
is a JSON schema, which can be provided as
- an elisp object, a nested plist structure.
- A JSON schema serialized to a string
- A shorthand object/array description, see `ai-workbench--dispatch-schema-type'.
See the manual or the wiki for examples.

Note: SCHEMA is presently experimental and subject to change, and not
all providers support structured output.

FSM is the state machine driving the request.  This can be used
to define a custom request control flow, see `ai-workbench-fsm' for
details.  You can safely ignore this -- FSM is an unstable
feature and subject to change.

Note:

1. This function is not fully self-contained.  Consider
let-binding the parameters `ai-workbench-backend', `ai-workbench-model',
`ai-workbench-use-tools' and `ai-workbench-use-context' around calls to it as
required.

2. The return value of this function is a state machine that may
be used to rerun or continue the request at a later time."
  (declare (indent 1))
  ;; TODO Remove this check in version 1.0
  (ai-workbench--sanitize-model)
  (let* ((start-marker
          (cond
           ((null position)
            (if (use-region-p)
                (set-marker (make-marker) (region-end))
              (ai-workbench--at-word-end (point-marker))))
           ((markerp position) position)
           ((integerp position)
            (set-marker (make-marker) position buffer))))
         (ai-workbench--schema schema)
         (prompt-buffer
          (cond                       ;prompt from buffer or explicitly supplied
           ((null prompt)           ;Send text up to end of word (for evil-mode users)
            (ai-workbench--create-prompt-buffer (ai-workbench--at-word-end (point))))
           ((stringp prompt)
            (ai-workbench--with-buffer-copy buffer nil nil
              (insert prompt)
              (setq major-mode 'fundamental-mode) ;Avoid mode-specific behavior
              (current-buffer)))
           ((consp prompt)
            ;; (ai-workbench--parse-list ai-workbench-backend prompt)
            (ai-workbench--with-buffer-copy buffer nil nil
              ;; TEMP Decide on the annotated prompt-list format
              (ai-workbench--parse-list-and-insert prompt)
              (setq major-mode 'fundamental-mode) ;Avoid mode-specific behavior
              (current-buffer)))))
         (system-list (ai-workbench--parse-directive system 'raw)) ;eval function-valued system prompts
         (info (list :data prompt-buffer
                     :buffer buffer
                     :position start-marker)))
    (when transforms (plist-put info :transforms transforms))
    (with-current-buffer prompt-buffer
      (setq ai-workbench-system-prompt         ;guaranteed to be buffer-local
            ;; Retain single-part system messages as strings to avoid surprises
            ;; when applying presets
            (if (cdr system-list) system-list (car system-list))))
    (when stream (plist-put info :stream stream))
    ;; This context should not be confused with the context aggregation context!
    (when callback (plist-put info :callback callback))
    (when context (plist-put info :context context))
    (when in-place (plist-put info :in-place in-place))
    ;; Add info to state machine context
    (when dry-run (plist-put info :dry-run dry-run))
    (setf (ai-workbench-fsm-info fsm) info))

  ;; TEMP: Augment in separate let block for now.  Are we overcapturing?
  ;; FIXME(augment): Call augmentors with INFO, not FSM
  (let ((info (ai-workbench-fsm-info fsm)))
    (with-current-buffer (plist-get info :data)
      (setq-local ai-workbench-prompt-transform-functions (plist-get info :transforms))
      ;; Preset has highest priority because it can change prompt-transform-functions
      (when (memq 'ai-workbench--transform-apply-preset ai-workbench-prompt-transform-functions)
        (ai-workbench--transform-apply-preset fsm)
        (setq ai-workbench-prompt-transform-functions ;avoid mutation, copy transforms
              (remq 'ai-workbench--transform-apply-preset ai-workbench-prompt-transform-functions)))
      (let ((augment-total              ;act like a hook, count total
             (if (memq t ai-workbench-prompt-transform-functions)
                 (length
                  (setq ai-workbench-prompt-transform-functions
                        (nconc (remq t ai-workbench-prompt-transform-functions)
                               (default-value 'ai-workbench-prompt-transform-functions))))
               (length ai-workbench-prompt-transform-functions)))
            (augment-idx 0))
        (if (null ai-workbench-prompt-transform-functions)
            (ai-workbench--realize-query fsm)
          ;; FIXME(request-lib): Cannot use ai-workbench--update-status from this file
          ;; (with-current-buffer (plist-get info :buffer) ;Apply prompt transformations
          ;;   (ai-workbench--update-status " Augmenting..." 'mode-line-emphasis))

          ;; FIXME(augment): This needs to be converted into a linear callback
          ;; chain to avoid race conditions with multiple async augmentors.
          (run-hook-wrapped
           'ai-workbench-prompt-transform-functions
           (lambda (func fsm-arg)
             (with-current-buffer (plist-get info :data)
               (goto-char (point-max))
               (if (= (car (func-arity func)) 2) ;async augmentor
                   (funcall func (lambda ()
                                   (cl-incf augment-idx)
                                   (when (>= augment-idx augment-total) ;All augmentors have run
                                     (ai-workbench--realize-query fsm-arg)))
                            fsm-arg)
                 (if (= (car (func-arity func)) 0)
                     (funcall func)
                   (funcall func fsm-arg)) ;sync augmentor
                 (cl-incf augment-idx)
                 (when (>= augment-idx augment-total) ;All augmentors have run
                   (ai-workbench--realize-query fsm-arg))))
             nil)           ;always return nil so run-hook-wrapped doesn't abort
           fsm)))))
  fsm)

(defun ai-workbench--realize-query (fsm)
  "Realize the query payload for FSM from its prompt buffer.

Initiate the request when done."
  (let ((info (ai-workbench-fsm-info fsm)))
    (with-current-buffer (plist-get info :data)
      (let* ((directive (ai-workbench--parse-directive ai-workbench-system-prompt 'raw))
             ;; DIRECTIVE contains both the system message and the template prompts
             (ai-workbench-system-prompt
              (unless (ai-workbench--model-capable-p 'nosystem) (car directive)))
             ;; TODO(tool) Limit tool use to capable models after documenting :capabilities
             ;; (ai-workbench-use-tools (and (ai-workbench--model-capable-p 'tool-use) ai-workbench-use-tools))
             (stream (and (plist-get info :stream) ai-workbench-use-curl ai-workbench-stream
                          ;; Check model-specific request-params for streaming preference
                          (let* ((model-params (ai-workbench--model-request-params ai-workbench-model))
                                 (stream-spec (plist-get model-params :stream)))
                            ;; If not present, there is no model-specific preference
                            (or (not (memq :stream model-params))
                                ;; If present, it must not be :json-false or nil
                                (and stream-spec (not (eq stream-spec :json-false)))))
                          ;; Check backend-specific streaming settings
                          (ai-workbench-backend-stream ai-workbench-backend)))
             (ai-workbench-stream stream)
             (full-prompt))
        (when (cdr directive)       ; prompt constructed from directive/template
          (save-excursion (goto-char (point-min))
                          (ai-workbench--parse-list-and-insert (cdr directive))))
        (goto-char (point-max))
        (setq full-prompt (ai-workbench--parse-buffer ;prompt from buffer or explicitly supplied
                           ai-workbench-backend (and ai-workbench--num-messages-to-send
                                              (* 2 ai-workbench--num-messages-to-send))))
        ;; Inject media chunks into the first user prompt if required.  Media
        ;; chunks are always included with the first user message,
        ;; irrespective of the preference in `ai-workbench-use-context'.  This is
        ;; because media cannot be included (in general) with system messages.
        ;; TODO(augment): Find a way to do this in the prompt-buffer?
        (when (and ai-workbench-context ai-workbench-use-context (ai-workbench--model-capable-p 'media))
          (ai-workbench--inject-media ai-workbench-backend full-prompt))
        (unless stream (cl-remf info :stream))
        (plist-put info :backend ai-workbench-backend)
        (plist-put info :model ai-workbench-model)
        (when ai-workbench-include-reasoning   ;Required for next-request-only scope
          (plist-put info :include-reasoning ai-workbench-include-reasoning))
        (when (and ai-workbench-use-tools ai-workbench-llm-tools)
          (plist-put info :tools ai-workbench-llm-tools))
        (plist-put info :data
                   (ai-workbench--request-data ai-workbench-backend full-prompt)))
      (kill-buffer (current-buffer)))
    ;; INIT -> WAIT
    (unless (plist-get info :dry-run) (ai-workbench--fsm-transition fsm))
    fsm))

(defun ai-workbench-abort (buf)
  "Stop any active ai-workbench-engine process associated with buffer BUF.

BUF defaults to the current buffer."
  (interactive (list (current-buffer)))
  (when-let* ((proc-attrs
               (cl-find-if
                (lambda (entry)
                  ;; each entry has the form (PROC . (FSM ABORT-FN))
                  (eq (thread-first (cadr entry) ; FSM
                                    (ai-workbench-fsm-info)
                                    (plist-get :buffer))
                      buf))
                ai-workbench--request-alist))
              (proc (car proc-attrs))
              (fsm (cadr proc-attrs))
              (info (ai-workbench-fsm-info fsm))
              (abort-fn (cddr proc-attrs)))
    ;; Run :callback with abort signal
    (with-demoted-errors "Callback error: %S"
      (and-let* ((cb (plist-get info :callback))
                 ((functionp cb)))
        (funcall cb 'abort info)))
    (funcall abort-fn)
    (setf (alist-get proc ai-workbench--request-alist nil 'remove) nil)
    (ai-workbench--fsm-transition fsm 'ABRT)
    (message "Stopped ai-workbench-engine request in buffer %S" (buffer-name buf))))


;;; Prompt creation
(defun ai-workbench--create-prompt-buffer (&optional prompt-end)
  "Return a buffer with the conversation prompt to be sent.

If the region is active limit the prompt text to the region contents.
Otherwise the prompt text is constructed from the contents of the
current buffer up to point, or PROMPT-END if provided."
  (save-excursion
    (save-restriction
      (let ((buf (current-buffer)))
        (cond
         ((derived-mode-p 'org-mode)
          (require 'ai-workbench-org)
          ;; Also handles regions in Org mode
          (ai-workbench-org--create-prompt-buffer prompt-end))
         ((use-region-p)
          (let ((rb (region-beginning)) (re (region-end)))
            (ai-workbench--with-buffer-copy buf rb re
              (current-buffer))))
         (t (unless prompt-end (setq prompt-end (point)))
            (ai-workbench--with-buffer-copy buf (point-min) prompt-end
              (current-buffer))))))))

(defun ai-workbench--create-prompt (&optional prompt-end)
  "Return a full conversation prompt from the contents of this buffer.

If `ai-workbench--num-messages-to-send' is set, limit to that many
recent exchanges.

If PROMPT-END (a marker) is provided, end the prompt contents
there.  This defaults to (point)."
  (with-current-buffer (ai-workbench--create-prompt-buffer prompt-end)
    (unwind-protect
        (ai-workbench--parse-buffer
         ai-workbench-backend (and ai-workbench--num-messages-to-send
                            (* 2 ai-workbench--num-messages-to-send)))
      (kill-buffer (current-buffer)))))

(make-obsolete 'ai-workbench--create-prompt 'ai-workbench--create-prompt-buffer
               "0.9.9")

(cl-defgeneric ai-workbench--parse-buffer (backend max-entries)
  "Parse current buffer backwards from point and return a list of prompts.

BACKEND is the LLM backend in use.

MAX-ENTRIES is the number of queries/responses to include for
contexbt.")

(defun ai-workbench--parse-list-and-insert (prompts)
  "Insert PROMPTS, a list of messages into the current buffer.

Propertize the insertions in a format ai-workbench-engine can parse into a
conversation.

PROMPTS is typically the input to `ai-workbench-request', either a list of strings
representing a conversation with alternate prompt/response turns, or a list of
lists with explicit roles (prompt/response/tool).

See `ai-workbench-request' for the former.  Support for the latter format is
experimental."
  (if (stringp (car prompts))           ; Simple format, list of strings
      (cl-loop for text in prompts
               for response = nil then (not response)
               when text
               if response
               do (insert ai-workbench-response-separator
                          (propertize text 'ai-workbench-engine 'response)
                          ai-workbench-response-separator)
               else do (insert text))
    (dolist (entry prompts)             ; Advanced format, list of lists
      (pcase entry
        (`(prompt . ,msg) (insert (or (car-safe msg) msg)))
        (`(response . ,msg)
         (insert ai-workbench-response-separator
                 (propertize (or (car-safe msg) msg) 'ai-workbench-engine 'response)))
        (`(tool . ,call)
         (insert ai-workbench-response-separator
                 (propertize
                  (concat
                   (prin1-to-string `( :name ,(plist-get call :name)
                                       :args ,(plist-get call :args)))
                   "\n\n" (plist-get call :result))
                  'ai-workbench-engine `(tool . ,(plist-get call :id)))))))))

(cl-defgeneric ai-workbench--parse-list (backend prompt-list)
  "Parse PROMPT-LIST and return a list of prompts for BACKEND.

PROMPT-LIST is interpreted as a conversation, i.e. an alternating
series of user prompts and LLM responses.  The returned structure
is suitable for including in the request payload.

BACKEND is the LLM backend in use.")

(cl-defgeneric ai-workbench--parse-media-links (mode beg end)
  "Find media links between BEG and END.

MODE is the major-mode of the buffer.

Returns a plist where each entry is of the form
  (:text \"some text\")
or
  (:media \"media uri or file path\")."
  (ignore mode)                         ;byte-compiler
  (list `(:text ,(buffer-substring-no-properties
                  beg end))))

(declare-function markdown-link-at-pos "markdown-mode")
(declare-function mailcap-file-name-to-mime-type "mailcap")

(defsubst ai-workbench-markdown--validate-link (link)
  "Validate a Markdown LINK as sendable under the current ai-workbench-engine settings.

Return a form (validp link-type path . REST), where REST is a list
explaining why sending the link is not supported by ai-workbench-engine.  Only the
first nil value in REST is guaranteed to be correct."
  (let ((mime))
    (if-let* ((path (nth 3 link))
              (prefix (or (string-search "://" path) 0))
              (link-type (if (= prefix 0) "file" (substring path 0 prefix)))
              (path (if (and (equal link-type "file") (> prefix 0))
                        (substring path (+ prefix 3)) path))
              (resource-type
               (or (and (equal link-type "file") 'file)
                   (and (ai-workbench--model-capable-p 'url)
                        (member link-type '("http" "https" "ftp")) 'url)))
              (user-check (funcall ai-workbench-markdown-validate-link link))
              (readablep (or (member link-type '("http" "https" "ftp"))
                             (file-remote-p path)
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

(cl-defmethod ai-workbench--parse-media-links ((_mode (eql 'markdown-mode)) beg end)
  "Parse text and actionable links between BEG and END.

Return a list of the form
 ((:text \"some text\")
  (:media \"/path/to/media.png\" :mime \"image/png\")
  (:text \"More text\"))
for inclusion into the user prompt for the ai-workbench-engine request."
  (let ((parts) (from-pt))
    (save-excursion
      (setq from-pt (goto-char beg))
      (while (re-search-forward ai-workbench-markdown--link-regex end t)
        (let* ((link-at-pt (markdown-link-at-pos (point)))
               (link-status (ai-workbench-markdown--validate-link link-at-pt)))
          (cl-destructuring-bind
              (valid type path resource-type user-check readablep mime-valid mime)
              link-status
            (cond
             ((and valid (member type '("http" "https" "ftp")))
              ;; Collect text up to this image, and collect this image url
              (let ((text (buffer-substring-no-properties from-pt (car link-at-pt))))
                (unless (string-blank-p text) (push (list :text text) parts))
                (push (list :url path :mime mime) parts)
                (setq from-pt (cadr link-at-pt))))
             (valid   ; Collect text up to this link, and collect this link data
              (let ((text (buffer-substring-no-properties from-pt (car link-at-pt))))
                (unless (string-blank-p text) (push (list :text text) parts))
                (push (if mime (list :media path :mime mime) (list :textfile path)) parts)
                (setq from-pt (cadr link-at-pt))))
             ((not resource-type)
              (message "Link source not followed for unsupported link type \"%s\"." type))
             ((not user-check)
              (message
               (if (eq ai-workbench-markdown-validate-link 'ai-workbench--link-standalone-p)
                   "Ignoring non-standalone link \"%s\"."
                 "Link %s failed to validate, see `ai-workbench-markdown-validate-link'.")
               path))
             ((not readablep) (message "Ignoring inaccessible file \"%s\"." path))
             ((and (not mime-valid) (eq resource-type 'file))
              (message "Ignoring unsupported binary file \"%s\"." path)))))))
    (unless (= from-pt end)
      (push (list :text (buffer-substring-no-properties from-pt end)) parts))
    (nreverse parts)))

(cl-defgeneric ai-workbench--inject-media (backend _prompts)
  "Wrap the last prompt in PROMPTS with ai-workbench-engine's context.

PROMPTS is a structure as returned by `ai-workbench--parse-buffer'.
Typically this is a list of plists.

BACKEND is the ai-workbench-engine backend in use."
  (display-warning
   '(ai-workbench-engine context)
   (format "Context support not implemented for backend %s, ignoring context"
           (ai-workbench-backend-name backend))))

(cl-defgeneric ai-workbench--request-data (backend prompts)
  "Generate a plist of all data for an LLM query.

BACKEND is the LLM backend in use.

PROMPTS is the plist of previous user queries and LLM responses.")

(cl-defun ai-workbench--sanitize-model (&key (backend ai-workbench-backend)
                                      (model ai-workbench-model)
                                      (shoosh t))
  "Check if MODEL is available in BACKEND, adjust accordingly.

If SHOOSH is true, don't issue a warning."
  (unless backend
    (user-error "No ai-workbench backend configured.  Call `ai-workbench-engine-cli-register' first"))
  (let ((available (ai-workbench-backend-models backend)))
    (when (stringp model)
      (unless shoosh
        (display-warning
         'ai-workbench-engine
         (format "`ai-workbench-model' expects a symbol, found string \"%s\"
   Resetting `ai-workbench-model' to %s"
                 model model)))
      (setq ai-workbench-model (ai-workbench--intern model)
            model ai-workbench-model))
    (unless (member model available)
      (let ((fallback (car available)))
        (unless shoosh
          (display-warning
           'ai-workbench-engine
           (format (concat "Preferred `ai-workbench-model' \"%s\" not"
                           "supported in \"%s\", using \"%s\" instead")
                   model (ai-workbench-backend-name backend) fallback)))
        (setq-local ai-workbench-model fallback)))))


;;; url-retrieve response handling
(defun ai-workbench--url-get-response (fsm)
  "Fetch response to prompt in state FSM from the LLM.

FSM is the state machine driving this request.  Its INFO slot
contains the data required for setting up the request.  INFO is a
plist with the following keys, among others:
- :data     (the data being sent)
- :buffer   (the ai-workbench-engine buffer)
- :position (marker at which to insert the response).
- :callback (optional, the request callback)

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((inhibit-message t)
         (message-log-max nil)
         (url-request-method "POST")
         (info (ai-workbench-fsm-info fsm))
         ;; We have to let-bind the following two since their dynamic
         ;; values are used for key lookup and url resolution
         (ai-workbench-backend (plist-get info :backend))
         (ai-workbench-model (plist-get info :model))
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (ai-workbench-backend-header ai-workbench-backend)))
                    (ai-workbench--maybe-funcall header info))))
         (callback (or (plist-get info :callback) ;if not the first run
                       #'ai-workbench--insert-response)) ;default callback
         ;; NOTE: We don't need the decode-coding-string dance here since we
         ;; don't pass it to the OS environment and Curl.
         (url-request-data
          (ai-workbench--json-encode (plist-get info :data))))
    (when (with-current-buffer (plist-get info :buffer)
            (and (derived-mode-p 'org-mode)
                 ai-workbench-org-convert-response))
      (plist-put info :transformer #'ai-workbench--convert-markdown->org))
    (plist-put info :callback callback)
    (when ai-workbench-log-level               ;logging
      (when (eq ai-workbench-log-level 'debug)
        (ai-workbench--log (ai-workbench--json-encode
                     (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                             url-request-extra-headers))
                    "request headers"))
      (ai-workbench--log url-request-data "request body"))
    (let ((proc-buf
           (url-retrieve (let ((backend-url (ai-workbench-backend-url ai-workbench-backend)))
                           (ai-workbench--maybe-funcall backend-url info))
                         (lambda (_)
                           (set-buffer-multibyte t)
                           (set-buffer-file-coding-system 'utf-8-unix)
                           (pcase-let ((`(,response ,http-status ,http-msg ,error)
                                        (ai-workbench--url-parse-response
                                         (plist-get info :backend) info))
                                       (buf (current-buffer)))
                             (plist-put info :http-status http-status)
                             (plist-put info :status http-msg)
                             (ai-workbench--fsm-transition fsm) ;WAIT -> TYPE
                             (when error (plist-put info :error error))
                             (when response ;Look for a reasoning block
                               (if (string-match-p "^\\s-*<think>" response)
                                   (when-let* ((idx (string-search "</think>" response)))
                                     (with-demoted-errors "ai-workbench-engine callback error: %S"
                                       (funcall callback
                                                (cons 'reasoning
                                                      (substring response nil (+ idx 8)))
                                                info))
                                     (setq response (string-trim-left
                                                     (substring response (+ idx 8)))))
                                 (when-let* ((reasoning (plist-get info :reasoning))
                                             ((stringp reasoning)))
                                   (funcall callback (cons 'reasoning reasoning) info))))
                             (when (or response (not (member http-status '("200" "100"))))
                               (with-demoted-errors "ai-workbench-engine callback error: %S"
                                 (funcall callback response info)))
                             (ai-workbench--fsm-transition fsm) ;TYPE -> next
                             (setf (alist-get buf ai-workbench--request-alist nil 'remove) nil)
                             (kill-buffer buf)))
                         nil t nil)))
      ;; TODO: Add transformer here.
      (setf (alist-get proc-buf ai-workbench--request-alist)
            (cons fsm
                  #'(lambda ()
                      (plist-put info :callback #'ignore)
                      (let (kill-buffer-query-functions)
                        ;;Can't stop url-retrieve process
                        (kill-buffer proc-buf))))))))

(cl-defgeneric ai-workbench--parse-response (backend response proc-info)
  "Response extractor for LLM requests.

BACKEND is the LLM backend in use.

RESPONSE is the parsed JSON of the response, as a plist.

PROC-INFO is a plist with process information and other context.
See `ai-workbench-curl--get-response' for its contents.")

(defun ai-workbench--url-parse-response (backend proc-info)
  "Parse response from BACKEND with PROC-INFO."
  (when ai-workbench-log-level                 ;logging
    (save-excursion
      (goto-char url-http-end-of-headers)
      (when (eq ai-workbench-log-level 'debug)
        (ai-workbench--log (ai-workbench--json-encode (buffer-substring-no-properties (point-min) (point)))
                    "response headers"))
      (ai-workbench--log (buffer-substring-no-properties (point) (point-max))
                  "response body")))
  (if-let* ((http-msg (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position))))
            (http-status
             (save-match-data
               (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                    (match-string 1 http-msg))))
            (response (progn (goto-char url-http-end-of-headers)
                             (condition-case nil
                                 (ai-workbench--json-read)
                               (error 'json-read-error)))))
      (cond
       ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
       ((or (memq url-http-response-status '(200 100))
            (string-match-p "\\(?:1\\|2\\)00 OK" http-msg))
        (list (and-let* ((resp (ai-workbench--parse-response backend response proc-info))
                         ((not (string-blank-p resp))))
                (string-trim resp))
              http-status http-msg))
       ((and-let* ((error-data
                    (cond ((plistp response) (or (plist-get response :error)     ; generic
                                                 (plist-get response :detail)    ; openai-oauth
                                                 (plist-get response :message)   ; bedrock
                                                 (plist-get response :Message))) ; bedrock
                          ((arrayp response)
                           (cl-some (lambda (el) (plist-get el :error)) response)))))
          (list nil http-status http-msg error-data)))
       ((eq response 'json-read-error)
        (list nil http-status (concat "(" http-msg ") Malformed JSON in response.") "json-read-error"))
       (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response.")))
    (list nil (concat "(" http-msg ") Could not parse HTTP response.")
          "Could not parse HTTP response.")))


;;; Curl request response handling

(defun ai-workbench-curl--get-args (info uuid include-headers)
  "Produce list of arguments for calling Curl.

INFO contains the request data, UUID is a unique identifier.

If INCLUDE-HEADERS is non-nil, include headers with the -H option."
  (let* ((data (plist-get info :data))
         ;; We have to let-bind the following three since their dynamic
         ;; values are used for key lookup and url resolution
         (ai-workbench-backend (plist-get info :backend))
         (ai-workbench-model (plist-get info :model))
         (ai-workbench-stream (plist-get info :stream))
         (url (let ((backend-url (ai-workbench-backend-url ai-workbench-backend)))
                (ai-workbench--maybe-funcall backend-url info)))
         (data-json (decode-coding-string (ai-workbench--json-encode data) 'utf-8 t)))
    (when ai-workbench-log-level (ai-workbench--log data-json "request body"))
    (append
     ai-workbench-curl--common-args
     ai-workbench-curl-extra-args
     (if include-headers
         (cl-loop
          for (key . val) in
          (append '(("Content-Type" . "application/json"))
                  (when-let* ((header (ai-workbench-backend-header ai-workbench-backend)))
                    (ai-workbench--maybe-funcall header info)))
          collect (format "-H%s: %s" key val))
       (list "-H@-"))
     (and-let* ((curl-args (ai-workbench-backend-curl-args ai-workbench-backend)))
       (ai-workbench--maybe-funcall curl-args))
     (list (format "-w(%s . %%{size_header})" uuid))
     (if (< (string-bytes data-json) ai-workbench-curl-file-size-threshold)
         (list (format "-d%s" data-json))
       (let* ((write-region-inhibit-fsync t)
              (file-name-handler-alist nil)
              (inhibit-message t)
              (temp-filename (make-temp-file "ai-workbench-curl-data" nil ".json" data-json))
              (cleanup-fn (lambda (&rest _) (when (file-exists-p temp-filename)
                                         (delete-file temp-filename)))))
         (plist-put info :post (cons cleanup-fn (plist-get info :post)))
         (list "--data-binary" (format "@%s" temp-filename))))
     (when (not (string-empty-p ai-workbench-proxy))
       (list "--proxy" ai-workbench-proxy
             "--proxy-negotiate"
             "--proxy-user" ":"))
     (list url))))

;;;###autoload
(defun ai-workbench-curl-get-response (fsm)
  "Fetch response to prompt in state FSM from the LLM using Curl.

FSM is the state machine driving this request.

FSM is the state machine driving this request.  Its INFO slot
contains the data required for setting up the request.  INFO is a
plist with the following keys, among others:
- :data     (the data being sent)
- :buffer   (the ai-workbench-engine buffer)
- :position (marker at which to insert the response).
- :callback (optional, the request callback)

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (let* ((uuid (md5 (format "%s%s%s%s"
                            (random) (emacs-pid) (user-full-name)
                            (recent-keys))))
         (info (ai-workbench-fsm-info fsm))
         (backend (plist-get info :backend))
         (args (ai-workbench-curl--get-args info uuid nil))
         (stream (plist-get info :stream))
         (process (make-process
                   :name "ai-workbench-curl"
                   :buffer (ai-workbench--temp-buffer " *ai-workbench-curl*")
                   :command (cons (ai-workbench--curl-path) args)
                   :connection-type 'pipe)))
    (with-current-buffer (process-buffer process)
      (cond
       ((eq (ai-workbench-backend-coding-system backend) 'binary)
        ;; set-buffer-file-coding-system is not needed since we don't save this buffer
        (set-buffer-multibyte nil)
        (set-process-coding-system process 'binary 'binary))
       (t
	;; Don't try to convert cr-lf to cr on Windows so that curl's "header size
	;; in bytes" stays correct. Explicitly set utf-8 for non-win systems too,
	;; for cases when buffer coding system is not set to utf-8.
	(set-process-coding-system process 'utf-8-unix 'utf-8-unix)))
      (set-process-query-on-exit-flag process nil)
      (let* ((ai-workbench-backend backend) ;Required for header function's environment
             (ai-workbench-model (plist-get info :model))
             (headers
              (append '(("Content-Type" . "application/json"))
                      (when-let* ((header (ai-workbench-backend-header backend)))
                        (ai-workbench--maybe-funcall header info)))))
        (when (eq ai-workbench-log-level 'debug)
          (ai-workbench--log (ai-workbench--json-encode
                       (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
                               headers))
                      "request headers")
          (ai-workbench--log (mapconcat #'shell-quote-argument
                                 (cons (ai-workbench--curl-path) args) " \\\n")
                      "request Curl command" 'no-json))
        (dolist (header headers)
          (process-send-string process (concat (car header) ": " (cdr header) "\n"))))
      (process-send-eof process)
      (if (plist-get info :uuid)        ;not the first run, set only the uuid
          (plist-put info :uuid uuid)
        (setf (ai-workbench-fsm-info fsm)      ;fist run, set all process parameters
              (nconc (list :uuid uuid
                           :transformer
                           (when (with-current-buffer (plist-get info :buffer)
                                   (and (derived-mode-p 'org-mode)
                                        ai-workbench-org-convert-response))
                             (ai-workbench--stream-convert-markdown->org
                              (plist-get info :position))))
                     (unless (plist-get info :callback)
                       (list :callback (if stream
                                           #'ai-workbench-curl--stream-insert-response
                                         #'ai-workbench--insert-response)))
                     info)))
      (if stream
          (progn (set-process-sentinel process #'ai-workbench-curl--stream-cleanup)
                 (set-process-filter process #'ai-workbench-curl--stream-filter))
        (set-process-sentinel process #'ai-workbench-curl--sentinel))
      (setf (alist-get process ai-workbench--request-alist)
            (cons fsm
                  #'(lambda ()
                      ;; Clean up Curl process
                      (set-process-sentinel process #'ignore)
                      (delete-process process)
                      (kill-buffer (process-buffer process))))))))

;; ;; Ahead-Of-Time dispatch code for the parsers
;; :parser ; FIXME `cl--generic-*' are internal functions
;; (cl--generic-method-function
;;  (if stream
;;      (cl-loop
;;       for type in
;;       (cl--class-allparents (get (type-of backend) 'cl--class))
;;       with methods = (cl--generic-method-table
;;                       (cl--generic 'ai-workbench-curl--parse-stream))
;;       when (cl--generic-member-method `(,type t) nil methods)
;;       return (car it))
;;    (cl-loop
;;     for type in
;;     (cl--class-allparents (get (type-of backend) 'cl--class))
;;     with methods = (cl--generic-method-table
;;                     (cl--generic 'ai-workbench--parse-response))
;;     when (cl--generic-member-method `(,type t t) nil methods)
;;     return (car it))))

(defun ai-workbench-curl--log-response (proc-buf proc-info)
  "Parse response buffer PROC-BUF and log response.

PROC-INFO is the plist containing process metadata."
  (with-current-buffer proc-buf
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "?\n?\n" nil t)
        (when (eq ai-workbench-log-level 'debug)
          (ai-workbench--log (ai-workbench--json-encode
                       (buffer-substring-no-properties
                        (point-min) (1- (point))))
                      "response headers"))
        (let ((p (point)))
          (when (search-forward (plist-get proc-info :uuid) nil t)
            (goto-char (1- (match-beginning 0)))
            (ai-workbench--log (buffer-substring-no-properties p (point))
                        "response body")))))))

;; TODO: Separate user-messaging from this function
(defun ai-workbench-curl--stream-cleanup (process _status)
  "Process sentinel for ai-workbench-engine curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process))
        (exit-status (process-exit-status process)))
    (let* ((fsm (car (alist-get process ai-workbench--request-alist)))
           (info (ai-workbench-fsm-info fsm))
           (http-status (plist-get info :http-status)))
      (when ai-workbench-log-level (ai-workbench-curl--log-response proc-buf info)) ;logging
      (cond
       ;; Curl exited with a non-zero status: connection-level failure
       ((not (zerop exit-status))
        ;; MAYBE: This transition should happen in the process filter, but it's
        ;; not clear how to reliably detect Curl failure there.
        (ai-workbench--fsm-transition fsm)     ;Curl failed, WAIT -> TYPE
        (plist-put info :error
                   (format "Curl failed with exit code %d. See Curl manpage for details."
                           exit-status))
        (plist-put info :status "Curl failure")
        (with-demoted-errors "ai-workbench-engine callback error: %S"
          (funcall (plist-get info :callback) nil info)))
       ;; Finish handling a successful streaming response
       ((member http-status '("200" "100"))
        (with-demoted-errors "ai-workbench-engine callback error: %S"
          (funcall (plist-get info :callback) t info)))
       ;; Capture error message from HTTP error response
       (t
        (with-current-buffer proc-buf
          (goto-char (point-max))
          (if (not (search-backward (plist-get info :uuid) nil t))
              (plist-put info :error "Could not parse Curl response")
            (backward-char)
            (pcase-let* ((`(,_ . ,header-size) (read (current-buffer)))
                         (response (progn (goto-char header-size)
                                          (condition-case nil (ai-workbench--json-read)
                                            (error 'json-read-error))))
                         (error-data
                          (cond ((plistp response)
                                 (or (plist-get response :error)     ; generic
                                     (plist-get response :detail)    ; openai-oauth
                                     (plist-get response :message)   ; bedrock
                                     (plist-get response :Message))) ; bedrock
                                ((arrayp response)
                                 (cl-some (lambda (el) (plist-get el :error)) response)))))
              (cond
               (error-data
                (plist-put info :error error-data))
               ((eq response 'json-read-error)
                (plist-put info :error "Malformed JSON in response."))
               (t (plist-put info :error "Could not parse HTTP response."))))))
        (with-demoted-errors "ai-workbench-engine callback error: %S"
          (funcall (plist-get info :callback) nil info))))
      (ai-workbench--fsm-transition fsm))      ; Move to next state
    (setf (alist-get process ai-workbench--request-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun ai-workbench-curl--stream-filter (process output)
  (let* ((fsm (car (alist-get process ai-workbench--request-alist)))
         (proc-info (ai-workbench-fsm-info fsm))
         (callback (or (plist-get proc-info :callback)
                       #'ai-workbench-curl--stream-insert-response)))
    (with-current-buffer (process-buffer process)
      ;; Insert output
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))

      ;; Find HTTP status
      (unless (plist-get proc-info :http-status)
        (save-excursion
          (goto-char (point-min))
          (when-let* (((not (= (line-end-position) (point-max))))
                      (http-msg (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                      (http-status
                       (save-match-data
                         (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                              (match-string 1 http-msg)))))
            (plist-put proc-info :http-status http-status)
            (plist-put proc-info :status (string-trim http-msg))
            (ai-workbench--fsm-transition fsm)))) ;Response started, WAIT -> TYPE

      (when-let* ((http-msg (plist-get proc-info :status))
                  (http-status (plist-get proc-info :http-status)))
        ;; Find data chunk(s) and run callback
        ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
        (when (member http-status '("200" "100"))
          (let ((response (ai-workbench-curl--parse-stream
                           (plist-get proc-info :backend) proc-info))
                (reasoning-block (plist-get proc-info :reasoning-block)))
            ;; Depending on the API, there are two modes that reasoning or
            ;; chain-of-thought content appears: as part of the main response
            ;; but surrounded by <think>...</think> tags, or as a separate
            ;; JSON field in the response stream.
            ;;
            ;; These cases are handled using two PROC-INFO keys:
            ;;
            ;; :reasoning-block is nil before checking for reasoning, 'in when
            ;; in a reasoning block, t when we reach the end of the block, and
            ;; 'done afterwards or if no reasoning block is found.  This
            ;; applies to both the modes above.
            ;;
            ;; :reasoning contains the reasoning text parsed from the separate
            ;; JSON field.
            ;;
            ;; NOTE: We assume here that the reasoning block always
            ;; precedes the main response block.
            (unless (eq reasoning-block 'done)
              (let ((reasoning (plist-get proc-info :reasoning)))
                (cond
                 ((stringp reasoning)
                  ;; Obtained from separate JSON field in response
                  (funcall callback (cons 'reasoning reasoning) proc-info)
                  (unless reasoning-block ;Record that we're in a reasoning block (#709)
                    (plist-put proc-info :reasoning-block 'in))
                  (plist-put proc-info :reasoning nil)) ;Reset for next parsing round
                 ((and (string-blank-p response) ;Defer checking if response is blank
                       (not reasoning-block))) ;unless we're in a reasoning block already
                 ((and (null reasoning-block) (length> response 0))
                  ;; Obtained from main response stream: reasoning block start
                  (if-let*  ((idx (string-match-p "<think>" response)))
                      (progn
                        (when (> idx 0) ;Collect leading whitespace before <think>
                          (funcall callback (substring response 0 idx) proc-info)
                          (setq response (substring response idx)))
                        (setq response (cons 'reasoning response))
                        (plist-put proc-info :reasoning-block 'in))
                    (plist-put proc-info :reasoning-block 'done)))
                 ((and (not (eq reasoning-block t)) (length> response 0))
                  (if-let* ((idx (string-match-p "</think>" response)))
                      (progn
                        (funcall callback
                                 (cons 'reasoning (substring response nil (+ idx 8)))
                                 proc-info)
                        (setq reasoning-block t) ;Signal end of reasoning stream
                        (plist-put proc-info :reasoning-block t)
                        (setq response (substring response (+ idx 8))))
                    (setq response (cons 'reasoning response)))))
                (when (eq reasoning-block t) ;End of reasoning block
                  (funcall callback '(reasoning . t) proc-info)
                  (plist-put proc-info :reasoning-block 'done))))
            (unless (equal response "") ;Response callback
              (funcall callback response proc-info))))))))

(cl-defgeneric ai-workbench-curl--parse-stream (backend proc-info)
  "Stream parser for ai-workbench-curl.

Implementations of this function run as part of the process
filter for the active query, and return partial responses from
the LLM.

BACKEND is the LLM backend in use.

PROC-INFO is a plist with process information and other context.
See `ai-workbench-curl--get-response' for its contents.")

(defun ai-workbench-curl--sentinel (process _status)
  "Process sentinel for ai-workbench-engine curl requests.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when-let* (((eq (process-status process) 'exit))
                (fsm (car (alist-get process ai-workbench--request-alist)))
                (proc-info (ai-workbench-fsm-info fsm))
                (proc-callback (plist-get proc-info :callback)))
      (when ai-workbench-log-level (ai-workbench-curl--log-response proc-buf proc-info)) ;logging
      (let ((exit-status (process-exit-status process)))
        (if (zerop exit-status)
            (pcase-let ((`(,response ,http-status ,http-msg ,error)
                         (with-current-buffer proc-buf
                           (ai-workbench-curl--parse-response proc-info))))
              (plist-put proc-info :http-status http-status)
              (plist-put proc-info :status http-msg)
              (ai-workbench--fsm-transition fsm) ;WAIT -> TYPE
              (when error (plist-put proc-info :error error))
              ;; Look for a reasoning block
              (if (and (stringp response) (string-match-p "^\\s-*<think>" response))
                  (when-let* ((idx (string-search "</think>" response)))
                    (with-demoted-errors "ai-workbench-engine callback error: %S"
                      (funcall proc-callback
                               (cons 'reasoning (substring response nil (+ idx 8)))
                               proc-info))
                    (setq response
                          (string-trim-left (substring response (+ idx 8)))))
                (when-let* ((reasoning (plist-get proc-info :reasoning))
                            ((stringp reasoning)))
                  (funcall proc-callback (cons 'reasoning reasoning) proc-info)))
              ;; Call callback with response text
              (when (or response (not (member http-status '("200" "100"))))
                (with-demoted-errors "ai-workbench-engine callback error: %S"
                  (funcall proc-callback response proc-info))))
          ;; Curl exited with a non-zero status: connection-level failure
          (plist-put proc-info :error
                     (format "Curl failed with exit code %d. See Curl manpage for details."
                             exit-status))
          (plist-put proc-info :status "Curl failure")
          (ai-workbench--fsm-transition fsm)   ;WAIT -> TYPE
          (with-demoted-errors "ai-workbench-engine callback error: %S"
            (funcall proc-callback nil proc-info))))
      (ai-workbench--fsm-transition fsm))      ;TYPE -> next
    (setf (alist-get process ai-workbench--request-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun ai-workbench-curl--parse-response (proc-info)
  "Parse the buffer BUF with curl's response.

PROC-INFO is a plist with contextual information."
  (let ((uuid (plist-get proc-info :uuid)))
    (goto-char (point-max))
    (if (not (search-backward uuid nil t))
        (list nil nil nil "Could not parse curl response.")
      (backward-char)
      (pcase-let* ((`(,_ . ,header-size) (read (current-buffer))))
        (goto-char (point-min))
        (if-let* ((http-msg (string-trim
                             (buffer-substring (line-beginning-position)
                                               (line-end-position))))
                  (http-status
                   (save-match-data
                     (and (string-match "HTTP/[.0-9]+ +\\([0-9]+\\)" http-msg)
                          (match-string 1 http-msg))))
                  (response (progn (goto-char header-size)
                                   (condition-case nil
                                       (ai-workbench--json-read)
                                     (error 'json-read-error)))))
            (cond
             ;; FIXME Handle the case where HTTP 100 is followed by HTTP (not 200) BUG #194
             ((member http-status '("200" "100"))
              (list (and-let* ((resp (ai-workbench--parse-response
                                      (plist-get proc-info :backend) response proc-info))
                               ((not (string-blank-p resp))))
                      (string-trim resp))
                    http-status http-msg))
             ((and-let* ((error-data
                          (cond ((plistp response) (or (plist-get response :error)     ; generic
                                                       (plist-get response :detail)    ; openai-oauth
                                                       (plist-get response :message)   ; bedrock
                                                       (plist-get response :Message))) ; bedrock
                                ((arrayp response)
                                 (cl-some (lambda (el) (plist-get el :error)) response)))))
                (list nil http-status http-msg error-data)))
             ((eq response 'json-read-error)
              (list nil http-status (concat "(" http-msg ") Malformed JSON in response.")
                    "Malformed JSON in response"))
             (t (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                      "Could not parse HTTP response.")))
          (list nil http-status (concat "(" http-msg ") Could not parse HTTP response.")
                "Could not parse HTTP response."))))))

(provide 'ai-workbench-request)
;;; ai-workbench-request.el ends here
