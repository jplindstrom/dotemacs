;;; gptel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gptel" "gptel.el" (0 0 0 0))
;;; Generated autoloads from gptel.el

(autoload 'gptel-mode "gptel" "\
Minor mode for interacting with LLMs.

If called interactively, enable Gptel mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'gptel-send "gptel" "\
Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response.

\(fn &optional ARG)" t nil)

(autoload 'gptel "gptel" "\
Switch to or start a chat session with NAME.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt.  Returns the
buffer created or switched to.

INTERACTIVEP is t when gptel is called interactively.

\(fn NAME &optional _ INITIAL INTERACTIVEP)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel" '("gptel-")))

;;;***

;;;### (autoloads nil "gptel-anthropic" "gptel-anthropic.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from gptel-anthropic.el

(autoload 'gptel-make-anthropic "gptel-anthropic" "\
Register an Anthropic API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.anthropic.com\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

\(fn NAME &key CURL-ARGS STREAM KEY (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"x-api-key\" \\=\\, key) (\"anthropic-version\" . \"2023-06-01\"))))) (MODELS \\='(\"claude-3-5-sonnet-20240620\" \"claude-3-sonnet-20240229\" \"claude-3-haiku-20240307\" \"claude-3-opus-20240229\")) (HOST \"api.anthropic.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/messages\"))" nil nil)

(function-put 'gptel-make-anthropic 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil "gptel-context" "gptel-context.el" (0 0 0 0))
;;; Generated autoloads from gptel-context.el
 (autoload 'gptel-add "gptel-context" "Add/remove regions or buffers from gptel's context." t)
 (autoload 'gptel-add-file "gptel-context" "Add files to gptel's context." t)

(autoload 'gptel-context--wrap "gptel-context" "\


\(fn MESSAGE)" nil nil)

(autoload 'gptel-context--collect "gptel-context" "\
Get the list of all active context overlays." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-context" '("gptel-")))

;;;***

;;;### (autoloads nil "gptel-curl" "gptel-curl.el" (0 0 0 0))
;;; Generated autoloads from gptel-curl.el

(autoload 'gptel-curl-get-response "gptel-curl" "\
Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :data (the data being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point.

\(fn INFO &optional CALLBACK)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-curl" '("gptel-")))

;;;***

;;;### (autoloads nil "gptel-gemini" "gptel-gemini.el" (0 0 0 0))
;;; Generated autoloads from gptel-gemini.el

(autoload 'gptel-make-gemini "gptel-gemini" "\
Register a Gemini backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, defaults to
\"generativelanguage.googleapis.com\".

MODELS is a list of available model names.

STREAM is a boolean to enable streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, \"https\" by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1beta/models\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

\(fn NAME &key CURL-ARGS HEADER KEY (STREAM nil) (HOST \"generativelanguage.googleapis.com\") (PROTOCOL \"https\") (MODELS \\='(\"gemini-pro\" \"gemini-1.5-pro-latest\")) (ENDPOINT \"/v1beta/models\"))" nil nil)

(function-put 'gptel-make-gemini 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil "gptel-kagi" "gptel-kagi.el" (0 0 0 0))
;;; Generated autoloads from gptel-kagi.el

(autoload 'gptel-make-kagi "gptel-kagi" "\
Register a Kagi FastGPT backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the Kagi host (with port), defaults to \"kagi.com\".

MODELS is a list of available Kagi models: only fastgpt is supported.

STREAM is a boolean to toggle streaming responses, defaults to
false.  Kagi does not support a streaming API yet.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v0/fastgpt\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

\(gptel-make-kagi \"Kagi\" :key my-kagi-key)

\(fn NAME &key CURL-ARGS STREAM KEY (HOST \"kagi.com\") (HEADER (lambda nil \\=`((\"Authorization\" \\=\\, (concat \"Bot \" (gptel--get-api-key)))))) (MODELS \\='(\"fastgpt\" \"summarize:cecil\" \"summarize:agnes\" \"summarize:daphne\" \"summarize:muriel\")) (PROTOCOL \"https\") (ENDPOINT \"/api/v0/\"))" nil nil)

(function-put 'gptel-make-kagi 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil "gptel-ollama" "gptel-ollama.el" (0 0 0 0))
;;; Generated autoloads from gptel-ollama.el

(autoload 'gptel-make-ollama "gptel-ollama" "\
Register an Ollama backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where Ollama runs (with port), defaults to localhost:11434

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, http by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/generate\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.  This is typically not required
for local models like Ollama.

Example:
-------

\(gptel-make-ollama
  \"Ollama\"
  :host \"localhost:11434\"
  :models \\='(\"mistral:latest\")
  :stream t)

\(fn NAME &key CURL-ARGS HEADER KEY MODELS STREAM (HOST \"localhost:11434\") (PROTOCOL \"http\") (ENDPOINT \"/api/chat\"))" nil nil)

(function-put 'gptel-make-ollama 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-ollama" '("gptel--ollama-token-count")))

;;;***

;;;### (autoloads nil "gptel-openai" "gptel-openai.el" (0 0 0 0))
;;; Generated autoloads from gptel-openai.el

(autoload 'gptel-make-openai "gptel-openai" "\
Register an OpenAI API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

\(fn NAME &key CURL-ARGS MODELS STREAM KEY (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"Authorization\" \\=\\, (concat \"Bearer \" key)))))) (HOST \"api.openai.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/chat/completions\"))" nil nil)

(function-put 'gptel-make-openai 'lisp-indent-function '1)

(autoload 'gptel-make-azure "gptel-openai" "\
Register an Azure backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the API host.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

\(gptel-make-azure
 \"Azure-1\"
 :protocol \"https\"
 :host \"RESOURCE_NAME.openai.azure.com\"
 :endpoint
 \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
 :stream t
 :models \\='(\"gpt-3.5-turbo\" \"gpt-4\"))

\(fn NAME &key CURL-ARGS HOST (PROTOCOL \"https\") (HEADER (lambda nil \\=`((\"api-key\" \\=\\, (gptel--get-api-key))))) (KEY \\='gptel-api-key) MODELS STREAM ENDPOINT)" nil nil)

(function-put 'gptel-make-azure 'lisp-indent-function '1)

(defalias 'gptel-make-gpt4all 'gptel-make-openai "\
Register a GPT4All backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where GPT4All runs (with port), typically localhost:8491

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v1/completions\"

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like GPT4All.

Example:
-------

\(gptel-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(\"mistral-7b-openorca.Q4_0.gguf\"))")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-openai" '("gptel--")))

;;;***

;;;### (autoloads nil "gptel-org" "gptel-org.el" (0 0 0 0))
;;; Generated autoloads from gptel-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-org" '("gptel-")))

;;;***

;;;### (autoloads nil "gptel-privategpt" "gptel-privategpt.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from gptel-privategpt.el

(autoload 'gptel-make-privategpt "gptel-privategpt" "\
Register an Privategpt API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.privategpt.com\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
\((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

CONTEXT and SOURCES: if true (the default), use available context
and provide sources used by the model to generate the response.

\(fn NAME &key CURL-ARGS STREAM KEY (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"Authorization\" \\=\\, (concat \"Bearer \" key)))))) (HOST \"localhost:8001\") (PROTOCOL \"http\") (MODELS \\='(\"private-gpt\")) (ENDPOINT \"/v1/chat/completions\") (CONTEXT t) (SOURCES t))" nil nil)

(function-put 'gptel-make-privategpt 'lisp-indent-function '1)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-privategpt" '("gptel--privategpt-parse-sources")))

;;;***

;;;### (autoloads nil "gptel-transient" "gptel-transient.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from gptel-transient.el
 (autoload 'gptel-menu "gptel-transient" nil t)
 (autoload 'gptel-system-prompt "gptel-transient" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gptel-transient" '("gptel-")))

;;;***

;;;### (autoloads nil nil ("gptel-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gptel-autoloads.el ends here
