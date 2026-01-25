;;; unison-ts-repl.el --- UCM integration for unison-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Filipe Guerreiro

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; UCM (Unison Codebase Manager) integration for unison-ts-mode.
;; Provides REPL interaction and command execution.
;;
;;; Code:

(require 'comint)
(require 'compile)
(require 'json)
(require 'project)
(require 'treesit)
(require 'seq)
(require 'url)

(defgroup unison-ts-repl nil
  "UCM integration for Unison."
  :group 'unison-ts
  :prefix "unison-ts-")

(defcustom unison-ts-ucm-executable "ucm"
  "Path to the UCM executable."
  :type 'string
  :group 'unison-ts-repl)

(defcustom unison-ts-output-auto-close 2
  "Seconds to wait before closing output buffer on success.
Set to nil to disable auto-close."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disable" nil))
  :group 'unison-ts-repl)

;;; UCM MCP Client
;;
;; UCM provides an MCP (Model Context Protocol) server that allows sending
;; commands without codebase lock conflicts. This is the preferred method
;; when LSP is running.

(defvar unison-ts-mcp--request-id 0
  "Counter for JSON-RPC request IDs.")

(defun unison-ts-mcp--make-request (method params)
  "Create a JSON-RPC 2.0 request for METHOD with PARAMS."
  (setq unison-ts-mcp--request-id (1+ unison-ts-mcp--request-id))
  `((jsonrpc . "2.0")
    (method . ,method)
    (params . ,params)
    (id . ,unison-ts-mcp--request-id)))

(defun unison-ts-mcp--call (requests)
  "Send REQUESTS to ucm mcp and return list of responses.
REQUESTS is a list of (method . params) cons cells.
Uses synchronous subprocess call to avoid async complexity."
  (let* ((default-directory (unison-ts-project-root))
         (empty-obj (make-hash-table))
         ;; Build init request + all other requests
         (init-req (unison-ts-mcp--make-request
                    "initialize"
                    `((protocolVersion . "2024-11-05")
                      (capabilities . ,empty-obj)
                      (clientInfo . ((name . "unison-ts-mode")
                                     (version . "0.1.0"))))))
         (all-requests (cons init-req
                             (mapcar (lambda (r)
                                       (unison-ts-mcp--make-request (car r) (cdr r)))
                                     requests)))
         ;; Build input string (newline-delimited JSON)
         (input (mapconcat #'json-encode all-requests "\n"))
         ;; Call ucm mcp synchronously
         (output (with-temp-buffer
                   (let ((exit-code (call-process-region
                                     input nil
                                     unison-ts-ucm-executable
                                     nil t nil
                                     "mcp")))
                     (unless (zerop exit-code)
                       (user-error "UCM MCP failed with exit code %d: %s"
                                   exit-code (buffer-string)))
                     (buffer-string))))
         ;; Parse responses (skip first one which is init response)
         (lines (split-string output "\n" t))
         (responses (mapcar (lambda (line)
                              (condition-case _err
                                  (json-read-from-string line)
                                (error
                                 (message "Warning: Failed to parse MCP response: %s" line)
                                 nil)))
                            lines)))
    ;; Return responses after init (skip first)
    (cdr responses)))

(defun unison-ts-mcp--call-tool (tool-name arguments)
  "Call MCP tool TOOL-NAME with ARGUMENTS synchronously."
  (let* ((responses (unison-ts-mcp--call
                     `(("tools/call" . ((name . ,tool-name)
                                        (arguments . ,(or arguments (make-hash-table))))))))
         (response (car responses)))
    (alist-get 'result response)))

(defun unison-ts-mcp--get-project-context ()
  "Get the current project context (name and branch) via MCP."
  (let ((result (unison-ts-mcp--call-tool "get-current-project-context" nil)))
    (when result
      (let* ((content-array (alist-get 'content result))
             (content (if (vectorp content-array)
                          (aref content-array 0)
                        (car content-array))))
        (when (and content (equal (alist-get 'type content) "text"))
          (json-read-from-string (alist-get 'text content)))))))

(defun unison-ts-mcp--with-project-context (func)
  "Call FUNC with project-name and branch-name from current context.
FUNC should accept two arguments: project-name and branch-name.
Signals an error if no project context is found."
  (let ((ctx (unison-ts-mcp--get-project-context)))
    (unless ctx
      (user-error "No Unison project context found. Open a project first"))
    (funcall func
             (alist-get 'projectName ctx)
             (alist-get 'branchName ctx))))

(defun unison-ts-mcp--make-project-context (project-name branch-name)
  "Create a projectContext alist from PROJECT-NAME and BRANCH-NAME."
  `((projectContext . ((projectName . ,project-name)
                       (branchName . ,branch-name)))))

(defun unison-ts-mcp--update-definitions (code)
  "Update definitions with CODE via MCP."
  (unison-ts-mcp--with-project-context
   (lambda (project-name branch-name)
     (unison-ts-mcp--call-tool
      "update-definitions"
      (append (unison-ts-mcp--make-project-context project-name branch-name)
              `((code . ((text . ,code)))))))))

(defun unison-ts-mcp--run-tests ()
  "Run tests in current project via MCP."
  (unison-ts-mcp--with-project-context
   (lambda (project-name branch-name)
     (unison-ts-mcp--call-tool
      "run-tests"
      (unison-ts-mcp--make-project-context project-name branch-name)))))

(defun unison-ts-mcp--run (definition)
  "Run DEFINITION in current project via MCP."
  (unison-ts-mcp--with-project-context
   (lambda (project-name branch-name)
     (unison-ts-mcp--call-tool
      "run"
      (append (unison-ts-mcp--make-project-context project-name branch-name)
              `((definition . ,definition)))))))

;;; UCM API Integration (Legacy HTTP API)
;;
;; When UCM runs in headless mode (e.g., for LSP), it exposes an HTTP API.
;; These functions allow sending commands via that API instead of spawning
;; a new UCM process, avoiding codebase lock conflicts.

(defcustom unison-ts-api-port 5858
  "Port for the UCM codebase server API.
This is the port UCM headless exposes for HTTP API requests.
Note: This is different from the LSP port (default 5757)."
  :type 'integer
  :group 'unison-ts-repl)

(defcustom unison-ts-api-token nil
  "Authentication token for UCM API requests.
If nil, no token is sent.  Set this if UCM was started with --token."
  :type '(choice (string :tag "Token")
                 (const :tag "None" nil))
  :group 'unison-ts-repl)

(defcustom unison-ts-api-host "localhost"
  "Host for the UCM codebase server API."
  :type 'string
  :group 'unison-ts-repl)

(defcustom unison-ts-lsp-port 5757
  "Port for the UCM LSP server.
This is the default port UCM uses for LSP (language server protocol)."
  :type 'integer
  :group 'unison-ts-repl)

(defun unison-ts-api--port-open-p (port)
  "Return non-nil if PORT is accepting connections on localhost."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "unison-port-check"
                   :host unison-ts-api-host
                   :service port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun unison-ts-api--lsp-running-p ()
  "Return non-nil if UCM LSP server is running."
  (unison-ts-api--port-open-p unison-ts-lsp-port))

(defun unison-ts-api--server-running-p ()
  "Return non-nil if a UCM headless server is accepting connections."
  (unison-ts-api--port-open-p unison-ts-api-port))

(defun unison-ts-api--get-endpoint ()
  "Return the UCM API base endpoint URL."
  (let ((base (format "http://%s:%d" unison-ts-api-host unison-ts-api-port)))
    (if unison-ts-api-token
        (format "%s?token=%s" base unison-ts-api-token)
      base)))

(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

(defun unison-ts-api--call (command)
  "Send COMMAND to UCM via the HTTP API.
Returns the response body as a string, or signals an error."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (json-encode `((command . ,command))))
        (endpoint (format "%s/api/ucm/command" (unison-ts-api--get-endpoint))))
    (let ((buffer (url-retrieve-synchronously endpoint t)))
      (when buffer
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (re-search-forward "^$" nil t)
              (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer buffer))))))

(defun unison-ts--send-command (command)
  "Send COMMAND to UCM, preferring API if headless server is running.
Falls back to REPL if no headless server is available."
  (if (unison-ts-api--server-running-p)
      (unison-ts-api--call command)
    (unison-ts--send-to-repl command)))

(defun unison-ts-project-root ()
  "Find Unison project root by looking for .unison directory.
Falls back to `project-root' or `default-directory'."
  (or (locate-dominating-file default-directory ".unison")
      (when-let ((proj (project-current)))
        (project-root proj))
      default-directory))

(defun unison-ts-project-name ()
  "Return the name of the current Unison project."
  (file-name-nondirectory (directory-file-name (unison-ts-project-root))))

(defun unison-ts--ensure-ucm ()
  "Ensure UCM executable is available.
Signals an error if UCM is not found."
  (unless (executable-find unison-ts-ucm-executable)
    (user-error "UCM not found.  Install from https://unison-lang.org")))

(defvar unison-ts-repl--buffers (make-hash-table :test 'equal :weakness 'value)
  "Hash table mapping project roots to their UCM REPL buffers.")

(defun unison-ts--external-ucm-pids ()
  "Return list of external UCM process PIDs (not managed by Emacs)."
  (let ((emacs-ucm-pids (delq nil
                              (mapcar (lambda (buf)
                                        (when-let ((proc (get-buffer-process buf)))
                                          (process-id proc)))
                                      (hash-table-values unison-ts-repl--buffers))))
        (all-pids nil))
    (with-temp-buffer
      (when (zerop (call-process "pgrep" nil t nil "-x" "ucm"))
        (setq all-pids (mapcar #'string-to-number
                               (split-string (buffer-string) "\n" t)))))
    (seq-difference all-pids emacs-ucm-pids)))

(defun unison-ts--find-ucm-buffer ()
  "Find any buffer running UCM (term, vterm, eshell, shell, or MCP REPL).
Returns the buffer or nil."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (or
                 ;; MCP REPL mode buffer
                 (derived-mode-p 'unison-ts-mcp-repl-mode)
                 ;; Process-based UCM buffer
                 (and (process-live-p (get-buffer-process buf))
                      (string-match-p "ucm" (buffer-name buf))))))
            (buffer-list)))

(defun unison-ts--kill-external-ucm ()
  "Kill external UCM processes after confirmation."
  (let ((pids (unison-ts--external-ucm-pids)))
    (when pids
      (dolist (pid pids)
        (signal-process pid 'TERM))
      (sit-for 0.5))))

;;; Comint-based REPL (when no headless UCM is running)

(defvar unison-ts-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-o") #'comint-clear-buffer)
    map)
  "Keymap for `unison-ts-repl-mode'.")

(define-derived-mode unison-ts-repl-mode comint-mode "UCM"
  "Major mode for interacting with UCM via subprocess."
  :group 'unison-ts-repl
  (setq-local comint-prompt-regexp "^[^>\n]*> ")
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes nil))

;;; MCP-based REPL (when headless UCM is running)
;;
;; This mode provides a REPL-like interface that sends commands via MCP
;; to avoid codebase lock conflicts with the running headless UCM.
;;
;; Supported commands:
;;   > <code>           - Evaluate a Unison expression (watch/typecheck)
;;   add <code>         - Add/update definitions
;;   test [namespace]   - Run tests
;;   run <name> [args]  - Run a function
;;   view <names>       - View definition source
;;   find <query>       - Search by name
;;   find : <type>      - Search by type
;;   docs <name>        - Show documentation
;;   help               - Show available commands

(defvar-local unison-ts-mcp-repl--input-start nil
  "Marker for the start of user input in MCP REPL buffer.")

(defvar-local unison-ts-mcp-repl--project-root nil
  "Project root associated with this MCP REPL buffer.")

(defvar unison-ts-mcp-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'unison-ts-mcp-repl-send)
    (define-key map (kbd "C-c M-o") #'unison-ts-mcp-repl-clear)
    (define-key map (kbd "C-a") #'unison-ts-mcp-repl-bol)
    map)
  "Keymap for `unison-ts-mcp-repl-mode'.")

(define-derived-mode unison-ts-mcp-repl-mode fundamental-mode "UCM-MCP"
  "Major mode for interacting with UCM via MCP protocol.
This mode sends commands to UCM via MCP, avoiding codebase lock
conflicts with headless UCM (LSP).

Key bindings:
\\{unison-ts-mcp-repl-mode-map}

Type `help' in the REPL for available commands."
  :group 'unison-ts-repl
  (setq-local unison-ts-mcp-repl--input-start (make-marker)))

(defun unison-ts-mcp-repl--insert-prompt ()
  "Insert the REPL prompt and set up input marker."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "%s> " (or (file-name-nondirectory
                                            (directory-file-name
                                             (or unison-ts-mcp-repl--project-root
                                                 default-directory)))
                                           "ucm"))
                        'face 'comint-highlight-prompt
                        'read-only t
                        'rear-nonsticky t))
    (set-marker unison-ts-mcp-repl--input-start (point))))

(defun unison-ts-mcp-repl-bol ()
  "Move to beginning of line, but after the prompt."
  (interactive)
  (let ((prompt-end (marker-position unison-ts-mcp-repl--input-start)))
    (if (and prompt-end (>= (point) prompt-end)
             (= (line-number-at-pos) (line-number-at-pos prompt-end)))
        (goto-char prompt-end)
      (beginning-of-line))))

(defun unison-ts-mcp-repl--get-input ()
  "Get the current input from the REPL buffer."
  (buffer-substring-no-properties
   (marker-position unison-ts-mcp-repl--input-start)
   (point-max)))

(defconst unison-ts-mcp-repl--help-text
  "UCM MCP REPL Commands:

  > <expr>          Evaluate/typecheck a Unison expression
  add <code>        Add or update definitions
  test [namespace]  Run tests (optionally in a namespace)
  run <name> [args] Execute a function
  view <names>      View definition source code
  find <query>      Search definitions by name
  find : <type>     Search definitions by type signature
  docs <name>       Show documentation for a definition
  help              Show this help message

Examples:
  > 1 + 2
  > List.map (x -> x + 1) [1, 2, 3]
  add myFunc x = x + 1
  test
  run main
  view List.map
  find foldl
  find : [a] -> a
  docs Optional.map
"
  "Help text for MCP REPL commands.")

(defun unison-ts-mcp-repl--parse-command (input)
  "Parse INPUT into (command . args) cons cell."
  (let ((trimmed (string-trim input)))
    (cond
     ;; Empty input
     ((string-empty-p trimmed)
      nil)
     ;; Help
     ((string-match-p "^help\\s-*$" trimmed)
      (cons 'help nil))
     ;; Watch/evaluate: starts with >
     ((string-match "^>\\s-*\\(.*\\)" trimmed)
      (cons 'watch (match-string 1 trimmed)))
     ;; Add definitions
     ((string-match "^add\\s-+\\(.*\\)" trimmed)
      (cons 'add (match-string 1 trimmed)))
     ;; Test
     ((string-match "^test\\(?:\\s-+\\(.*\\)\\)?$" trimmed)
      (cons 'test (match-string 1 trimmed)))
     ;; Run
     ((string-match "^run\\s-+\\(\\S-+\\)\\(?:\\s-+\\(.*\\)\\)?$" trimmed)
      (cons 'run (cons (match-string 1 trimmed)
                       (when (match-string 2 trimmed)
                         (split-string (match-string 2 trimmed))))))
     ;; View
     ((string-match "^view\\s-+\\(.*\\)" trimmed)
      (cons 'view (split-string (match-string 1 trimmed))))
     ;; Find by type (find : <type>)
     ((string-match "^find\\s-*:\\s-*\\(.*\\)" trimmed)
      (cons 'find-type (match-string 1 trimmed)))
     ;; Find by name
     ((string-match "^find\\s-+\\(.*\\)" trimmed)
      (cons 'find-name (match-string 1 trimmed)))
     ;; Docs
     ((string-match "^docs\\s-+\\(\\S-+\\)" trimmed)
      (cons 'docs (match-string 1 trimmed)))
     ;; Default: treat as code to evaluate
     (t
      (cons 'watch trimmed)))))

(defun unison-ts-mcp-repl--execute (command args)
  "Execute MCP COMMAND with ARGS and return result string."
  (let* ((default-directory (or unison-ts-mcp-repl--project-root
                                default-directory))
         (ctx (unison-ts-mcp--get-project-context)))
    (unless ctx
      (user-error "No Unison project context found"))
    (let ((project-name (alist-get 'projectName ctx))
          (branch-name (alist-get 'branchName ctx)))
      (pcase command
        ('help
         unison-ts-mcp-repl--help-text)
        ('watch
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "typecheck-code"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             (code . ((sourceCode . ,(if (string-prefix-p ">" args)
                                         args
                                       (concat "> " args)))))))))
        ('add
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "update-definitions"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             (code . ((text . ,args)))))))
        ('test
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "run-tests"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             ,@(when args `((subnamespace . ,args)))))))
        ('run
         (let ((func-name (car args))
               (func-args (cdr args)))
           (unison-ts-mcp-repl--format-result
            (unison-ts-mcp--call-tool
             "run"
             `((projectContext . ((projectName . ,project-name)
                                  (branchName . ,branch-name)))
               (mainFunctionName . ,func-name)
               (args . ,(or func-args [])))))))
        ('view
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "view-definitions"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             (names . ,args)))))
        ('find-name
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "search-definitions-by-name"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             (query . ,args)))))
        ('find-type
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "search-by-type"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             (query . ,args)))))
        ('docs
         (unison-ts-mcp-repl--format-result
          (unison-ts-mcp--call-tool
           "docs"
           `((projectContext . ((projectName . ,project-name)
                                (branchName . ,branch-name)))
             (name . ,args)))))
        (_
         (format "Unknown command: %s\nType 'help' for available commands." command))))))

(defun unison-ts-mcp-repl--format-result (result)
  "Format MCP RESULT for display in REPL."
  (if result
      (let* ((content-array (alist-get 'content result))
             (content (if (vectorp content-array)
                          (aref content-array 0)
                        (car content-array)))
             (text (alist-get 'text content)))
        (if text
            (let ((parsed (condition-case nil
                              (json-read-from-string text)
                            (error text))))
              (if (and (listp parsed) (not (stringp parsed)))
                  ;; Structured response
                  (let ((errors (alist-get 'errorMessages parsed))
                        (outputs (alist-get 'outputMessages parsed)))
                    (concat
                     (when (and errors (> (length errors) 0))
                       (concat "Errors:\n"
                               (mapconcat #'identity (append errors nil) "\n")
                               "\n"))
                     (when (and outputs (> (length outputs) 0))
                       (mapconcat #'identity
                                  (seq-filter
                                   (lambda (m) (not (string-match-p "^Loading changes" m)))
                                   (append outputs nil))
                                  "\n"))))
                ;; Plain text
                (format "%s" parsed)))
          "(no content)"))
    "(no result)"))

(defun unison-ts-mcp-repl-send ()
  "Send current input to UCM via MCP."
  (interactive)
  (let* ((input (string-trim (unison-ts-mcp-repl--get-input)))
         (parsed (unison-ts-mcp-repl--parse-command input)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (add-text-properties (marker-position unison-ts-mcp-repl--input-start)
                           (point)
                           '(read-only t)))
    (when parsed
      (condition-case err
          (let ((result (unison-ts-mcp-repl--execute (car parsed) (cdr parsed))))
            (unison-ts-mcp-repl--insert-response result))
        (error
         (unison-ts-mcp-repl--insert-error (error-message-string err)))))
    (unison-ts-mcp-repl--insert-prompt)))

(defun unison-ts-mcp-repl--insert-response (response)
  "Insert MCP RESPONSE into the REPL buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (when (and response (not (string-empty-p (string-trim response))))
      (insert (propertize (string-trim response) 'face 'default) "\n"))))

(defun unison-ts-mcp-repl--insert-error (message)
  "Insert error MESSAGE into the REPL buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "Error: %s\n" message)
                        'face 'error))))

(defun unison-ts-mcp-repl-clear ()
  "Clear the MCP REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (unison-ts-mcp-repl--insert-prompt)))

;;; REPL buffer management

(defun unison-ts-repl--buffer-name ()
  "Return the REPL buffer name for the current project."
  (format "*ucm: %s*" (unison-ts-project-name)))

(defun unison-ts-repl--get-buffer ()
  "Get the UCM REPL buffer for the current project.
Returns nil if no REPL buffer exists or it's not usable."
  (let* ((root (unison-ts-project-root))
         (buf (gethash root unison-ts-repl--buffers)))
    (when (and buf (buffer-live-p buf))
      ;; For comint REPL, check process is alive
      ;; For MCP REPL, buffer being live is sufficient
      (with-current-buffer buf
        (if (derived-mode-p 'unison-ts-mcp-repl-mode)
            buf
          (when (get-buffer-process buf)
            buf))))))

(defun unison-ts-repl--start ()
  "Start UCM REPL for the current project.
Uses MCP-based REPL when headless UCM is running (LSP active),
otherwise starts a subprocess-based REPL."
  (unison-ts--ensure-ucm)
  (let ((existing-buf (unison-ts-repl--get-buffer))
        (lsp-running (unison-ts-api--lsp-running-p)))
    (cond
     ;; Already have a usable REPL buffer
     (existing-buf existing-buf)
     ;; LSP running - use MCP REPL (no conflict, MCP coexists with headless)
     (lsp-running
      (message "UCM headless detected. Using MCP-based REPL (no lock conflict).")
      (unison-ts-repl--start-mcp))
     ;; No LSP - start regular subprocess REPL
     (t
      (unison-ts-repl--do-start)))))

(defun unison-ts-repl--start-mcp ()
  "Start an MCP-based REPL buffer."
  (let* ((root (unison-ts-project-root))
         (buf-name (unison-ts-repl--buffer-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'unison-ts-mcp-repl-mode)
        (unison-ts-mcp-repl-mode))
      (setq unison-ts-mcp-repl--project-root root)
      (setq default-directory root)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "UCM MCP REPL - Connected via MCP protocol\n"
                            'face 'font-lock-comment-face))
        (insert (propertize (format "Project: %s\n"
                                    (file-name-nondirectory
                                     (directory-file-name root)))
                            'face 'font-lock-comment-face))
        (insert (propertize "Type 'help' for available commands.\n\n"
                            'face 'font-lock-comment-face)))
      (unison-ts-mcp-repl--insert-prompt))
    (puthash root buf unison-ts-repl--buffers)
    buf))

(defun unison-ts-repl--do-start ()
  "Start a subprocess-based UCM REPL."
  (let* ((root (unison-ts-project-root))
         (default-directory root)
         (buf-name (unison-ts-repl--buffer-name))
         (buf (make-comint-in-buffer "ucm" buf-name unison-ts-ucm-executable nil)))
    (with-current-buffer buf
      (unison-ts-repl-mode))
    (puthash root buf unison-ts-repl--buffers)
    buf))

;;;###autoload
(defun unison-ts-repl ()
  "Switch to UCM REPL buffer, starting UCM if needed.
When headless UCM is running (e.g., via LSP/eglot), uses an
MCP-based REPL that communicates via the MCP protocol to avoid
codebase lock conflicts. Otherwise, starts a traditional subprocess REPL."
  (interactive)
  (let ((buf (or (unison-ts-repl--get-buffer)
                 (unison-ts-repl--start))))
    (pop-to-buffer buf)))

(defvar unison-ts-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    map)
  "Keymap for `unison-ts-output-mode'.")

(define-compilation-mode unison-ts-output-mode "UCM-Output"
  "Major mode for UCM command output."
  (setq-local compilation-error-regexp-alist
              '(("^\\s-*\\([0-9]+\\) \\|" nil 1)))
  (setq-local compilation-disable-input t))

(defun unison-ts-output--sentinel (proc _event)
  "Process sentinel for UCM output.
PROC is the process.  Auto-close buffer on success after
`unison-ts-output-auto-close' seconds."
  (when (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc))
             unison-ts-output-auto-close)
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (run-with-timer unison-ts-output-auto-close nil
                        (lambda ()
                          (when (buffer-live-p buf)
                            (delete-windows-on buf)
                            (kill-buffer buf))))))))

(defun unison-ts--send-to-repl (command)
  "Send COMMAND to the UCM REPL, starting it if needed.
Works with both subprocess-based and MCP-based REPLs."
  (let ((buf (or (unison-ts-repl--get-buffer)
                 (unison-ts-repl--start))))
    (with-current-buffer buf
      (if (derived-mode-p 'unison-ts-mcp-repl-mode)
          ;; MCP REPL - insert command and send
          (progn
            (goto-char (point-max))
            (insert command)
            (unison-ts-mcp-repl-send))
        ;; Comint REPL - send to process
        (goto-char (point-max))
        (comint-send-string (get-buffer-process buf) (concat command "\n"))))
    (display-buffer buf)))

(defun unison-ts--parse-mcp-output (text)
  "Parse MCP output TEXT which may be JSON-encoded UCM response."
  (condition-case nil
      (let ((parsed (json-read-from-string text)))
        (if (and (listp parsed)
                 (or (alist-get 'errorMessages parsed)
                     (alist-get 'outputMessages parsed)))
            parsed
          text))
    (error text)))

(defun unison-ts--display-mcp-result (result title)
  "Display MCP RESULT appropriately based on content.
Short success messages go to minibuffer, errors/long output to a buffer."
  (if (and result (listp result))
      (let* ((content-array (alist-get 'content result))
             (content (if (vectorp content-array)
                          (aref content-array 0)
                        (car content-array)))
             (text (alist-get 'text content))
             (parsed (when text (unison-ts--parse-mcp-output text))))
        (if (and (listp parsed) (not (stringp parsed)))
            (let ((errors (seq-uniq (append (alist-get 'errorMessages parsed) nil)))
                  (outputs (seq-filter
                            (lambda (msg) (not (string-match-p "^Loading changes" msg)))
                            (seq-uniq (append (alist-get 'outputMessages parsed) nil)))))
              (if (= (length errors) 0)
                  ;; Success → minibuffer
                  (message "UCM: %s" (string-join outputs " "))
                ;; Errors → buffer
                (unison-ts--display-in-buffer title errors outputs)))
          ;; Non-parsed output → buffer
          (unison-ts--display-in-buffer title nil (list (format "%s" parsed)))))
    ;; Fallback → buffer
    (unison-ts--display-in-buffer title nil (list (format "%S" result)))))

(defun unison-ts--display-in-buffer (title errors outputs)
  "Display ERRORS and OUTPUTS in a *UCM: TITLE* buffer."
  (let ((buf (get-buffer-create (format "*UCM: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (and errors (> (length errors) 0))
          (insert "⚠️ Errors:\n")
          (seq-do (lambda (msg) (insert msg "\n")) errors)
          (insert "\n"))
        (when outputs
          (seq-do (lambda (msg) (insert msg "\n")) outputs))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

;;;###autoload
(defun unison-ts-add ()
  "Add definitions from the current file to the codebase via MCP."
  (interactive)
  (if (buffer-file-name)
      (let* ((code (buffer-substring-no-properties (point-min) (point-max)))
             (result (unison-ts-mcp--update-definitions code)))
        (unison-ts--display-mcp-result result "add"))
    (user-error "Buffer is not visiting a file")))

;;;###autoload
(defun unison-ts-update ()
  "Update existing definitions in the codebase via MCP."
  (interactive)
  (if (buffer-file-name)
      (let* ((code (buffer-substring-no-properties (point-min) (point-max)))
             (result (unison-ts-mcp--update-definitions code)))
        (unison-ts--display-mcp-result result "update"))
    (user-error "Buffer is not visiting a file")))

;;;###autoload
(defun unison-ts-test ()
  "Run tests in the current project via MCP."
  (interactive)
  (let ((result (unison-ts-mcp--run-tests)))
    (unison-ts--display-mcp-result result "test")))

;;;###autoload
(defun unison-ts-run ()
  "Run a term from the codebase via MCP."
  (interactive)
  (let* ((term (read-string "Term to run: "))
         (result (unison-ts-mcp--run term)))
    (unison-ts--display-mcp-result result "run")))

;;;###autoload
(defun unison-ts-watch ()
  "Typecheck current file and show results via MCP."
  (interactive)
  (if buffer-file-name
      (let* ((code (buffer-substring-no-properties (point-min) (point-max)))
             (ctx (unison-ts-mcp--get-project-context))
             (result (unison-ts-mcp--call-tool
                      "typecheck-code"
                      `((projectContext . ((projectName . ,(alist-get 'projectName ctx))
                                           (branchName . ,(alist-get 'branchName ctx))))
                        (code . ((text . ,code)))))))
        (unison-ts--display-mcp-result result "watch"))
    (user-error "Buffer is not visiting a file")))

;;;###autoload
(defun unison-ts-load ()
  "Load current file into the codebase via MCP."
  (interactive)
  (if buffer-file-name
      (let* ((code (buffer-substring-no-properties (point-min) (point-max)))
             (result (unison-ts-mcp--update-definitions code)))
        (unison-ts--display-mcp-result result "load"))
    (user-error "Buffer is not visiting a file")))

(defun unison-ts--send-code-via-mcp (code)
  "Send CODE to UCM via MCP for updating."
  (let ((result (unison-ts-mcp--update-definitions code)))
    (unison-ts--display-mcp-result result "eval")))

;;;###autoload
(defun unison-ts-send-region (start end)
  "Send the region between START and END to UCM via MCP."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region active"))
  (let ((text (buffer-substring-no-properties start end)))
    (unison-ts--send-code-via-mcp text)))

(defun unison-ts--definition-node-p (node)
  "Return non-nil if NODE is a Unison definition."
  (member (treesit-node-type node)
          '("term_declaration" "type_declaration" "ability_declaration")))

;;;###autoload
(defun unison-ts-send-definition ()
  "Send the definition at point to UCM via MCP."
  (interactive)
  (let ((node (treesit-node-at (point))))
    (unless node
      (user-error "No tree-sitter node at point"))
    (let ((def-node (treesit-parent-until node #'unison-ts--definition-node-p t)))
      (unless def-node
        (user-error "Point is not within a definition"))
      (let ((text (treesit-node-text def-node t)))
        (unison-ts--send-code-via-mcp text)))))

(provide 'unison-ts-repl)

;; Local Variables:
;; package-lint-main-file: "unison-ts-mode.el"
;; End:

;;; unison-ts-repl.el ends here
