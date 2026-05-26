;;; org-mcp.el --- MCP server for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: convenience, files, matching, outlines
;; Version: 0.10.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.2.0"))
;; Homepage: https://github.com/laurynas-biveinis/org-mcp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a Model Context Protocol (MCP) server for
;; Org-mode.

;;; Code:

(require 'cl-lib)
(require 'mcp-server-lib)
(require 'org)
(require 'org-id)
(require 'url-util)

(defcustom org-mcp-allowed-files nil
  "List of absolute paths to Org files that can be accessed via MCP."
  :type '(repeat file)
  :group 'org-mcp)

(defconst org-mcp--server-id "org-mcp"
  "Server ID for org-mcp MCP server registration.")

(defconst org-mcp--uri-headline-prefix "org-headline://"
  "URI prefix for headline resources.")

(defconst org-mcp--uri-id-prefix "org-id://"
  "URI prefix for ID-based resources.")

(defun org-mcp--extract-uri-suffix (uri prefix)
  "Extract suffix from URI after PREFIX.
Returns the suffix string if URI starts with PREFIX, nil otherwise."
  (when (string-prefix-p prefix uri)
    (substring uri (length prefix))))

;; Error handling helpers

(defun org-mcp--headline-not-found-error (headline-path)
  "Throw error for HEADLINE-PATH not found."
  (mcp-server-lib-tool-throw
   (format "Cannot find headline: %s"
           (mapconcat #'identity headline-path "/"))))

(defun org-mcp--id-not-found-error (id)
  "Throw error for ID not found."
  (mcp-server-lib-tool-throw (format "Cannot find ID '%s'" id)))

(defun org-mcp--tool-validation-error (message &rest args)
  "Throw validation error MESSAGE with ARGS for tool operations."
  (mcp-server-lib-tool-throw (apply #'format message args)))

(defun org-mcp--validate-string-field
    (value field-name &optional allow-nil)
  "Validate VALUE is a string and signal a tool error if not.
FIELD-NAME is the wire-protocol parameter name embedded into the
error message so the caller can pinpoint which input to fix.  When
ALLOW-NIL is non-nil, nil VALUE is also accepted -- use this for
optional parameters.  Strings are otherwise unconstrained here;
emptiness, allowed-value, and format checks belong to caller-side
validators that may assume VALUE is a string after this guard."
  (unless (if allow-nil
              (or (null value) (stringp value))
            (stringp value))
    (org-mcp--tool-validation-error
     "Field %s must be a string, got: %S (type: %s)"
     field-name value (type-of value))))

(defun org-mcp--resource-validation-error (message &rest args)
  "Signal validation error MESSAGE with ARGS for resource operations."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (apply #'format message args)))

(defun org-mcp--state-mismatch-error (expected found context)
  "Throw state mismatch error.
EXPECTED is the expected value, FOUND is the actual value,
CONTEXT describes what is being compared."
  (mcp-server-lib-tool-throw
   (format "%s mismatch: expected '%s', found '%s'"
           context expected found)))

(defun org-mcp--resource-not-found-error (resource-type identifier)
  "Signal resource not found error.
RESOURCE-TYPE is the type of resource,
IDENTIFIER is the resource identifier."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "%s not found: '%s'" resource-type identifier)))

(defun org-mcp--tool-id-disallowed-error (id)
  "Throw tool error: ID resolves to a non-allowed file.
The resolved file path is intentionally not included in the error
message to avoid disclosing files the user excluded from
`org-mcp-allowed-files'."
  (mcp-server-lib-tool-throw
   (format "ID '%s' resolves to a file not in the allowed list" id)))

(defun org-mcp--resource-file-access-error (filename)
  "Signal file access error for a filename-based resource request.
FILENAME is the path that was rejected."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "'%s': the referenced file not in allowed list" filename)))

(defun org-mcp--resource-id-disallowed-error (id)
  "Signal resource error: ID resolves to a non-allowed file.
The resolved file path is intentionally not included in the error
message to avoid disclosing files the user excluded from
`org-mcp-allowed-files'."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "ID '%s' resolves to a file not in the allowed list" id)))

;; Helpers

(defun org-mcp--read-file (file-path)
  "Read and return the contents of FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun org-mcp--find-allowed-file (filename)
  "Find FILENAME in `org-mcp-allowed-files'.
Returns the expanded path if found, nil if not in the allowed list.
Compares truenames to handle symlinks and path variations."
  (let ((target (file-truename filename)))
    (when-let* ((found
                 (cl-find
                  target
                  org-mcp-allowed-files
                  :key #'file-truename
                  :test #'string=)))
      (expand-file-name found))))

(defun org-mcp--refresh-file-buffers (file-path)
  "Refresh all buffers visiting FILE-PATH.
Preserves narrowing state across the refresh operation."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when-let* ((buf-file (buffer-file-name)))
        (when (string= buf-file file-path)
          (let ((was-narrowed (buffer-narrowed-p))
                (narrow-start nil)
                (narrow-end nil))
            ;; Save narrowing markers if narrowed
            (when was-narrowed
              (setq narrow-start (point-min-marker))
              (setq narrow-end (point-max-marker)))
            (condition-case err
                (unwind-protect
                    (progn
                      (revert-buffer t t t)
                      ;; Check if buffer was modified by hooks
                      (when (buffer-modified-p)
                        (org-mcp--tool-validation-error
                         "Buffer for file %s was modified during \
refresh.  Check your `after-revert-hook' for functions that modify \
the buffer"
                         file-path)))
                  ;; Restore narrowing even if revert fails
                  (when was-narrowed
                    (narrow-to-region narrow-start narrow-end)))
              (error
               (org-mcp--tool-validation-error
                "Failed to refresh buffer for file %s: %s. \
Check your Emacs hooks (`before-revert-hook', \
`after-revert-hook', `revert-buffer-function')"
                file-path (error-message-string err))))))))))

(defun org-mcp--complete-and-save (file-path response-alist)
  "Create ID if needed, save FILE-PATH, return JSON.
Creates or gets an Org ID for the current headline and returns it.
FILE-PATH is the path to save the buffer contents to.
RESPONSE-ALIST is an alist of response fields."
  (let ((id (org-id-get-create)))
    (write-region (point-min) (point-max) file-path)
    (org-mcp--refresh-file-buffers file-path)
    (json-encode
     (append
      `((success . t))
      response-alist
      `((uri . ,(concat org-mcp--uri-id-prefix id)))))))

(defun org-mcp--fail-if-modified (file-path operation)
  "Check if FILE-PATH has unsaved change in any buffer.
OPERATION is a string describing the operation for error messages."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (string= (buffer-file-name) file-path)
                 (buffer-modified-p))
        (org-mcp--tool-validation-error
         (concat
          "Cannot %s: an Emacs buffer visiting this file has unsaved "
          "changes; ask the user to save it (C-x C-s) and retry")
         operation)))))

(defmacro org-mcp--with-org-file (file-path &rest body)
  "Execute BODY in a temp Org buffer with file at FILE-PATH."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert-file-contents ,file-path)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defmacro org-mcp--modify-and-save
    (file-path operation response-alist &rest body)
  "Execute BODY to modify Org file at FILE-PATH, then save result.
OPERATION is a string describing the operation for error messages.
RESPONSE-ALIST is an alist of response fields appended to the JSON
return value.

Macro behaviour:
- Validates FILE-PATH has no unsaved changes (errors via
  `org-mcp--fail-if-modified' if so).
- Sets up a temp buffer with FILE-PATH's contents in `org-mode',
  with `set-visited-file-name' pointing at FILE-PATH and point at
  `point-min'.
- Executes BODY in that buffer.
- After BODY returns, calls `org-mcp--complete-and-save' which
  ensures the entry containing point has an Org ID (creating one
  if needed), writes the buffer to FILE-PATH, refreshes any
  visiting Emacs buffer, and returns JSON with the entry's
  `org-id://' URI.

BODY contract:
- May access FILE-PATH, OPERATION, and RESPONSE-ALIST as
  variables.
- Must NOT call `org-mcp--complete-and-save' itself; the macro
  appends it after BODY.  Calling it inside BODY would write the
  file twice and return a stale JSON shape.
- Must leave point at or within the entry whose `org-id://' URI
  should be returned to the caller.
- BODY's return value is discarded; the macro's return value is
  the JSON from `org-mcp--complete-and-save'.
- Errors signalled in BODY propagate up unmodified (no rollback)."
  (declare (indent 3) (debug (form form form body)))
  `(progn
     (org-mcp--fail-if-modified ,file-path ,operation)
     (with-temp-buffer
       (set-visited-file-name ,file-path t)
       (insert-file-contents ,file-path)
       (org-mode)
       (goto-char (point-min))
       ,@body
       (org-mcp--complete-and-save ,file-path ,response-alist))))

(defun org-mcp--lookup-id-file (id)
  "Resolve Org ID to an allowed file path without signalling errors.
Returns a cons (STATUS . FILE) where STATUS is one of:
  `:found'      ID found and FILE is in `org-mcp-allowed-files'.
  `:disallowed' ID found but the resolved file is not in the
                allowed list.
  `:missing'    ID not found in `org-id-locations' nor in any
                allowed file.
FILE is meaningful only for `:found' (the expanded path).  For
`:disallowed' and `:missing' FILE is nil; the resolved-but-
unauthorised path is intentionally not carried in the result so
callers cannot inadvertently leak file locations outside
`org-mcp-allowed-files' through error messages.

First consults `org-id-find-id-file'; if that misses, falls back
to scanning every entry in `org-mcp-allowed-files' for an `:ID:'
property matching ID.  On a successful fallback the resolved
\(id, file) pair is registered into `org-id-locations' so subsequent
lookups for the same ID hit the DB at O(1) instead of re-scanning;
this cache write is gated on `org-id-track-globally', so users who
have explicitly opted out of global ID tracking do not get implicit
DB mutations.  The cache write is performed best-effort -- any
signal from `org-id-add-location' is swallowed so a write failure
cannot poison a successful resolution.

Callers translate the status into a tool error or a resource error
of the appropriate shape."
  (if-let* ((id-file (org-id-find-id-file id)))
      (if-let* ((allowed-file (org-mcp--find-allowed-file id-file)))
          (cons :found allowed-file)
        (cons :disallowed nil))
    (if-let* ((file
               (catch 'found
                 (dolist (allowed-file org-mcp-allowed-files)
                   (when (file-exists-p allowed-file)
                     (org-mcp--with-org-file allowed-file
                       (when (org-find-property "ID" id)
                         (throw 'found
                                (expand-file-name
                                 allowed-file)))))))))
        (progn
          (when org-id-track-globally
            (ignore-errors
              (org-id-add-location id file)))
          (cons :found file))
      (cons :missing nil))))

(defun org-mcp--find-allowed-file-with-id (id)
  "Find an allowed file containing the Org ID.
Returns the expanded file path if found and allowed.
Throws a tool error if ID exists but file is not allowed, or if ID
is not found in the database or in any allowed file (see
`org-mcp--lookup-id-file' for the shared resolution logic, which
includes the fallback scan of `org-mcp-allowed-files')."
  (pcase-let ((`(,status . ,file) (org-mcp--lookup-id-file id)))
    (pcase status
      (:found file)
      (:disallowed (org-mcp--tool-id-disallowed-error id))
      (:missing (org-mcp--id-not-found-error id)))))

(defmacro org-mcp--with-uri-prefix-dispatch
    (uri headline-body id-body)
  "Dispatch tool URI handling based on prefix.
URI is the URI string to dispatch on.
HEADLINE-BODY is executed when URI starts with
`org-mcp--uri-headline-prefix', with the URI after the prefix bound
to `headline'.
ID-BODY is executed when URI starts with `org-mcp--uri-id-prefix',
with the URI after the prefix bound to `id'.
Throws an error if neither prefix matches."
  (declare (indent 1))
  `(if-let* ((id
              (org-mcp--extract-uri-suffix
               ,uri org-mcp--uri-id-prefix)))
       ,id-body
     (if-let* ((headline
                (org-mcp--extract-uri-suffix
                 ,uri org-mcp--uri-headline-prefix)))
         ,headline-body
       (org-mcp--tool-validation-error
        "Invalid resource URI format: %s"
        ,uri))))

(defun org-mcp--validate-file-access (filename)
  "Validate that FILENAME is in the allowed list.
FILENAME must be an absolute path.
Returns the full path if allowed, signals an error otherwise."
  (unless (file-name-absolute-p filename)
    (org-mcp--resource-validation-error "Path must be absolute: %s"
                                        filename))
  (let ((allowed-file (org-mcp--find-allowed-file filename)))
    (unless allowed-file
      (org-mcp--resource-file-access-error filename))
    allowed-file))

(defun org-mcp--extract-children (target-level)
  "Extract children at TARGET-LEVEL until next lower level heading."
  (let ((children '()))
    (save-excursion
      (while (and (re-search-forward "^\\*+ " nil t)
                  (>= (org-current-level) target-level))
        (when (= (org-current-level) target-level)
          (let* ((title (org-get-heading t t t t))
                 (child
                  `((title . ,title)
                    (level . ,target-level)
                    (children . []))))
            (push child children)))))
    (vconcat (nreverse children))))

(defun org-mcp--extract-headings ()
  "Extract heading structure from current org buffer."
  (let ((result '()))
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t) ; Find level 1 headings
      (let* ((title (org-get-heading t t t t))
             ;; Get level 2 children
             (children (org-mcp--extract-children 2))
             (heading
              `((title . ,title) (level . 1) (children . ,children))))
        (push heading result)))
    (vconcat (nreverse result))))

(defun org-mcp--generate-outline (file-path)
  "Generate JSON outline structure for FILE-PATH."
  (org-mcp--with-org-file file-path
    (let ((headings (org-mcp--extract-headings)))
      `((headings . ,headings)))))

(defun org-mcp--decode-file-path (encoded-path)
  "Decode special characters from ENCODED-PATH.
Specifically decodes %23 back to #."
  (replace-regexp-in-string "%23" "#" encoded-path))

(defun org-mcp--split-headline-uri (path-after-protocol)
  "Split PATH-AFTER-PROTOCOL into (file-path . headline-path).
PATH-AFTER-PROTOCOL is the part after `org-headline://'.
Returns (FILE . HEADLINE) where FILE is the decoded file path and
HEADLINE is the part after the fragment separator, or nil when the
fragment is absent or empty.
File paths with # characters should be encoded as %23.
A single trailing `/' on the decoded file path is stripped
regardless of whether a fragment is present, and an empty
fragment collapses to a nil HEADLINE, so all of
`org-headline://file.org', `org-headline://file.org/',
`org-headline://file.org#', `org-headline://file.org/#',
`org-headline://file.org#H' and `org-headline://file.org/#H'
resolve to equivalent (FILE . HEADLINE) pairs."
  (let* ((hash-pos (string-match "#" path-after-protocol))
         (file-encoded
          (if hash-pos
              (substring path-after-protocol 0 hash-pos)
            path-after-protocol))
         (file-decoded (org-mcp--decode-file-path file-encoded))
         ;; `> 1' leaves "/" alone — never collapse it to "".
         (file
          (if (and (> (length file-decoded) 1)
                   (eq
                    (aref file-decoded (1- (length file-decoded)))
                    ?/))
              (substring file-decoded 0 -1)
            file-decoded))
         (headline
          (and hash-pos
               (let ((fragment
                      (substring path-after-protocol (1+ hash-pos))))
                 (and (> (length fragment) 0) fragment)))))
    (cons file headline)))

(defun org-mcp--parse-resource-uri (uri)
  "Parse URI and return (file-path . headline-path).
Validates file access and returns expanded file path."
  (let (file-path
        headline-path)
    (org-mcp--with-uri-prefix-dispatch
        uri
      ;; Handle org-headline:// URIs
      (let* ((split-result (org-mcp--split-headline-uri headline))
             (filename (car split-result))
             (headline-path-str (cdr split-result))
             (allowed-file (org-mcp--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (setq headline-path
              (when headline-path-str
                (mapcar
                 #'url-unhex-string
                 (split-string headline-path-str "/")))))
      ;; Handle org-id:// URIs
      (progn
        (setq file-path (org-mcp--find-allowed-file-with-id id))
        (setq headline-path (list id))))
    (cons file-path headline-path)))

(defun org-mcp--parse-parent-uri (parent-uri)
  "Parse PARENT-URI into a (FILE-PATH PARENT-PATH PARENT-ID) list.
PARENT-PATH and PARENT-ID are mutually exclusive: at most one is
non-nil.  For `org-headline://' URIs, PARENT-PATH is a list of
unhex-decoded path components, or nil if the fragment is empty
or absent.  For `org-id://' URIs, PARENT-ID is the bare ID; an
`org-id://' URI is interpreted by `org-mcp--tool-add-todo' as a
child insert, so top-level insertion requires `org-headline://'
with empty or absent fragment.
Throws a validation error if PARENT-URI is malformed or if its
file is not in the allowed list."
  (let (file-path
        parent-path
        parent-id)
    (org-mcp--with-uri-prefix-dispatch
        parent-uri
      ;; Handle org-headline:// URIs
      (let* ((split-result (org-mcp--split-headline-uri headline))
             (filename (car split-result))
             (path-str (cdr split-result))
             (allowed-file (org-mcp--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (when path-str
          (setq parent-path
                (mapcar
                 #'url-unhex-string (split-string path-str "/")))))
      ;; Handle org-id:// URIs
      (progn
        (setq file-path (org-mcp--find-allowed-file-with-id id))
        (setq parent-id id)))
    (list file-path parent-path parent-id)))

(defun org-mcp--navigate-to-headline (headline-path)
  "Navigate to headline in HEADLINE-PATH.
HEADLINE-PATH is a list of headline titles forming a path.
Returns t if found, nil otherwise.  Point is left at the headline."
  (catch 'not-found
    (let ((search-start (point-min))
          (search-end (point-max))
          (current-level 0)
          (found nil)
          (path-index 0))
      (dolist (target-title headline-path)
        (setq found nil)
        (goto-char search-start)
        (while (and (not found)
                    (re-search-forward "^\\*+ " search-end t))
          (let ((title (org-get-heading t t t t))
                (level (org-current-level)))
            (when (and (string= title target-title)
                       (or (= current-level 0)
                           (= level (1+ current-level))))
              (setq found t)
              (setq current-level level)
              ;; Limit search to this subtree for nesting
              (when (< (1+ path-index) (length headline-path))
                (setq search-start (point))
                (setq search-end
                      (save-excursion
                        (org-end-of-subtree t t)
                        (point)))))))
        (unless found
          (throw 'not-found nil))
        (setq path-index (1+ path-index))))
    t))

(defun org-mcp--extract-headline-content ()
  "Extract content of current headline including the headline itself.
Point should be at the headline."
  (let ((start (line-beginning-position)))
    (org-end-of-subtree t t)
    ;; Remove trailing newline if present
    (when (and (> (point) start) (= (char-before) ?\n))
      (backward-char))
    (buffer-substring-no-properties start (point))))

(defun org-mcp--get-headline-content (file-path headline-path)
  "Get content for headline at HEADLINE-PATH in FILE-PATH.
HEADLINE-PATH is a list of headline titles to traverse.
Returns the content string or nil if not found."
  (org-mcp--with-org-file file-path
    (when (org-mcp--navigate-to-headline headline-path)
      (org-mcp--extract-headline-content))))

(defun org-mcp--goto-headline-from-uri (headline-path is-id)
  "Navigate to headline based on HEADLINE-PATH and IS-ID flag.
If IS-ID is non-nil, treats HEADLINE-PATH as containing an ID.
Otherwise, navigates using HEADLINE-PATH as title hierarchy."
  (if is-id
      ;; ID case - headline-path contains single ID
      (if-let* ((pos (org-find-property "ID" (car headline-path))))
          (goto-char pos)
        (org-mcp--id-not-found-error (car headline-path)))
    ;; Path case - headline-path contains title hierarchy
    (unless (org-mcp--navigate-to-headline headline-path)
      (org-mcp--headline-not-found-error headline-path))))

(defun org-mcp--get-content-by-id (file-path id)
  "Get content for org node with ID in FILE-PATH.
Returns the content string or nil if not found."
  (org-mcp--with-org-file file-path
    (when-let* ((pos (org-find-property "ID" id)))
      (goto-char pos)
      (org-mcp--extract-headline-content))))

(defun org-mcp--validate-todo-state (state field-name)
  "Validate STATE is a valid TODO keyword.
FIELD-NAME is the JSON wire-protocol name of the parameter being
validated (e.g. `\"todo_state\"' or `\"new_state\"'); it is embedded
into the validation error so the consumer can pinpoint which input
to fix."
  (let ((valid-states
         (delete
          "|"
          (org-remove-keyword-keys
           (apply #'append (mapcar #'cdr org-todo-keywords))))))
    (unless (member state valid-states)
      (org-mcp--tool-validation-error
       "Field %s must be one of %s, got: '%s'"
       field-name (mapconcat #'identity valid-states ", ") state))))

(defun org-mcp--validate-and-normalize-tags (tags)
  "Validate TAGS and return a normalized list of tag strings.
TAGS is the JSON-decoded `tags' value: nil, a string, or a vector
\(see `org-mcp--normalize-tags-to-list').
Validates:
- Tag names follow Org rules (alphanumeric, underscore, at-sign)
- Tags are in `org-tag-alist' or `org-tag-persistent-alist'
  (if either is configured)
- Tags don't violate mutual exclusivity groups in either alist
Signals error for invalid tags."
  (let ((tag-list (org-mcp--normalize-tags-to-list tags))
        (allowed-tags
         (append
          (mapcar
           #'org-mcp--extract-tag-from-alist-entry org-tag-alist)
          (mapcar
           #'org-mcp--extract-tag-from-alist-entry
           org-tag-persistent-alist))))
    ;; Remove special keywords like :startgroup
    (setq allowed-tags
          (cl-remove-if
           #'org-mcp--is-tag-group-keyword-p allowed-tags))
    ;; If tag alists are configured, validate against them
    (when allowed-tags
      (dolist (tag tag-list)
        (unless (member tag allowed-tags)
          (org-mcp--tool-validation-error
           "Tag not in configured tag alist: %s"
           tag))))
    ;; Always validate tag names follow Org's rules
    (dolist (tag tag-list)
      (unless (string-match "^[[:alnum:]_@]+$" tag)
        (org-mcp--tool-validation-error
         "Invalid tag name (must be alphanumeric, _, or @): %s"
         tag)))
    ;; Validate mutual exclusivity if tag-alist is configured
    (when org-tag-alist
      (org-mcp--validate-mutex-tag-groups tag-list org-tag-alist))
    (when org-tag-persistent-alist
      (org-mcp--validate-mutex-tag-groups
       tag-list org-tag-persistent-alist))
    tag-list))

(defun org-mcp--extract-tag-from-alist-entry (entry)
  "Extract tag name from an `org-tag-alist' ENTRY.
ENTRY can be a string or a cons cell (tag . key)."
  (if (consp entry)
      (car entry)
    entry))

(defun org-mcp--is-tag-group-keyword-p (tag)
  "Check if symbol TAG is a special keyword like :startgroup."
  (and (symbolp tag) (string-match "^:" (symbol-name tag))))

(defun org-mcp--parse-mutex-tag-groups (tag-alist)
  "Parse mutually exclusive tag groups from TAG-ALIST.
Returns a list of lists, where each inner list contains tags
that are mutually exclusive with each other."
  (let ((groups '())
        (current-group nil)
        (in-group nil))
    (dolist (entry tag-alist)
      (cond
       ;; Start of a mutex group
       ((eq entry :startgroup)
        (setq in-group t)
        (setq current-group '()))
       ;; End of a mutex group
       ((eq entry :endgroup)
        (when (and in-group current-group)
          (push current-group groups))
        (setq in-group nil)
        (setq current-group nil))
       ;; Inside a group - collect tags
       (in-group
        (let ((tag (org-mcp--extract-tag-from-alist-entry entry)))
          (when (and tag (not (org-mcp--is-tag-group-keyword-p tag)))
            (push tag current-group))))))
    groups))

(defun org-mcp--validate-mutex-tag-groups (tags tag-alist)
  "Validate that TAGS don't violate mutex groups in TAG-ALIST.
TAGS is a list of tag strings.
Errors if multiple tags from same mutex group."
  (let ((mutex-groups (org-mcp--parse-mutex-tag-groups tag-alist)))
    (dolist (group mutex-groups)
      (let ((tags-in-group
             (cl-intersection tags group :test #'string=)))
        (when (> (length tags-in-group) 1)
          (org-mcp--tool-validation-error
           "Tags %s are mutually exclusive (cannot use together)"
           (mapconcat (lambda (tag) (format "'%s'" tag)) tags-in-group
                      ", ")))))))

(defun org-mcp--validate-headline-title (title)
  "Validate that TITLE is not empty or whitespace-only.
Throws an MCP tool error if validation fails."
  (when (or (string-empty-p title)
            (string-match-p "^[[:space:]]*$" title)
            ;; Explicitly match NBSP for Emacs 27.2 compatibility
            ;; In Emacs 27.2, [[:space:]] doesn't match NBSP (U+00A0)
            (string-match-p "^[\u00A0]*$" title))
    (org-mcp--tool-validation-error
     "Headline title cannot be empty or contain only whitespace"))
  (when (string-match-p "[\n\r]" title)
    (org-mcp--tool-validation-error
     "Headline title cannot contain newlines")))

(defun org-mcp--validate-body-no-headlines (body level)
  "Validate that BODY doesn't contain headlines at LEVEL or higher.
LEVEL is the Org outline level (1 for *, 2 for **, etc).
Throws an MCP tool error if invalid headlines are found."
  ;; Build regex to match headlines at the current level or higher
  ;; For level 3, this matches ^*, ^**, or ^***
  ;; Matches asterisks + space/tab (headlines need content)
  (let ((regex (format "^\\*\\{1,%d\\}[ \t]" level)))
    (when (string-match regex body)
      (org-mcp--tool-validation-error
       "Body cannot contain headlines at level %d or higher"
       level))))

(defun org-mcp--validate-body-no-unbalanced-blocks (body)
  "Validate that BODY doesn't contain unbalanced blocks.
Uses a state machine: tracks if we're in a block, and which one.
Text inside blocks is literal and doesn't start/end other blocks.
Throws an MCP tool error if unbalanced blocks are found."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    ;; Org accepts block markers in any case (`#+BEGIN_SRC',
    ;; `#+begin_src', `#+Begin_Src', ...).  Pin `case-fold-search' to
    ;; `t' so the regex below catches all of them; the surrounding
    ;; `upcase' calls canonicalize the captured marker.
    (let ((case-fold-search t)
          (current-block nil)) ; Current block type or nil
      ;; Scan forward for all block markers
      ;; Block names can be any non-whitespace chars
      (while (re-search-forward "^#\\+\\(BEGIN\\|END\\)_\\(\\S-+\\)"
                                nil
                                t)
        (let ((marker-type (upcase (match-string 1)))
              (block-type (upcase (match-string 2))))
          (cond
           ;; Found BEGIN
           ((string= marker-type "BEGIN")
            (if current-block
                ;; Already in block - BEGIN is literal
                nil
              ;; Not in a block - enter this block
              (setq current-block block-type)))
           ;; Found END
           ((string= marker-type "END")
            (cond
             ;; Not in any block - this END is orphaned
             ((null current-block)
              (org-mcp--tool-validation-error
               "Orphaned END_%s without BEGIN_%s"
               block-type block-type))
             ;; In matching block - exit the block
             ((string= current-block block-type)
              (setq current-block nil))
             ;; In different block - this END is just literal text
             (t
              nil))))))
      ;; After scanning, check if we're still in a block
      (when current-block
        (org-mcp--tool-validation-error
         "Body contains unclosed %s block"
         current-block)))))

(defun org-mcp--normalize-tags-to-list (tags)
  "Normalize JSON-decoded TAGS parameter to a list of tag strings.
The MCP wire layer feeds `tags' as one of:
- nil (JSON `null' or absent field) -> returns nil
- vector (JSON array)               -> converts to list
- string (JSON string)              -> wraps in singleton list
Throws error for any other type."
  (cond
   ((null tags)
    nil)
   ((vectorp tags)
    (append tags nil))
   ((stringp tags)
    (list tags))
   (t
    (org-mcp--tool-validation-error "Invalid tags format: %s" tags))))

(defun org-mcp--skip-file-header-element ()
  "Skip one syntactic element of the file's leading header block.
Each call consumes at most one element starting at point and
returns non-nil if it consumed anything, nil otherwise.  Intended
to be driven by `(while (org-mcp--skip-file-header-element))' so
the loop terminates naturally at the first non-header line.

For the user-facing contract -- what counts as a header element,
the leading-whitespace policy, why drawer keywords are uniformly
consumed, why headings still require column 0 -- see the
`:description' string registered for `org-mcp--tool-add-todo' in
`org-mcp-enable' below.  This doc string covers the internal
function contract only.

Caller preconditions, NOT re-checked here:
- Point is at the beginning of a line.  The leading `^' in each
  regex makes a match impossible unless point is at a line
  beginning, so a mid-line call falls through to the nil branch
  rather than mis-reading the rest of the line as a header
  element.

Throws a validation error via `org-mcp--tool-validation-error'
naming the actual keyword if a drawer opener has no matching
`:END:'."
  (cond
   ((eobp)
    nil)
   ;; Broader than Org's comment grammar (`^# ' / `^#$') by design.
   ;; Consume any `#'-prefixed line (with optional leading whitespace,
   ;; matching Org's parser), including `#hashtag' paragraphs Org
   ;; parses as `paragraph' rather than `comment'.
   ((looking-at "^[ \t]*#")
    (forward-line)
    t)
   ((looking-at "^[ \t]*$")
    (forward-line)
    t)
   ((and (looking-at "^[ \t]*:\\([-_[:alnum:]]+\\):[ \t]*$")
         ;; A bare `:END:' with no preceding opener is not a drawer
         ;; opener -- fall through to ordinary content so the header
         ;; terminates before it.
         (not (string= (upcase (match-string 1)) "END")))
    (let ((drawer-name (match-string 1))
          ;; Match `:END:' case-insensitively to mirror Org's parser
          ;; (which accepts `:end:', `:End:', etc.).  Pinned here --
          ;; only the `:END:' checks below depend on case folding;
          ;; the opener regex above uses `[:alnum:]' and is case-safe
          ;; regardless.
          (case-fold-search t))
      (forward-line)
      (while (and (not (eobp))
                  (not (looking-at "^[ \t]*:END:[ \t]*$"))
                  (not (looking-at "^\\*+ ")))
        (forward-line))
      (unless (looking-at "^[ \t]*:END:[ \t]*$")
        (org-mcp--tool-validation-error
         (if (looking-at "^\\*+ ")
             (concat
              "Heading line inside :%s: drawer in file"
              " header block; drawers cannot contain"
              " headings")
           "Unterminated :%s: drawer in file header block")
         drawer-name))
      (forward-line)
      t))
   (t
    nil)))

(defun org-mcp--validate-file-header ()
  "Walk the file's leading header block, signaling drawer errors.
Drives `org-mcp--skip-file-header-element' from `point-min' inside
`save-excursion', so point and validation errors are decoupled from
the caller's positioning concerns.  Intended to be called once per
modifying tool invocation, before any insertion or navigation, so
that file-header integrity is checked uniformly regardless of whether
the call targets the top level or a parent heading.  Returns the
buffer position immediately past the header block."
  (save-excursion
    (goto-char (point-min))
    (while (org-mcp--skip-file-header-element))
    (point)))

(defun org-mcp--navigate-to-parent-or-top (parent-path parent-id)
  "Navigate to parent headline or top of file.
PARENT-PATH is a list of headline titles (or nil for top-level).
PARENT-ID is an ID string (or nil).
Returns parent level (integer) if parent exists, nil for top-level.
Assumes point is in an Org buffer."
  (if (or parent-path parent-id)
      (progn
        (org-mcp--goto-headline-from-uri
         (or (and parent-id (list parent-id)) parent-path) parent-id)
        ;; Save parent level before moving point
        ;; Ensure we're at the beginning of headline
        (org-back-to-heading t)
        (org-current-level))
    ;; No parent specified - top level
    ;; Skip past any header comments (#+TITLE, #+AUTHOR, etc.)
    (while (and (not (eobp)) (looking-at "^#\\+"))
      (forward-line))
    ;; Position correctly: if blank line after headers,
    ;; skip it; if headline immediately after, stay
    (when (and (not (eobp)) (looking-at "^[ \t]*$"))
      ;; On blank line after headers, skip
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (forward-line)))
    nil))

(defun org-mcp--position-for-new-child (after-uri parent-end)
  "Position point for inserting a new child under current heading.
AFTER-URI is an optional org-id:// URI of a sibling to insert after.
PARENT-END is the end position of the parent's subtree.
Assumes point is at parent heading.
If AFTER-URI is non-nil, positions after that sibling.
If nil, positions at end of parent's subtree.
An empty or whitespace-only AFTER-URI is rejected as a validation
error (distinct from absent); omit the key entirely to get default
placement.  A bare `org-id://' prefix with no UUID after it is also
rejected at the validation boundary; otherwise the empty extracted
UUID would surface downstream as the misleading `Sibling with ID
not found under parent' with an empty interpolated ID.
Throws validation error if AFTER-URI is invalid or sibling not found."
  (when (and
         after-uri
         (or (string-empty-p after-uri)
             (string-match-p "\\`[[:space:]]*\\'" after-uri)
             ;; Explicitly match NBSP for Emacs 27.2 compatibility
             ;; In Emacs 27.2, [[:space:]] doesn't match NBSP (U+00A0)
             (string-match-p "\\`[\u00A0]*\\'" after-uri)))
    (org-mcp--tool-validation-error
     (concat
      "Field after_uri must not be empty or whitespace-only; "
      "omit the key to insert as last child")))
  (if after-uri
      (progn
        ;; Parse after-uri to get the ID
        (let ((after-id
               (org-mcp--extract-uri-suffix
                after-uri org-mcp--uri-id-prefix))
              (found nil))
          (when (or (null after-id) (string-empty-p after-id))
            (org-mcp--tool-validation-error
             "Field after_uri is not %s: %s"
             org-mcp--uri-id-prefix after-uri))
          ;; Find the sibling with the specified ID
          (org-back-to-heading t) ;; At parent
          ;; Search sibling in parent's subtree
          ;; Move to first child
          (if (org-goto-first-child)
              (progn
                ;; Now search among siblings
                (while (and (not found) (< (point) parent-end))
                  (let ((current-id (org-entry-get nil "ID")))
                    (when (string= current-id after-id)
                      (setq found t)
                      ;; Move to sibling end
                      (org-end-of-subtree t t)))
                  (unless found
                    ;; Move to next sibling
                    (unless (org-get-next-sibling)
                      ;; No more siblings
                      (goto-char parent-end)))))
            ;; No children
            (goto-char parent-end))
          (unless found
            (org-mcp--tool-validation-error
             "Sibling with ID %s not found under parent"
             after-id))))
    ;; No after_uri - insert at end of parent's subtree.  Leave point
    ;; at the start of the following heading (or `point-max') so the
    ;; preceding `\n' acts as the separator.  An earlier
    ;; `(backward-char 1)' here moved point onto that `\n', which
    ;; tricked `ensure-newline' into adding a second one and inserted
    ;; a spurious blank line before the next heading.
    (org-end-of-subtree t t)))

(defun org-mcp--ensure-newline ()
  "Ensure there is a newline or buffer start before point."
  (unless (or (bobp) (looking-back "\n" 1))
    (insert "\n")))

(defun org-mcp--insert-heading (title parent-level)
  "Insert a new Org heading at the appropriate level.
TITLE is the headline text to insert.
PARENT-LEVEL is the parent's heading level (integer) if inserting
as a child, or nil if inserting at top-level.
Assumes point is positioned where the heading should be inserted.
After insertion, point is left on the heading line at end-of-line."
  (if parent-level
      ;; We're inside a parent
      (progn
        (org-mcp--ensure-newline)
        ;; Insert heading manually at parent level + 1
        ;; We don't use `org-insert-heading' because when parent has
        ;; no children, it creates a sibling of the parent instead of
        ;; a child
        (let ((heading-start (point)))
          (insert (make-string (1+ parent-level) ?*) " " title "\n")
          ;; Set point to heading for `org-todo' and `org-set-tags'
          (goto-char heading-start)
          (end-of-line)))
    ;; Top-level heading
    ;; Check if there are no headlines yet (empty buffer or only
    ;; headers before us)
    (let ((has-headline
           (save-excursion
             (goto-char (point-min))
             (re-search-forward "^\\*+ " nil t))))
      (if (not has-headline)
          (progn
            (org-mcp--ensure-newline)
            (insert "* "))
        ;; Has headlines - append at end of file (the tool description's
        ;; documented "end of file" placement for the no-fragment parent
        ;; URI).  `org-insert-heading' at the caller-positioned point
        ;; lands the new heading BEFORE the first existing heading,
        ;; contradicting the documented contract.
        (goto-char (point-max))
        (org-mcp--ensure-newline)
        (insert "* "))
      (insert title))))

(defun org-mcp--insert-body-after-heading (body)
  "Insert BODY after the heading line at point.
Moves to end-of-line, inserts `\\n' + BODY (adding a trailing
`\\n' if BODY doesn't already end with one), and drops the
heading's baked trailing `\\n' iff it was the buffer's last char
so EOF ends up with exactly one trailing newline; mid-file
placements keep the baked `\\n' as the blank-line separator
before the next heading.  Computing the EOF predicate up front
keeps the trim independent of any later code that might shift
`point-max'.

Caller preconditions, NOT re-checked here:
- Point is on the heading line whose section is being extended.
- BODY is a non-nil string."
  (end-of-line)
  (let ((heading-newline-at-eof
         (and (eq (char-after) ?\n) (= (1+ (point)) (point-max)))))
    (insert "\n" body)
    (unless (string-suffix-p "\n" body)
      (insert "\n"))
    (when heading-newline-at-eof
      (delete-char 1))))

(defun org-mcp--replace-body-content
    (old-body new-body body-content replace-all body-begin body-end)
  "Replace body content in the current buffer.
OLD-BODY is the substring to replace.
NEW-BODY is the replacement text.
BODY-CONTENT is the current body content string.
REPLACE-ALL if non-nil, replace all occurrences.
BODY-BEGIN is the buffer position where body starts.
BODY-END is the buffer position where body ends."
  (let ((new-body-content
         (cond
          ;; Special case: empty old_body with empty body
          ((and (string= old-body "")
                (string-match-p "\\`[[:space:]]*\\'" body-content))
           new-body)
          ;; Normal replacement.  Pin `case-fold-search' to nil so the
          ;; search agrees with the occurrence counter in the caller
          ;; (which already pins it to nil).
          (t
           (let ((case-fold-search nil))
             (if replace-all
                 (replace-regexp-in-string
                  (regexp-quote old-body) new-body body-content
                  t t)
               ;; The caller has already verified `old-body' occurs at
               ;; least once under the same `case-fold-search' regime,
               ;; so `string-match' is guaranteed to find a hit.
               (let ((pos
                      (string-match
                       (regexp-quote old-body) body-content)))
                 (cl-assert pos)
                 (concat
                  (substring body-content 0 pos) new-body
                  (substring body-content
                             (+ pos (length old-body)))))))))))

    ;; Replace the body content
    (if (< body-begin body-end)
        (delete-region body-begin body-end)
      ;; Empty body - ensure we're at the right position
      (goto-char body-begin))
    ;; Guard against EOF-mid-line insertion: when the empty-body
    ;; branch's body-begin is at point-max of a file lacking a
    ;; trailing newline, point sits right after the heading's last
    ;; char (or after the property drawer's `:END:'), and the bare
    ;; `(insert ...)' would concatenate the new body onto that line.
    (org-mcp--ensure-newline)
    (insert new-body-content)
    ;; Final-newline guarantee: leave the inserted body terminated
    ;; with `\n' so the file ends cleanly even when `new-body' does
    ;; not.  Skip when the next char is already `\n' to avoid
    ;; doubling up before a following heading.
    (unless (or (string-suffix-p "\n" new-body-content)
                (eq (char-after) ?\n))
      (insert "\n"))))

;; Tool handlers

(defun org-mcp--tool-get-todo-config ()
  "Return the TODO keyword configuration."
  (let ((seq-list '())
        (sem-list '()))
    (dolist (seq org-todo-keywords)
      (let* ((type (car seq))
             (keywords (cdr seq))
             (type-str (symbol-name type))
             (keyword-vec [])
             (before-bar t))
        (dolist (kw keywords)
          (if (string= kw "|")
              (setq before-bar nil)
            ;; Check if this is the last keyword and no "|" seen
            (let ((is-last-no-bar
                   (and before-bar (equal kw (car (last keywords))))))
              (when is-last-no-bar
                (setq keyword-vec (vconcat keyword-vec ["|"])))
              (push `((state
                       .
                       ,(car (org-remove-keyword-keys (list kw))))
                      (isFinal
                       . ,(or is-last-no-bar (not before-bar)))
                      (sequenceType . ,type-str))
                    sem-list)))
          (setq keyword-vec (vconcat keyword-vec (vector kw))))
        (push
         `((type . ,type-str) (keywords . ,keyword-vec)) seq-list)))
    (json-encode
     `((sequences . ,(vconcat (nreverse seq-list)))
       (semantics . ,(vconcat (nreverse sem-list)))))))

(defun org-mcp--tool-get-tag-config ()
  "Return the tag configuration as literal Elisp strings."
  (json-encode
   `((org-use-tag-inheritance
      .
      ,(prin1-to-string org-use-tag-inheritance))
     (org-tags-exclude-from-inheritance
      . ,(prin1-to-string org-tags-exclude-from-inheritance))
     (org-tag-alist . ,(prin1-to-string org-tag-alist))
     (org-tag-persistent-alist
      . ,(prin1-to-string org-tag-persistent-alist)))))

(defun org-mcp--tool-get-allowed-files ()
  "Return the list of allowed Org files."
  (json-encode `((files . ,(vconcat org-mcp-allowed-files)))))

(defun org-mcp--tool-update-todo-state (uri current_state new_state)
  "Update the TODO state of a headline at URI.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the updated headline.
CURRENT_STATE is the current TODO state (empty string for no state).
NEW_STATE is the new TODO state to set.

MCP Parameters:
  uri - URI of the headline to update
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  current_state - Expected current TODO state
                  Use empty string \"\" if headline has no TODO state
                  Must match actual state or tool will error
  new_state - New TODO state to set
              Must be a valid keyword from `org-todo-keywords'"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field current_state "current_state")
  (org-mcp--validate-string-field new_state "new_state")
  (pcase-let ((`(,file-path . ,headline-path)
               (org-mcp--parse-resource-uri uri)))
    (org-mcp--validate-todo-state new_state "new_state")
    (org-mcp--modify-and-save file-path "update"
                              `((previous_state . ,current_state)
                                (new_state . ,new_state))
      (org-mcp--validate-file-header)
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--uri-id-prefix uri))

      ;; Treat "" and nil as the same "no TODO state":
      ;; `org-get-todo-state' returns nil; the wire protocol uses "".
      (beginning-of-line)
      (let ((actual-state (org-get-todo-state))
            (expected-state
             (and (not (string-empty-p current_state))
                  current_state)))
        (unless (equal actual-state expected-state)
          (org-mcp--state-mismatch-error
           (if (string-empty-p current_state)
               "(no state)"
             current_state)
           (or actual-state "(no state)") "State")))

      ;; Update the state
      (org-todo new_state))))

(defun org-mcp--tool-add-todo
    (title todo_state tags body parent_uri &optional after_uri)
  "Add a new TODO item to an Org file.
Creates an Org ID for the new headline and returns its ID-based URI.
TITLE is the headline text.
TODO_STATE is the TODO state from `org-todo-keywords'.
TAGS is a single tag string or list of tag strings.
BODY is optional body text.
PARENT_URI is the URI of the parent item.
AFTER_URI is optional URI of sibling to insert after.

MCP Parameters:
  title - Headline text without TODO state or tags
          Cannot be empty or whitespace-only
          Cannot contain newlines
  todo_state - TODO state from `org-todo-keywords'
  tags - Tags to add (single string or array of strings)
         Single tag: \"urgent\"; multiple tags: [\"work\", \"urgent\"]
         Validated against `org-tag-alist' if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups
  body - Optional body text content
         Cannot contain headlines at same or higher level as the
         new item
         If #+BEGIN/#+END blocks are present, they must be balanced
  parent_uri - Parent item URI
               For top-level: org-headline://{absolute-path}
               For child: org-headline://{path}#{parent-path}
                          or org-id://{parent-uuid}
  after_uri - Sibling to insert after (optional)
              Must be org-id://{uuid} format
              If omitted, appends as last child of parent
              Empty or whitespace-only string is rejected; omit
              the key instead"
  (org-mcp--validate-string-field title "title")
  (org-mcp--validate-string-field todo_state "todo_state")
  (org-mcp--validate-string-field body "body" t)
  (org-mcp--validate-string-field parent_uri "parent_uri")
  (org-mcp--validate-string-field after_uri "after_uri" t)
  (org-mcp--validate-headline-title title)
  (org-mcp--validate-todo-state todo_state "todo_state")
  ;; Collapse empty `body' to nil locally: the validator accepts both
  ;; nil and "" (allow-nil=t), but the downstream body-insertion block
  ;; would emit two unwanted blank lines for "" because
  ;; `(insert "\n" "")' plus the trailing `(string-suffix-p "\n" "")'
  ;; guard both fire.
  (let ((effective-body (and body (not (string-empty-p body)) body)))
    (pcase-let ((tag-list (org-mcp--validate-and-normalize-tags tags))
                (`(,file-path ,parent-path ,parent-id)
                 (org-mcp--parse-parent-uri parent_uri)))

      ;; Add the TODO item
      (org-mcp--modify-and-save file-path "add TODO"
                                `((file
                                   .
                                   ,(file-name-nondirectory
                                     file-path))
                                  (title . ,title))
        (org-mcp--validate-file-header)
        (let ((parent-level
               (org-mcp--navigate-to-parent-or-top
                parent-path parent-id)))

          ;; Handle positioning after navigation to parent
          (when (or parent-path parent-id)
            (let ((parent-end
                   (save-excursion
                     (org-end-of-subtree t t)
                     (point))))
              (org-mcp--position-for-new-child after_uri parent-end)))

          ;; Validate body before inserting heading
          ;; Calculate the target level for validation
          (let ((target-level
                 (if (or parent-path parent-id)
                     ;; Child heading - parent level + 1
                     (1+ (or parent-level 0))
                   ;; Top-level heading
                   1)))

            ;; Validate body content if provided
            (when effective-body
              (org-mcp--validate-body-no-headlines
               effective-body target-level)
              (org-mcp--validate-body-no-unbalanced-blocks
               effective-body)))

          ;; Insert the new heading
          (org-mcp--insert-heading title parent-level)

          (org-todo todo_state)

          (when tag-list
            (org-set-tags tag-list))

          ;; Add body if provided
          (if effective-body
              (progn
                (org-mcp--insert-body-after-heading effective-body)
                ;; Move back to the heading for org-id-get-create
                ;; org-id-get-create requires point to be on a heading
                (org-back-to-heading t))
            ;; No body - ensure newline after heading
            (end-of-line)
            (unless (looking-at "\n")
              (insert "\n"))))))))

;; Resource handlers

(defun org-mcp--handle-outline-resource (params)
  "Handler for org://{filename}/outline template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (org-mcp--validate-file-access filename))
         (outline
          (org-mcp--generate-outline
           (expand-file-name allowed-file))))
    (json-encode outline)))

(defun org-mcp--handle-file-resource (params)
  "Handler for org://{filename} template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (org-mcp--validate-file-access filename)))
    (org-mcp--read-file (expand-file-name allowed-file))))

(defun org-mcp--handle-headline-resource (params)
  "Handler for org-headline://{filename} template.
PARAMS is an alist containing the filename parameter.
The filename parameter includes both file and headline path."
  (let* ((full-path (alist-get "filename" params nil nil #'string=))
         (split-result (org-mcp--split-headline-uri full-path))
         (filename (car split-result))
         (allowed-file (org-mcp--validate-file-access filename))
         (headline-path-str (cdr split-result))
         ;; Parse the path (URL-encoded headline path)
         (headline-path
          (when headline-path-str
            (mapcar
             #'url-unhex-string
             (split-string headline-path-str "/")))))
    (if headline-path
        (let ((content
               (org-mcp--get-headline-content
                allowed-file headline-path)))
          (unless content
            (org-mcp--resource-not-found-error
             "Headline" (mapconcat #'identity headline-path "/")))
          content)
      ;; No headline path means get entire file
      (org-mcp--read-file allowed-file))))

(defun org-mcp--handle-id-resource (params)
  "Handler for org-id://{uuid} template.
PARAMS is an alist containing the uuid parameter.
ID resolution shares the fallback-aware path used by the modifying
tools via `org-mcp--lookup-id-file', so an ID present in an allowed
file resolves even when `org-id-locations' has no record of it."
  (let ((id (alist-get "uuid" params nil nil #'string=)))
    (pcase-let ((`(,status . ,file) (org-mcp--lookup-id-file id)))
      (pcase status
        (:found (org-mcp--get-content-by-id file id))
        (:disallowed (org-mcp--resource-id-disallowed-error id))
        (:missing (org-mcp--resource-not-found-error "ID" id))))))

(defun org-mcp--tool-rename-headline (uri current_title new_title)
  "Rename headline title at URI from CURRENT_TITLE to NEW_TITLE.
Preserves the current TODO state and tags, creates an Org ID for the
headline if one doesn't exist.
Returns the ID-based URI for the renamed headline.

MCP Parameters:
  uri - URI of the headline to rename
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  current_title - Expected current title without TODO state or tags
                  Must match actual title or tool will error
                  Used to prevent race conditions
  new_title - New title without TODO state or tags
              Cannot be empty or whitespace-only
              Cannot contain newlines"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field current_title "current_title")
  (org-mcp--validate-string-field new_title "new_title")
  (org-mcp--validate-headline-title new_title)

  (pcase-let ((`(,file-path . ,headline-path)
               (org-mcp--parse-resource-uri uri)))

    ;; Rename the headline in the file
    (org-mcp--modify-and-save file-path "rename"
                              `((previous_title . ,current_title)
                                (new_title . ,new_title))
      (org-mcp--validate-file-header)
      ;; Navigate to the headline
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--uri-id-prefix uri))

      ;; Verify current title matches
      (beginning-of-line)
      (let ((actual-title (org-get-heading t t t t)))
        (unless (string= actual-title current_title)
          (org-mcp--state-mismatch-error
           current_title actual-title "Title")))

      (org-edit-headline new_title))))

(defun org-mcp--tool-edit-body
    (resource_uri old_body new_body &optional replace_all)
  "Edit body content of an Org node using partial string replacement.
RESOURCE_URI is the URI of the node to edit.
OLD_BODY is the substring to search for within the node's body.
         Use empty string \"\" to add content to an empty node.
NEW_BODY is the replacement text.
REPLACE_ALL if non-nil, replace all occurrences.

MCP Parameters:
  resource_uri - URI of the headline to edit
                 Formats:
                   - org-headline://{absolute-path}#{url-encoded-path}
                   - org-id://{uuid}
  old_body - Substring to find and replace within the body
             Matched case-sensitively
             Must appear exactly once unless replace_all is true
             Use empty string \"\" only for adding to empty nodes;
             in that case the node body must be empty or
             whitespace-only, otherwise the tool errors
  new_body - Replacement text
             Cannot introduce headlines at same or higher level
             Must maintain balanced #+BEGIN/#+END blocks
  replace_all - Replace all occurrences (optional, default false)
                When false, old_body must be unique in the body"
  (org-mcp--validate-string-field resource_uri "resource_uri")
  (org-mcp--validate-string-field old_body "old_body")
  (org-mcp--validate-string-field new_body "new_body")
  ;; Normalize falsy values to nil so the multi-occurrence guard
  ;; fires.  Accept `:json-false' (JSON boolean) and string "false"
  ;; (a common LLM-client typo); both are otherwise truthy in Elisp.
  (let ((replace_all
         (cond
          ((eq replace_all :json-false)
           nil)
          ((equal replace_all "false")
           nil)
          (t
           replace_all))))
    (org-mcp--validate-body-no-unbalanced-blocks new_body)

    (pcase-let ((`(,file-path . ,headline-path)
                 (org-mcp--parse-resource-uri resource_uri)))

      (org-mcp--modify-and-save file-path "edit body" nil
        (org-mcp--validate-file-header)
        (org-mcp--goto-headline-from-uri
         headline-path
         (string-prefix-p org-mcp--uri-id-prefix resource_uri))

        (org-mcp--validate-body-no-headlines
         new_body (org-current-level))

        ;; Skip past headline and properties
        (org-end-of-meta-data t)

        ;; Get body boundaries
        (let ((body-begin (point))
              (body-end nil)
              (body-content nil)
              (occurrence-count 0))

          ;; Find end of body (before next headline or end of subtree)
          (save-excursion
            (if (org-goto-first-child)
                ;; Has children - body ends before first child
                (setq body-end (point))
              ;; No children - body extends to end of subtree
              (org-end-of-subtree t)
              (setq body-end (point))))

          ;; Extract body content
          (setq body-content
                (buffer-substring-no-properties body-begin body-end))

          ;; Trim leading newline if present
          ;; (`org-end-of-meta-data' includes it)
          (when (and (> (length body-content) 0)
                     (= (aref body-content 0) ?\n))
            (setq body-content (substring body-content 1))
            (setq body-begin (1+ body-begin)))

          ;; Check if body is empty
          (when (string-match-p "\\`[[:space:]]*\\'" body-content)
            ;; Empty old_body + empty body -> add content
            (if (string= old_body "")
                ;; Treat as single replacement
                (setq occurrence-count 1)
              (org-mcp--tool-validation-error
               "Node has no body content")))

          ;; Count occurrences (unless already handled above)
          (unless (= occurrence-count 1)
            ;; Empty old_body with non-empty body is an error
            (if (and (string= old_body "")
                     (not
                      (string-match-p
                       "\\`[[:space:]]*\\'" body-content)))
                (org-mcp--tool-validation-error
                 "Cannot use empty old_body with non-empty body")
              ;; Normal occurrence counting
              (let ((case-fold-search nil)
                    (search-pos 0))
                (while (string-match
                        (regexp-quote old_body) body-content
                        search-pos)
                  (setq occurrence-count (1+ occurrence-count))
                  (setq search-pos (match-end 0))))))

          ;; Validate occurrences
          (cond
           ((= occurrence-count 0)
            (org-mcp--tool-validation-error "Body text not found: %s"
                                            old_body))
           ((and (> occurrence-count 1) (not replace_all))
            (org-mcp--tool-validation-error
             "Text appears %d times (use replace_all)"
             occurrence-count)))

          ;; Perform replacement.
          ;; `save-excursion' keeps point inside the edit target's
          ;; entry so `org-id-get-create' (in `complete-and-save')
          ;; resolves to the edit target.  Without it, body
          ;; insertion lands point in a different entry -- the
          ;; parent's first child (when the target has children) or
          ;; a strictly-deeper heading inside the new body
          ;; (permitted by `validate-body-no-headlines') -- and
          ;; `org-back-to-heading' inside `org-id-get-create'
          ;; would resolve to that entry.
          (save-excursion
            (org-mcp--replace-body-content
             old_body
             new_body
             body-content
             replace_all
             body-begin
             body-end)))))))

;; Tools duplicating resource templates

(defun org-mcp--tool-read-file (file)
  "Tool wrapper for org://{filename} resource template.
FILE is the absolute path to an Org file.

MCP Parameters:
  file - Absolute path to an Org file"
  (org-mcp--validate-string-field file "file")
  (org-mcp--handle-file-resource `(("filename" . ,file))))

(defun org-mcp--tool-read-outline (file)
  "Tool wrapper for org-outline://{filename} resource template.
FILE is the absolute path to an Org file.

MCP Parameters:
  file - Absolute path to an Org file"
  (org-mcp--validate-string-field file "file")
  (org-mcp--handle-outline-resource `(("filename" . ,file))))

(defun org-mcp--tool-read-headline (file headline_path)
  "Tool wrapper for org-headline://{filename}#{path} resource.
FILE is the absolute path to an Org file.
HEADLINE_PATH is the non-empty slash-separated path to
headline.

MCP Parameters:
  file - Absolute path to an Org file
  headline_path - Non-empty slash-separated path to headline
                  (string)
                  Only slashes in headline titles must be
                  encoded as %2F
                  Example: \"Project/Planning\" for nested headlines
                  Example: \"A%2FB Testing\" for headline titled
                  \"A/B Testing\"
                  To read entire files, use org-read-file
                  instead"
  (org-mcp--validate-string-field file "file")
  (org-mcp--validate-string-field headline_path "headline_path")
  (when (string-empty-p headline_path)
    (org-mcp--tool-validation-error
     "Field headline_path must be non-empty; use \
org-read-file tool to read entire files"))
  (let ((full-path (concat file "#" headline_path)))
    (org-mcp--handle-headline-resource `(("filename" . ,full-path)))))

(defun org-mcp--tool-read-by-id (uuid)
  "Tool wrapper for org-id://{uuid} resource template.
UUID is the UUID from headline's ID property.

MCP Parameters:
  uuid - UUID from headline's ID property"
  (org-mcp--validate-string-field uuid "uuid")
  (org-mcp--handle-id-resource `(("uuid" . ,uuid))))

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-todo-config
   :id "org-get-todo-config"
   :description
   "Get the TODO keyword configuration from the current Emacs
Org-mode settings.  Returns information about task state sequences
and their semantics.

Parameters: None

Returns JSON object with two arrays:
  sequences - Array of TODO keyword sequences, each containing:
    - type: Sequence type (e.g., \"sequence\", \"type\")
    - keywords: Array of keywords including \"|\" separator between
active and done states
  semantics - Array of keyword semantics, each containing:
    - state: The TODO keyword (e.g., \"TODO\", \"DONE\")
    - isFinal: Whether this is a final (done) state (boolean)
    - sequenceType: The sequence type this keyword belongs to

The \"|\" separator in sequences marks the boundary between active
states (before) and done states (after).  If no \"|\" is present,
the last keyword is treated as the done state.

Use this tool to understand the available task states in the Org
configuration before creating or updating TODO items."
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-tag-config
   :id "org-get-tag-config"
   :description
   "Get tag-related configuration from the current Emacs Org-mode
settings.  Returns literal Elisp variable values as strings for tag
configuration introspection.

Parameters: None

Returns JSON object with literal Elisp expressions (as strings) for:
  org-use-tag-inheritance - Controls tag inheritance behavior
  org-tags-exclude-from-inheritance - Tags that don't inherit
  org-tag-alist - List of allowed tags with optional key bindings and
                  groups
  org-tag-persistent-alist - Additional persistent tags (or nil)

The org-tag-alist format includes:
  - Simple tags: (\"tagname\" . key-char)
  - Group markers: :startgroup, :endgroup for mutually exclusive tags
  - Grouptags: :startgrouptag, :grouptags, :endgrouptag for tag
hierarchies

Use this tool to understand:
  - Which tags are allowed
  - Tag inheritance rules
  - Mutually exclusive tag groups
  - Tag hierarchy relationships

This helps validate tag usage and understand tag semantics before
adding or modifying tags on TODO items."
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-allowed-files
   :id "org-get-allowed-files"
   :description
   "Get the list of Org files accessible through the org-mcp
server.  Returns the configured allowed files exactly as specified in
org-mcp-allowed-files.

Parameters: None

Returns JSON object containing:
  files (array of strings): Absolute paths of allowed Org files

Example response:
  {
    \"files\": [
      \"/home/user/org/tasks.org\",
      \"/home/user/org/projects.org\",
      \"/home/user/notes/daily.org\"
    ]
  }

Empty configuration returns:
  {
    \"files\": []
  }

Use cases:
  - Discovery: What Org files can I access through MCP?
  - URI Construction: I need to build an org-headline:// URI - what's
    the exact path?
  - Access Troubleshooting: Why is my file access failing?
  - Configuration Verification: Did my org-mcp-allowed-files setting
    work correctly?"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-update-todo-state
   :id "org-update-todo-state"
   :description
   "Update the TODO state of an Org headline.  Changes the task state
while preserving the headline title, tags, and other properties.
Creates an Org ID property for the headline if one doesn't exist.
Modifies the file on disk; fails if an Emacs buffer visiting the
file has unsaved changes; ask the user to save the buffer and retry.
Validates the file's leading header block on every call; any
`:NAME:' line at column 0 (with optional leading whitespace and
any keyword) is treated as a drawer opener and must have a
matching `:END:'.  An unterminated drawer, or a drawer containing
a heading, is rejected as a validation error.

Returns JSON object:
  success - Always true on success (boolean)
  previous_state - The previous TODO state (string, empty for none)
  new_state - The new TODO state that was set (string)
  uri - ID-based URI (org-id://{uuid}) for the updated headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-add-todo
   :id "org-add-todo"
   :description
   "Add a new TODO item to an Org file at a specified location.
Creates the headline with TODO state, tags, and optional body content.
Automatically creates an Org ID property for the new headline.
Modifies the file on disk; fails if an Emacs buffer visiting the
file has unsaved changes; ask the user to save the buffer and retry.
Validates the file's leading header block on every call; any
`:NAME:' line at column 0 (with optional leading whitespace and
any keyword) is treated as a drawer opener and must have a
matching `:END:'.  An unterminated drawer, or a drawer containing
a heading, is rejected as a validation error.

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the new headline
  file - Filename (not full path) where item was added
  title - The headline title that was created

Positioning behavior:
  - With parent_uri only: Appends as last child of parent
  - With parent_uri + after_uri: Inserts immediately after specified
sibling
  - Top-level (parent_uri with no fragment): Adds at end of file."
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-rename-headline
   :id "org-rename-headline"
   :description
   "Rename an Org headline's title while preserving its TODO state,
tags, properties, and body content.  Creates an Org ID property for
the headline if one doesn't exist.
Modifies the file on disk; fails if an Emacs buffer visiting the
file has unsaved changes; ask the user to save the buffer and retry.
Validates the file's leading header block on every call; any
`:NAME:' line at column 0 (with optional leading whitespace and
any keyword) is treated as a drawer opener and must have a
matching `:END:'.  An unterminated drawer, or a drawer containing
a heading, is rejected as a validation error.

Returns JSON object:
  success - Always true on success (boolean)
  previous_title - The previous headline title (string)
  new_title - The new title that was set (string)
  uri - ID-based URI (org-id://{uuid}) for the renamed headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-edit-body
   :id "org-edit-body"
   :description
   "Edit the body content of an Org headline using partial string
replacement.  Finds and replaces a substring within the headline's
body text.  Creates an Org ID property for the headline if one doesn't
exist.
Modifies the file on disk; fails if an Emacs buffer visiting the
file has unsaved changes; ask the user to save the buffer and retry.
Validates the file's leading header block on every call; any
`:NAME:' line at column 0 (with optional leading whitespace and
any keyword) is treated as a drawer opener and must have a
matching `:END:'.  An unterminated drawer, or a drawer containing
a heading, is rejected as a validation error.

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the edited headline"
   :read-only nil
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-file
   :id "org-read-file"
   :description
   "Read complete raw content of an Org file. Returns entire file as
plain text with all formatting, properties, and structure preserved.
File must be in org-mcp-allowed-files.
Reads the file from disk; unsaved changes in an Emacs buffer visiting
the file are not reflected.

Returns: Plain text content of the entire Org file"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-outline
   :id "org-read-outline"
   :description
   "Get hierarchical structure of Org file as JSON outline. Returns
   all headline titles and nesting relationships at full depth. File
   must be in org-mcp-allowed-files.
Reads the file from disk; unsaved changes in an Emacs buffer visiting
the file are not reflected.

Returns: JSON object with hierarchical outline structure"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-headline
   :id "org-read-headline"
   :description
   "Read specific Org headline by hierarchical path. Returns headline
   with TODO state, tags, properties, body text, and all nested
   subheadings. File must be in org-mcp-allowed-files.
Reads the file from disk; unsaved changes in an Emacs buffer visiting
the file are not reflected.

Returns: Plain text content of the headline and its subtree"
   :read-only t
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-by-id
   :id "org-read-by-id"
   :description
   "Read Org headline by its unique ID property. More stable than
path-based access since IDs don't change when headlines are renamed
or moved. File containing the ID must be in org-mcp-allowed-files.
Reads the file from disk; unsaved changes in an Emacs buffer visiting
the file are not reflected.

Returns: Plain text content of the headline and its subtree"
   :read-only t
   :server-id org-mcp--server-id)

  ;; Register template resources for org files
  (mcp-server-lib-register-resource
   "org://{filename}" #'org-mcp--handle-file-resource
   :name "Org file"
   :description
   "Access the complete raw content of an Org file.  Returns the
entire file as plain text, preserving all formatting, properties, and
structure.  Reads the file from disk; unsaved changes in an Emacs
buffer visiting the file are not reflected.

URI format: org://{filename}
  filename - Absolute path to the Org file (required)

Returns: Plain text content of the entire Org file"
   :mime-type "text/plain"
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-resource
   "org-outline://{filename}" #'org-mcp--handle-outline-resource
   :name "Org file outline"
   :description
   "Get the hierarchical structure of an Org file as a JSON
outline.  Extracts headline titles and their nesting relationships up
to 2 levels deep.  Reads the file from disk; unsaved changes in an
Emacs buffer visiting the file are not reflected.

URI format: org-outline://{filename}
  filename - Absolute path to the Org file (required)

Returns: JSON object with structure:
  {
    \"headings\": [
      {
        \"title\": \"Top-level heading\",
        \"level\": 1,
        \"children\": [
          {
            \"title\": \"Subheading\",
            \"level\": 2,
            \"children\": []
          }
        ]
      }
    ]
  }

Depth limitation:
  - Level 1 headings (top-level) are extracted
  - Level 2 headings (direct children) are included
  - Deeper levels are not included (children arrays are empty)

Example URIs:
  org-outline:///home/user/notes/tasks.org
  org-outline:///Users/name/Documents/projects.org

Use this resource to:
  - Get document structure overview
  - Understand file organization without reading full content"
   :mime-type "application/json"
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-resource
   (concat org-mcp--uri-headline-prefix "{filename}")
   #'org-mcp--handle-headline-resource
   :name "Org headline content"
   :description
   "Access content of a specific Org headline by its path in the
file hierarchy.  Returns the headline and all its subheadings as
plain text.  Reads the file from disk; unsaved changes in an Emacs
buffer visiting the file are not reflected.

URI format: org-headline://{filename}#{headline-path}
  filename - Absolute path (# characters must be encoded as %23)
  # - Fragment separator (literal #, not encoded)
  headline-path - URL-encoded headline titles separated by /

URI encoding rules:
  - File path # → %23 (e.g., file#1.org → file%231.org)
  - Fragment separator → # (literal, marks start of headline path)
  - Headline title spaces → %20
  - Headline title # → %23 (e.g., Task #5 → Task%20%2345)
  - Path separator → / (literal, between nested headlines)

Encoding limitations:
  - ONLY # is encoded in file paths (minimal encoding for readability)
  - File paths with % characters should be avoided
  - Files named with %XX patterns (e.g., \"100%23done.org\") will fail
  - For such files, rename them or use org-id:// URIs instead
  - Headline paths use full URL encoding (all special chars encoded)

Returns: Plain text content including:
  - The headline itself with TODO state and tags
  - All properties drawer content
  - Body text
  - All nested subheadings (complete subtree)

Example URIs:
  org-headline:///home/user/tasks.org#Project%20Alpha
    → Top-level \"Project Alpha\" heading

  org-headline:///home/user/tasks.org#Project%20Alpha/Planning
    → \"Planning\" subheading under \"Project Alpha\"

  org-headline:///home/user/tasks.org#Issue%20%2342
    → Heading titled \"Issue #42\"

  org-headline:///home/user/file%231.org#Task%20%235
    → \"Task #5\" from file named \"file#1.org\"

  org-headline:///home/user/tasks.org
    → Entire file (no fragment means whole file)

Use this resource to:
  - Read specific sections of an Org file
  - Access headline content by hierarchical path
  - Get complete subtree including all children"
   :mime-type "text/plain"
   :server-id org-mcp--server-id)

  (mcp-server-lib-register-resource
   (concat org-mcp--uri-id-prefix "{uuid}")
   #'org-mcp--handle-id-resource
   :name "Org node by ID"
   :description
   "Access content of an Org headline by its unique ID property.
More stable than path-based access since IDs don't change when
headlines are renamed or moved.  Reads the file from disk; unsaved
changes in an Emacs buffer visiting the file are not reflected.

URI format: org-id://{uuid}
  uuid - Value of the headline's ID property (required)

How IDs work in Org:
  Headlines can have an ID property:
    * My Headline
    :PROPERTIES:
    :ID: 550e8400-e29b-41d4-a716-446655440000
    :END:

  The ID provides permanent, unique identification regardless of:
    - Headline title changes
    - Headline moving to different locations in file
    - File renaming or moving

Security and access:
  - The file containing the ID must be in org-mcp-allowed-files
  - Uses org-id database for ID-to-file lookup
  - Falls back to searching allowed files if database is stale

Returns: Plain text content including:
  - The headline itself with TODO state and tags
  - All properties drawer content
  - Body text
  - All nested subheadings (complete subtree)

Example URIs:
  org-id://550e8400-e29b-41d4-a716-446655440000
    → Headline with that ID property

Use this resource to:
  - Access headlines by stable identifier
  - Reference content that may be renamed or moved
  - Build cross-references between Org nodes"
   :mime-type "text/plain"
   :server-id org-mcp--server-id))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-tool
   "org-get-todo-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-tag-config" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-get-allowed-files" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-update-todo-state" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-add-todo" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-rename-headline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-edit-body" org-mcp--server-id)
  ;; Unregister workaround tools
  (mcp-server-lib-unregister-tool "org-read-file" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-read-outline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool
   "org-read-headline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-read-by-id" org-mcp--server-id)
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource
   "org://{filename}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   "org-outline://{filename}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   (concat
    org-mcp--uri-headline-prefix "{filename}")
   org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   (concat org-mcp--uri-id-prefix "{uuid}") org-mcp--server-id))

(provide 'org-mcp)
;;; org-mcp.el ends here
