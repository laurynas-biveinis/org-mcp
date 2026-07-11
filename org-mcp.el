;;; org-mcp.el --- MCP server for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis <laurynas.biveinis@gmail.com>
;; Keywords: convenience, files, matching, outlines
;; Version: 0.10.0
;; Package-Requires: ((emacs "28.2") (mcp-server-lib "0.4.0"))
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
(require 'org-archive)
(require 'org-id)
(require 'org-agenda)
(require 'calendar)
(require 'url-util)
(require 'subr-x)

(defcustom org-mcp-allowed-files nil
  "List of absolute paths to Org files that can be accessed via MCP."
  :type '(repeat file)
  :group 'org-mcp)

(defconst org-mcp--server-id "org-mcp"
  "Server ID for org-mcp MCP server registration.")

(defconst org-mcp--version "0.10.0"
  "Version reported as `serverInfo.version' in the MCP `initialize' result.")

(defconst org-mcp--uri-headline-prefix "org-headline://"
  "URI prefix for headline resources.")

(defconst org-mcp--uri-id-prefix "org-id://"
  "URI prefix for ID-based resources.")

(defconst org-mcp--instructions
  "Cross-cutting behavior shared by org-mcp's tools and resources.

File-modifying tools (org-add-todo, org-update-todo-state,
org-edit-headline, org-edit-body, org-refile-headline) modify the
file on disk; they fail if an Emacs buffer visiting the file has
unsaved changes; ask the user to save the buffer and retry.  They
validate the file's leading header block on every call; any `:NAME:'
line at column 0 (with optional leading whitespace and any keyword) is
treated as a drawer opener and must have a matching `:END:'.  An
unterminated drawer, or a drawer containing a heading, is rejected as
a validation error.  A `#+BEGIN_NAME' opener with no matching
`#+END_NAME', or a `#+BEGIN:' dynamic block with no matching `#+END:',
is rejected too.

org-archive-subtree and org-refile-headline likewise modify files on
disk and fail on unsaved changes, but can write two files --
org-archive-subtree the source and the destination archive file,
org-refile-headline the source and (for a cross-file move) the target
file -- so a buffer visiting either one with unsaved changes blocks the
operation; ask the user to save and retry.

Read tools (org-read-file, org-read-outline, org-read-headline,
org-read-by-id, org-grep, org-find-tagged-ancestor) and all resources
read the file from disk; unsaved changes in an Emacs buffer visiting
the file are not reflected.

org-read-headline, org-read-by-id, and the corresponding
org-headline:// (with a headline path) and org-id:// resources return
a JSON object of `headline_path' (the ancestor chain from the
outermost ancestor down to the read headline itself, in the same node
shape as org-grep's `headline_path') and `content' (the raw subtree
text); whole-file org-headline:// reads return raw file content."
  "Server-level MCP `initialize' instructions for org-mcp.
Holds the guidance that would otherwise be repeated in every
tool and resource description.")

(defun org-mcp--blank-or-nbsp-only-p (s)
  "Return non-nil if S is empty or has only whitespace and NBSP chars.
NBSP (U+00A0) is listed explicitly in the character class so it is
always rejected, independent of whether `[[:space:]]' matches NBSP --
that depends on the active syntax table and the Emacs version."
  (string-match-p "\\`[[:space:]\u00A0]*\\'" s))

(defun org-mcp--extract-uri-suffix (uri prefix)
  "Extract suffix from URI after PREFIX.
Returns the suffix string if URI starts with PREFIX and the suffix
carries meaningful content (i.e. is not empty, whitespace-only, or
NBSP-only) and does not itself contain another URI scheme separator
`://', nil otherwise.  A bare prefix URI (e.g. `org-id://') or one
whose suffix is only blank characters cannot identify a resource;
a doubly-prefixed URI like `org-id://org-headline://foo' indicates
a malformed input rather than an ID lookup.  Returning nil for
both cases makes the dispatch helpers classify them as malformed
URIs at the validation boundary instead of dispatching a
no-meaningful-content lookup downstream."
  (when (string-prefix-p prefix uri)
    (let ((suffix (substring uri (length prefix))))
      (unless (org-mcp--blank-or-nbsp-only-p suffix)
        (unless (string-match-p "://" suffix)
          suffix)))))

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

(defun org-mcp--normalize-json-boolean (value)
  "Return nil for a JSON-falsy VALUE, VALUE unchanged otherwise.
`json.el' decodes JSON `false' to `:json-false', which is truthy in
Elisp; the string \"false\" is a common client typo.  Both normalize
to nil so callers can test the result with ordinary truthiness."
  (cond
   ((eq value :json-false)
    nil)
   ((equal value "false")
    nil)
   (t
    value)))

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

(defun org-mcp--agenda-allowed-file-list ()
  "Return absolute truenames of regular files in `org-mcp-allowed-files'.
Directory entries are excluded so they cannot be expanded by
`org-agenda-list' into files outside the allow-list."
  (let (out)
    (dolist (f org-mcp-allowed-files)
      (when (and f (stringp f) (file-regular-p f))
        (push (file-truename f) out)))
    (nreverse out)))

(defconst org-mcp--agenda-buffer-name " *org-mcp agenda*"
  "Name of the private buffer `org-mcp--agenda-buffer-text' builds into.
The leading space keeps it off `buffer-list'.")

(defun org-mcp--agenda-node-at-marker (marker)
  "Serialize the heading at MARKER to a bare node, or nil if unresolvable.
Returns the deepest node of `org-mcp--headline-path-nodes' -- the same
node shape `org-grep' emits -- for the heading MARKER points at, or nil
when MARKER's buffer is dead, its file is not in `org-mcp-allowed-files',
or MARKER is not on a heading.  The node's URI is built from the
allowed-files entry's canonical path (via `org-mcp--find-allowed-file'),
byte-identical to the URIs org-grep and the read tools emit; a heading
in a file outside the allow-list (reachable only via a custom command
that sets its own `org-agenda-files') is dropped from the structured
items -- it still appears in the raw agenda text."
  (let ((buf (marker-buffer marker)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when-let* ((file (buffer-file-name))
                    (allowed (org-mcp--find-allowed-file file)))
          (save-excursion
            (goto-char marker)
            (when (org-at-heading-p)
              (car
               (last (org-mcp--headline-path-nodes allowed))))))))))

(defun org-mcp--agenda-blocks ()
  "Walk the current agenda buffer into a list of block alists.
Each block is an alist ((header . STRING-or-nil) (items . VECTOR)).  A
line carrying the `org-agenda-structural-header' text property opens a
new block whose header is that line's trimmed text; a line carrying an
`org-hd-marker' becomes an item node (via
`org-mcp--agenda-node-at-marker') in the current block.  Items seen
before the first structural header form an initial block with a nil
header.  A heading that renders on several lines of one block (a
multi-day span entry, a repeated deadline warning) is emitted once:
item nodes are deduplicated within a block by their source-heading
identity (the `org-hd-marker' buffer and position), since the bare node
carries no per-line date to tell the copies apart.  Deduping on
identity rather than `uri' keeps two distinct headings that share a
title path (and thus an `org-headline://' URI) from collapsing.  Lines
that resolve to no heading (date headers, diary, clock, separators)
contribute no item but remain in the agenda text."
  (let ((blocks '())
        (header nil)
        (items '())
        (seen (make-hash-table :test 'equal))
        (in-block nil))
    (cl-flet
     ((flush
       ()
       (when (or in-block items)
         (push `((header . ,header)
                 (items . ,(vconcat (nreverse items))))
               blocks))))
     (goto-char (point-min))
     (while (not (eobp))
       (let ((bol (line-beginning-position)))
         (cond
          ((get-text-property bol 'org-agenda-structural-header)
           (flush)
           (setq
            header
            (string-trim
             (buffer-substring-no-properties bol (line-end-position)))
            items nil
            in-block t)
           (clrhash seen))
          ((get-text-property bol 'org-hd-marker)
           (let* ((marker (get-text-property bol 'org-hd-marker))
                  (key
                   (cons
                    (marker-buffer marker) (marker-position marker))))
             (when-let* ((fresh (not (gethash key seen)))
                         (node
                          (org-mcp--agenda-node-at-marker marker)))
               (puthash key t seen)
               (push node items))))))
       (forward-line 1))
     (flush))
    (nreverse blocks)))

(defun org-mcp--agenda-collect (agenda-files build-fn)
  "Build an agenda over AGENDA-FILES via BUILD-FN under agenda isolation.
BUILD-FN takes no arguments and populates the private agenda buffer
\(e.g. calls `org-agenda-list' or `org-agenda' with a dispatch key).
Return a list (TEXT STARTING-DAY BLOCKS): TEXT is the agenda buffer as
a plain string, STARTING-DAY is the absolute day number the agenda
anchored on (`org-starting-day', nil for a non-dated view), and BLOCKS
is `org-mcp--agenda-blocks'.  Binds `org-agenda-files' and a private
agenda buffer name so the user window layout is not disturbed.
Building the agenda mutates several global agenda bookkeeping variables
-- most damagingly it invalidates the markers of any live interactive
agenda via `org-agenda-reset-markers' -- so they are let-bound here,
which restores the user's values on exit and leaves a concurrent
interactive agenda intact.  `org-agenda-start-day' is bound to nil so
an omitted reference day anchors on today rather than the user's
global, and `org-agenda-contributing-files' is bound to nil so the
build does not overwrite the user's live value.  Any active agenda
restriction lock is neutralized for the build: `org-agenda-restrict'
and `org-agenda-overriding-restriction' are bound to nil, and the
`org-restrict' symbol property on `org-agenda-files' (which the agenda
honors above the dynamic variable) is cleared and restored, so the
agenda is always built from AGENDA-FILES alone.  The caller must ensure
AGENDA-FILES is non-nil."
  (cl-assert agenda-files)
  (let*
      ((buffer-name org-mcp--agenda-buffer-name)
       (org-agenda-files agenda-files)
       (org-agenda-buffer-tmp-name buffer-name)
       ;; Single-command dispatch names its buffer from the tmp-name
       ;; above, but the composite path (`org-agenda-run-series')
       ;; names it from `org-agenda-buffer-name', so bind that to the
       ;; private name too or a composite view escapes into the
       ;; user's real "*Org Agenda*" buffer.
       (org-agenda-buffer-name buffer-name)
       (org-agenda-buffer org-agenda-buffer)
       (org-agenda-pre-window-conf nil)
       (org-agenda-window-setup 'current-window)
       (org-agenda-sticky nil)
       (org-agenda-markers nil)
       (org-agenda-this-buffer-name nil)
       (org-agenda-last-prefix-arg nil)
       (org-todo-keywords-for-agenda nil)
       (org-done-keywords-for-agenda nil)
       (org-agenda-start-day nil)
       (org-agenda-contributing-files nil)
       (org-agenda-restrict nil)
       (org-agenda-overriding-restriction nil)
       (org-agenda-compact-blocks nil)
       (saved-restrict (get 'org-agenda-files 'org-restrict)))
    (save-window-excursion
      (unwind-protect
          (progn
            (put 'org-agenda-files 'org-restrict nil)
            (funcall build-fn)
            (let ((buf (get-buffer buffer-name)))
              (unless buf
                (org-mcp--tool-validation-error
                 "Agenda command produced no agenda buffer; it may build a sparse tree or prompt for input"))
              (with-current-buffer buf
                (list
                 (buffer-substring-no-properties
                  (point-min) (point-max))
                 org-starting-day (org-mcp--agenda-blocks)))))
        (put 'org-agenda-files 'org-restrict saved-restrict)
        (when-let* ((buf (get-buffer buffer-name)))
          (kill-buffer buf))))))

(defun org-mcp--agenda-buffer-text (agenda-files start-day span)
  "Build `org-agenda-list' for AGENDA-FILES, period START-DAY and SPAN.
Thin wrapper over `org-mcp--agenda-collect'; see it for the return
shape and the agenda-isolation guarantees."
  (org-mcp--agenda-collect
   agenda-files (lambda () (org-agenda-list nil start-day span))))

(defun org-mcp--agenda-start-day (date span)
  "Return the `org-agenda-list' start day for DATE and SPAN.
DATE is the user's reference-day string or nil.  For the month span
the result is the absolute day number of the first of DATE's calendar
month (today's month when DATE is nil), so the agenda spans that whole
calendar month, matching `org-agenda-month-view'.  For other spans
DATE is returned unchanged for `org-agenda-list' to resolve."
  (if (eq span 'month)
      (let ((decoded
             (decode-time
              (if date
                  (org-read-date nil t date)
                (current-time)))))
        (calendar-absolute-from-gregorian
         (list (nth 4 decoded) 1 (nth 5 decoded))))
    date))

(defun org-mcp--agenda-iso-date (absolute-day)
  "Format ABSOLUTE-DAY, a Gregorian absolute day number, as YYYY-MM-DD."
  (let ((gregorian (calendar-gregorian-from-absolute absolute-day)))
    (format "%04d-%02d-%02d"
            (nth 2 gregorian)
            (nth 0 gregorian)
            (nth 1 gregorian))))

(defun org-mcp--refresh-file-buffers
    (file-path &optional except-buffer)
  "Refresh all buffers visiting FILE-PATH.
Preserves narrowing state across the refresh operation.
EXCEPT-BUFFER, when non-nil, is skipped: callers that have just
written FILE-PATH from a buffer pass that buffer so it is not
reverted into itself (and its revert hooks not fired needlessly)."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when-let* ((buf-file (buffer-file-name)))
        (when (and (string= buf-file file-path)
                   (not (eq buf except-buffer)))
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

(defun org-mcp--complete-and-save (response-alist)
  "Create ID if needed, save the visited file, return JSON.
Creates or gets an Org ID for the current headline and returns it.
Saves the current buffer to its visited file (`buffer-file-name'), so
the buffer must visit the target file (as set up by
`org-mcp--with-visiting-org-file').
Point must be at (or within) the entry whose `org-id://' URI should
be returned, as `org-id-get-create' uses point to locate the entry.
RESPONSE-ALIST is an alist of response fields."
  (cl-assert (buffer-file-name))
  (let ((id (org-id-get-create))
        (file-path (buffer-file-name)))
    (write-region (point-min) (point-max) file-path)
    (org-mcp--refresh-file-buffers file-path (current-buffer))
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

(defmacro org-mcp--with-visiting-org-file
    (file-path operation &rest body)
  "Execute BODY in a temp buffer visiting Org FILE-PATH.
OPERATION is a string describing the operation for error messages.

Validates FILE-PATH has no unsaved changes (errors via
`org-mcp--fail-if-modified' if so), then sets up a temp buffer with
FILE-PATH's contents in `org-mode', `set-visited-file-name' pointing
at FILE-PATH, and point at `point-min', and runs BODY there.  Unlike
`org-mcp--with-org-file', the buffer visits FILE-PATH so BODY may save
it, and the unsaved-change guard runs first.

The visiting buffer's modified flag is cleared before `with-temp-buffer'
kills it.  Setup (`set-visited-file-name' then `insert-file-contents',
no VISIT arg) leaves the buffer modified before BODY runs, and BODY's own
save (`write-region', no VISIT arg) does not clear it, so it is modified
whenever `with-temp-buffer' kills it.  Killing a modified file-visiting
buffer makes a live Emacs pop `Buffer FILE<2> modified; kill anyway?'.
The `FILE<2>' name arises because the buffer `set-visited-file-name' creates
collides with any buffer the user already has visiting FILE-PATH.  The
`unwind-protect' spans setup and BODY, so the flag is cleared however the
macro exits -- normal return, or a signal from setup or BODY alike -- and
the teardown kill is silent."
  (declare (indent 2) (debug (form form body)))
  (let ((file-var (gensym "file-path"))
        (buf-var (gensym "buf")))
    `(let ((,file-var ,file-path))
       (org-mcp--fail-if-modified ,file-var ,operation)
       (with-temp-buffer
         (let ((,buf-var (current-buffer)))
           (unwind-protect
               (progn
                 (set-visited-file-name ,file-var t)
                 (insert-file-contents ,file-var)
                 (org-mode)
                 (goto-char (point-min))
                 ,@body)
             (with-current-buffer ,buf-var
               (set-buffer-modified-p nil))))))))

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
- Must NOT call `org-mcp--complete-and-save' itself; the macro
  appends it after BODY.  Calling it inside BODY would write the
  file twice and return a stale JSON shape.
- Must leave point at or within the entry whose `org-id://' URI
  should be returned to the caller.
- BODY's return value is discarded; the macro's return value is
  the JSON from `org-mcp--complete-and-save'.
- Errors signalled in BODY propagate up unmodified (no rollback)."
  (declare (indent 3) (debug (form form form body)))
  `(org-mcp--with-visiting-org-file ,file-path ,operation
     ,@body
     (org-mcp--complete-and-save ,response-alist)))

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

(defun org-mcp--heading-raw-fields ()
  "Return a plist of metadata for the Org heading at point.
Keys: :title :todo :priority :tags (local tags only) :scheduled
:deadline :id.  Point must be on the heading line."
  (let ((priority (nth 3 (org-heading-components))))
    `(:title
      ,(org-get-heading t t t t)
      :todo ,(org-get-todo-state)
      :priority ,(and priority (char-to-string priority))
      :tags ,(org-get-tags nil t)
      :scheduled ,(org-entry-get nil "SCHEDULED")
      :deadline ,(org-entry-get nil "DEADLINE")
      :id ,(org-entry-get nil "ID"))))

(defun org-mcp--heading-node-fields (raw)
  "Return the JSON node field alist for RAW, without `uri'.
RAW is a plist as returned by `org-mcp--heading-raw-fields'.  Keys:
`title', `todo', `priority', `tags' (local only), `scheduled',
`deadline'."
  `((title . ,(plist-get raw :title))
    (todo . ,(plist-get raw :todo))
    (priority . ,(plist-get raw :priority))
    (tags . ,(vconcat (plist-get raw :tags)))
    (scheduled . ,(plist-get raw :scheduled))
    (deadline . ,(plist-get raw :deadline))))

(defun org-mcp--heading-node (file-path title-path raw)
  "Build a JSON node alist from RAW.
RAW is a plist as returned by `org-mcp--heading-raw-fields'.  The
`uri' is `org-id://' when RAW carries an ID, else an
`org-headline://' URI for FILE-PATH and TITLE-PATH (the ancestor
titles from the root down to and including this heading)."
  (let ((id (plist-get raw :id)))
    (append
     (org-mcp--heading-node-fields raw)
     `((uri
        .
        ,(if id
             (concat org-mcp--uri-id-prefix id)
           (org-mcp--build-headline-uri file-path title-path)))))))

(defun org-mcp--headline-path-nodes (file-path)
  "Return the chain of heading nodes from root to the heading at point.
Each node is an alist as built by `org-mcp--heading-node', ordered
root first, the heading at point last; FILE-PATH is used to build
`org-headline://' URIs.  Point must be on a heading line; it is not
moved."
  (let ((raws
         (save-excursion
           (let ((acc (list (org-mcp--heading-raw-fields))))
             (while (org-up-heading-safe)
               (push (org-mcp--heading-raw-fields) acc))
             acc))))
    (cl-loop
     for
     raw
     in
     raws
     for
     title-path
     =
     (list (plist-get raw :title))
     then
     (append title-path (list (plist-get raw :title)))
     collect
     (org-mcp--heading-node file-path title-path raw))))

(defun org-mcp--extract-children
    (target-level parent-title-path file-path)
  "Extract children at TARGET-LEVEL until next lower level heading.
PARENT-TITLE-PATH is the list of ancestor titles above TARGET-LEVEL
and FILE-PATH is the Org file; both are used to build each child's
URI."
  (let ((children '()))
    (save-excursion
      (while (and (re-search-forward "^\\*+ " nil t)
                  (>= (org-current-level) target-level))
        (when (= (org-current-level) target-level)
          (let* ((raw (org-mcp--heading-raw-fields))
                 (title-path
                  (append
                   parent-title-path (list (plist-get raw :title))))
                 (node
                  (org-mcp--heading-node file-path title-path raw)))
            (push (append
                   node `((level . ,target-level) (children . [])))
                  children)))))
    (vconcat (nreverse children))))

(defun org-mcp--extract-headings (file-path)
  "Extract heading structure from current org buffer.
FILE-PATH is the Org file, used to build each node's URI."
  (let ((result '()))
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t) ; Find level 1 headings
      (let* ((raw (org-mcp--heading-raw-fields))
             (title-path (list (plist-get raw :title)))
             (node (org-mcp--heading-node file-path title-path raw))
             ;; Get level 2 children
             (children
              (org-mcp--extract-children 2 title-path file-path)))
        (push
         (append node `((level . 1) (children . ,children))) result)))
    (vconcat (nreverse result))))

(defun org-mcp--generate-outline (file-path)
  "Generate JSON outline structure for FILE-PATH."
  (org-mcp--with-org-file file-path
    (let ((headings (org-mcp--extract-headings file-path)))
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

(defun org-mcp--parse-resource-uri-with-headline (uri)
  "Parse URI like `org-mcp--parse-resource-uri', requiring a headline part.
Returns the same (file-path . headline-path) cons; signals a tool
validation error for a whole-file URI with no headline fragment."
  (let ((parsed (org-mcp--parse-resource-uri uri)))
    (unless (cdr parsed)
      (org-mcp--tool-validation-error
       "URI must identify a headline, not a whole file"))
    parsed))

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
    (when (and (> (point) start) (eq (char-before) ?\n))
      (backward-char))
    (buffer-substring-no-properties start (point))))

(defun org-mcp--headline-with-path-json (file-path)
  "Return JSON for the headline at point in FILE-PATH.
The JSON object carries `headline_path' (the chain of heading nodes
from root to this headline, as in org-grep results) and `content'
\(the raw subtree text).  Point must be on the heading line."
  (let ((nodes (org-mcp--headline-path-nodes file-path)))
    (json-encode
     `((headline_path . ,(vconcat nodes))
       (content . ,(org-mcp--extract-headline-content))))))

(defun org-mcp--get-headline-content (file-path headline-path)
  "Get content for headline at HEADLINE-PATH in FILE-PATH.
HEADLINE-PATH is a list of headline titles to traverse.
Returns a JSON string with `headline_path' and `content', or nil if
not found."
  (org-mcp--with-org-file file-path
    (when (org-mcp--navigate-to-headline headline-path)
      (org-mcp--headline-with-path-json file-path))))

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
Returns a JSON string with `headline_path' and `content', or nil if
not found."
  (org-mcp--with-org-file file-path
    (when-let* ((pos (org-find-property "ID" id)))
      (goto-char pos)
      (org-mcp--headline-with-path-json file-path))))

(defun org-mcp--validate-todo-state (state field-name)
  "Validate STATE is a valid TODO keyword.
The empty string \"\" means no TODO keyword (Org's empty state,
accepted by `org-todo') and is always valid.
FIELD-NAME is the JSON wire-protocol name of the parameter being
validated (e.g. `\"todo_state\"' or `\"new_state\"'); it is embedded
into the validation error so the consumer can pinpoint which input
to fix."
  (unless (string-empty-p state)
    (let ((valid-states
           (delete
            "|"
            (org-remove-keyword-keys
             (apply #'append (mapcar #'cdr org-todo-keywords))))))
      (unless (member state valid-states)
        (org-mcp--tool-validation-error
         "Field %s must be one of %s, got: '%s'"
         field-name
         (mapconcat #'identity valid-states ", ")
         state)))))

(defun org-mcp--validate-tag-charset (tag-list)
  "Signal a tool error if any tag in TAG-LIST violates Org's name charset.
Org tag names allow only alphanumerics, underscore, and at-sign."
  (dolist (tag tag-list)
    (unless (string-match-p "\\`[[:alnum:]_@]+\\'" tag)
      (org-mcp--tool-validation-error
       "Invalid tag name (must be alphanumeric, _, or @): %s"
       tag))))

(defun org-mcp--validate-and-normalize-tags (tags)
  "Validate TAGS and return a normalized list of tag strings.
TAGS is the JSON-decoded `tags' value: nil, a string, or a vector
\(see `org-mcp--normalize-tags-to-list').
Validates:
- Tag names follow Org rules (alphanumeric, underscore, at-sign)
- Tags are in `org-tag-alist' or `org-tag-persistent-alist'
  (if either is configured)
- Tags don't violate mutual exclusivity groups in either alist
Duplicate tags are collapsed so the caller never writes `:tag:tag:'.
Signals error for invalid tags."
  (let ((tag-list
         (delete-dups (org-mcp--normalize-tags-to-list tags)))
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
    (org-mcp--validate-tag-charset tag-list)
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
  (when (org-mcp--blank-or-nbsp-only-p title)
    (org-mcp--tool-validation-error
     "Headline title cannot be empty or contain only whitespace"))
  (when (string-match-p "[\n\r]" title)
    (org-mcp--tool-validation-error
     "Headline title cannot contain newlines")))

(defun org-mcp--validate-body-no-headlines (body level)
  "Validate that BODY doesn't contain headlines at LEVEL or shallower.
LEVEL is the Org outline level (1 for *, 2 for **, etc); \"shallower\"
means fewer stars (closer to root in the outline hierarchy).
Throws an MCP tool error if invalid headlines are found."
  ;; Build regex to match headlines at the current level or shallower
  ;; For level 3, this matches ^*, ^**, or ^***
  ;; Literal space (not [ \t]): Org's heading grammar requires a
  ;; space after the stars; `*\tfoo' is a paragraph, not a heading.
  (let ((regex (format "^\\*\\{1,%d\\} " level)))
    (when (string-match regex body)
      (org-mcp--tool-validation-error
       "Body cannot contain headlines at level %d or shallower (fewer stars)"
       level))))

(defconst org-mcp--block-name-regex "\\(\\S-+\\)"
  "Capture group matching the name in an Org block opener or closer.
Per Org's grammar a block name (the suffix in `#+BEGIN_NAME' /
`#+END_NAME', or the name argument in `#+BEGIN: NAME PARAMS') may
contain any non-whitespace characters: dots and colons are
accepted by Org's parser.  Shared between the file-header walker
and `org-mcp--validate-body-no-unbalanced-blocks' to keep the two
sites' character classes from drifting apart.")

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
      (while (re-search-forward (concat
                                 "^#\\+\\(BEGIN\\|END\\)_"
                                 org-mcp--block-name-regex)
                                nil t)
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
Throws a tool error for any other type, or when an array element is
not a string."
  (cond
   ((null tags)
    nil)
   ((vectorp tags)
    (let ((tag-list (append tags nil)))
      (dolist (tag tag-list)
        (unless (stringp tag)
          (org-mcp--tool-validation-error
           "Tag must be a string, got: %S (type: %s)"
           tag (type-of tag))))
      tag-list))
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
on a malformed structural element in the file header: a drawer
with no matching `:END:' or with a heading inside it, a
`#+BEGIN_NAME' block with no matching `#+END_NAME', or a
`#+BEGIN:' dynamic block with no matching `#+END:'."
  (cond
   ((eobp)
    nil)
   ;; Block-pair branch must precede the generic `#'-prefix branch
   ;; below: a `#+BEGIN_*' line is structurally an opener, not a
   ;; standalone `#'-prefixed line, so consume the whole block
   ;; through its matching `#+END_*' to keep drawer-looking and
   ;; heading-looking lines in the block body from being re-parsed.
   ;; Handles both block shapes Org accepts in the file header: a
   ;; named block `#+BEGIN_NAME ... #+END_NAME' (NAME matches), and
   ;; a dynamic block `#+BEGIN: NAME PARAMS ... #+END:' (closer is
   ;; just `#+END:', name not echoed).  `match-string 1' is the
   ;; named-block name; nil iff the opener is the colon form.
   ((let ((case-fold-search t))
      (looking-at
       (concat
        "^[ \t]*#\\+begin\\(?::\\|_"
        org-mcp--block-name-regex
        "\\)")))
    (let* ((block-name (match-string 1))
           (case-fold-search t)
           (end-regex
            (if block-name
                (format "^[ \t]*#\\+end_%s[ \t]*$"
                        (regexp-quote block-name))
              "^[ \t]*#\\+end:[ \t]*$"))
           (error-msg
            (if block-name
                (format "Unterminated #+BEGIN_%s block in file header"
                        block-name)
              "Unterminated #+BEGIN: dynamic block in file header")))
      (forward-line)
      (while (and (not (eobp)) (not (looking-at end-regex)))
        (forward-line))
      ;; Stricter than Org's parser (which degrades an unterminated
      ;; block to a paragraph at parse time): an unterminated block
      ;; in the file's header is much more likely a typo than
      ;; intent, so error early -- matching the drawer-unterminated
      ;; posture below and the project's general "reject malformed
      ;; header structure at the validation boundary" stance.
      (unless (looking-at end-regex)
        (org-mcp--tool-validation-error "%s" error-msg))
      (forward-line)
      t))
   ;; Broader than Org's comment grammar (`^# ' / `^#$') by design.
   ;; Consume any `#'-prefixed line (with optional leading whitespace,
   ;; matching Org's parser), including `#hashtag' paragraphs Org
   ;; parses as `paragraph' rather than `comment'.  Stopping here
   ;; would insert the new heading before such a line and silently
   ;; make it the new heading's section body -- the same rebinding
   ;; class the drawer branch exists to prevent.
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

(defun org-mcp--validate-and-skip-file-header ()
  "Validate the file's leading header block and return the position past it.
Drives `org-mcp--skip-file-header-element' from `point-min' inside
`save-excursion', so point and validation errors are decoupled from
the caller's positioning concerns.  Intended to be called once per
modifying tool invocation, before any insertion or navigation, so
that file-header integrity is checked uniformly regardless of
whether the call targets the top level or a parent heading.  The
returned position is the buffer offset immediately past the
header block; callers that only care about the side-effect
validation can ignore it."
  (save-excursion
    (goto-char (point-min))
    (while (org-mcp--skip-file-header-element))
    (point)))

(defun org-mcp--org-todo-checked (state)
  "Set the current entry's TODO keyword to STATE, erroring on a silent veto.
Point must be on the entry's heading.  A member of `org-blocker-hook'
can veto the transition; non-interactive `org-todo' then fails silently,
leaving the buffer unchanged.  Detect that no-op via
`buffer-chars-modified-tick' and raise a tool error, naming the blocking
entry when `org-block-entry-blocking' records one and falling back
otherwise (e.g. a checkbox block leaves it nil).

On a `COMMENT'-prefixed heading `org-todo' strips the COMMENT before it
consults `org-blocker-hook' and, on a veto, throws without restoring it --
a buffer change that would mask the veto from the tick guard.  Strip the
COMMENT here first so `org-todo' sees a plain heading, then restore it
either way, leaving the tick to reflect only the transition."
  (let ((recomment (org-in-commented-heading-p t)))
    (when recomment
      (org-toggle-comment))
    (let ((org-block-entry-blocking nil)
          (tick (buffer-chars-modified-tick)))
      (org-todo state)
      (let ((blocked (= tick (buffer-chars-modified-tick))))
        (when recomment
          (org-toggle-comment))
        (when blocked
          (mcp-server-lib-tool-throw
           (format
            "TODO state change to %s blocked%s"
            state
            (if org-block-entry-blocking
                (format " by \"%s\"" org-block-entry-blocking)
              " (checkbox or other org-blocker-hook member)"))))))))

(defun org-mcp--goto-headline-for-modify (headline-path uri)
  "Validate the file header, then move point to HEADLINE-PATH's line start.
URI selects ID-based resolution when it carries the `org-id://' prefix,
otherwise headline-path resolution.  Combines the header-integrity check
that every modifying tool performs with navigation to the target
heading."
  (org-mcp--validate-and-skip-file-header)
  (org-mcp--goto-headline-from-uri
   headline-path (string-prefix-p org-mcp--uri-id-prefix uri))
  (beginning-of-line))

(defun org-mcp--navigate-to-parent (parent-path parent-id)
  "Navigate point to a parent headline and return its level.
PARENT-PATH is a list of headline titles, or PARENT-ID is an ID
string identifying the parent.  Returns the parent's heading level
as an integer.  Top-level inserts are handled by the caller
directly using the position returned by
`org-mcp--validate-and-skip-file-header'.

Caller preconditions, NOT re-checked here:
- Exactly one of PARENT-PATH or PARENT-ID is non-nil; this function
  is only called when the caller has already decided the insertion
  targets a child of an existing heading."
  (org-mcp--goto-headline-from-uri
   (or (and parent-id (list parent-id)) parent-path) parent-id)
  (org-back-to-heading t)
  (cl-assert
   (or (null parent-id) (string= (org-entry-get nil "ID") parent-id)))
  (org-current-level))

;; The two `org-mcp--position-*' helpers below are positioners, not
;; inserters: callers must run `org-mcp--ensure-line-start' before
;; inserting a column-0 token, since each helper may leave point
;; mid-line at `point-max' when the file lacks a trailing newline.

(defun org-mcp--position-after-sibling (after-id)
  "Position point at end of the AFTER-ID child of current heading.
Scans the parent's children for one whose `:ID:' equals AFTER-ID and
lands point at the end of that sibling's subtree, ready for insertion
immediately after it.

Caller preconditions, NOT re-checked here:
- Point is at parent heading.
- AFTER-ID is a non-nil bare UUID, already extracted from the
  originating `org-id://{uuid}' URI by `org-mcp--validate-after-uri'
  at the tool boundary.

Throws a validation error if AFTER-ID matches the parent's own
`:ID:', or if no child carries AFTER-ID.  The parent-self check
lives here rather than at the tool boundary so the
`org-headline://' parent case is covered alongside the
`org-id://' one.  If the parent has no `:ID:', no `org-id://'
`after_uri' can collide with it; the `when-let*' short-circuit
skips the parent-self check on that branch.

Post-condition on point: point lands at column 0 of the next heading
or at `point-max' (mid-line when the file lacks a trailing newline);
the caller normalizes this via `org-mcp--ensure-line-start'."
  ;; When the parent has no `:ID:', `parent-id' is nil;
  ;; short-circuiting here avoids the `(string= nil after-id)' = t
  ;; coercion that fires when `after-id' is the literal string "nil".
  (when-let* ((parent-id (org-entry-get nil "ID"))
              ((string= parent-id after-id)))
    (org-mcp--tool-validation-error
     "Field after_uri %s%s refers to parent_uri itself, not a sibling"
     org-mcp--uri-id-prefix after-id))
  (let ((sibling-pos nil))
    (save-excursion
      (let ((more (org-goto-first-child)))
        (while (and more (not sibling-pos))
          (let ((sibling-id (org-entry-get nil "ID")))
            (if (and sibling-id (string= sibling-id after-id))
                (setq sibling-pos (point))
              (setq more (org-get-next-sibling)))))))
    (unless sibling-pos
      (org-mcp--tool-validation-error
       "Sibling with ID %s not found under parent"
       after-id))
    (goto-char sibling-pos)
    (org-end-of-subtree t t)))

(defun org-mcp--position-for-new-child (position)
  "Position point for a new child of current heading per POSITION.
POSITION is the symbol `start' to insert before the parent's first
existing child \(falling back to end-of-subtree when the parent has
no children), or `end' for end-of-subtree placement.

Caller preconditions, NOT re-checked here:
- Point is at parent heading.

Post-condition on point:
- POSITION is `start' and the parent has at least one child: point
  lies on the first child's heading line (column 0).
- POSITION is `start' with no children, or POSITION is `end': point
  lies at parent-end -- column 0 of the next heading, or at
  `point-max' (mid-line when the file lacks a trailing newline).

The caller normalizes the mid-line case via
`org-mcp--ensure-line-start'."
  (unless (and (eq position 'start) (org-goto-first-child))
    (org-end-of-subtree t t)))

(defun org-mcp--ensure-line-start ()
  "Ensure point is at the start of a line.
Inserts a newline before point if needed.  Beginning-of-buffer
counts as a line start, so no insertion is needed there."
  (unless (or (bobp) (eq (char-before) ?\n))
    (insert "\n")))

(defun org-mcp--insert-heading-line (level title)
  "Insert heading with LEVEL stars and TITLE at point.
Inserts with a trailing newline so following content starts on its
own line; leaves point at end-of-line of the new heading so the
caller can apply `org-todo' and `org-set-tags'.

Uses manual `(insert ...)' to bypass `org-insert-heading's
context-dependent adjustments at column-0 points (sibling-vs-
child resolution; leading-separator insertion that creates a
blank line).

Safe to call at `point-max'; the trailing `\\n' becomes the file's
final newline."
  (org-mcp--ensure-line-start)
  (let ((heading-start (point)))
    (insert (make-string level ?*) " " title "\n")
    (goto-char heading-start)
    (end-of-line)))

(defun org-mcp--point-for-top-level-insert (position header-end)
  "Return the buffer position for inserting a new top-level heading.
POSITION is `start' or `end'.  HEADER-END is the buffer position past
the file header block, as returned by
`org-mcp--validate-and-skip-file-header', anchoring the heading search
past it so `^\\*' patterns inside the header block do not match.
`end', or `start' when the buffer past the header block has no
top-level heading (empty file, header-only, or only zeroth-section
content like a plain paragraph), returns `point-max' -- so any
zeroth-section content stays above a heading inserted there rather
than being absorbed into its section body.  `start' with an existing
top-level heading returns the position of the first one."
  (or (and (eq position 'start)
           (save-excursion
             (goto-char header-end)
             (and (re-search-forward "^\\* " nil t)
                  (match-beginning 0))))
      (point-max)))

(defun org-mcp--insert-top-level-heading (title position header-end)
  "Insert TITLE as a new top-level heading at POSITION.
POSITION is `start' to insert before any existing top-level heading,
or `end' to append at end of buffer.  HEADER-END is the buffer
position past the file header block, as returned by
`org-mcp--validate-and-skip-file-header'.  The insertion point is
computed by `org-mcp--point-for-top-level-insert'.  After insertion,
point is left at end-of-line of the new heading so the caller can
apply `org-todo' and `org-set-tags'.

Manual `(insert ...)' throughout, not `org-insert-heading' -- see
`org-mcp--insert-heading-line' for the rationale."
  (goto-char
   (org-mcp--point-for-top-level-insert position header-end))
  (if (looking-at-p "\\* ")
      (org-mcp--insert-heading-line 1 title)
    (org-mcp--ensure-line-start)
    ;; Manual insert (no trailing `\n').  Routing through
    ;; `insert-heading-line' would defer EOF normalisation to
    ;; the body-insertion block; manual keeps the EOF case local.
    (insert "* " title)))

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
    (org-mcp--ensure-line-start)
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

(defun org-mcp--agenda-command-type (entry)
  "Return the type string classifying `org-agenda-custom-commands' ENTRY.
One of \"prefix\" (a bare key group: a two-element list or the dotted
`(KEY . DESC)' form), a builtin block-type symbol name (\"agenda\",
\"agenda*\", \"todo\", \"todo-tree\", \"search\", \"occur-tree\",
\"tags\", \"tags-todo\", \"tags-tree\", \"alltodo\", \"stuck\"),
\"composite\" (a list of blocks), or \"function\" (a user function or
lambda).  The dotted-pair check comes first so `length'/`nth' never
traverse an improper list."
  (cond
   ((not (listp (cdr entry)))
    "prefix")
   ((< (length entry) 3)
    "prefix")
   (t
    (let ((block-type (nth 2 entry)))
      (cond
       ((memq
         block-type
         '(agenda
           agenda*
           todo
           todo-tree
           search
           occur-tree
           tags
           tags-todo
           tags-tree
           alltodo
           stuck))
        (symbol-name block-type))
       ((functionp block-type)
        "function")
       ((listp block-type)
        "composite")
       (t
        "function"))))))

(defun org-mcp--tool-get-agenda-config ()
  "Return the custom agenda commands from `org-agenda-custom-commands'."
  (json-encode
   `((commands
      .
      ,(vconcat
        (mapcar
         (lambda (entry)
           (let* ((key (car entry))
                  (type (org-mcp--agenda-command-type entry))
                  (description
                   (if (listp (cdr entry))
                       (nth 1 entry)
                     (cdr entry)))
                  (node
                   `((key
                      .
                      ,(if (characterp key)
                           (char-to-string key)
                         key))
                     (description . ,description) (type . ,type))))
             (if (string= type "function")
                 (append node `((raw . ,(prin1-to-string entry))))
               node)))
         org-agenda-custom-commands))))))

(defun org-mcp--agenda-block-unrunnable-reason (type match)
  "Return why an agenda block of TYPE with MATCH can't run here, or nil.
The sparse-tree types (tags-tree, todo-tree, occur-tree) build an
in-buffer tree rather than an agenda listing; the match-requiring
listing types (tags, tags-todo, search) would prompt when MATCH is nil
or blank -- Org treats a whitespace-only match as absent -- so the same
blank check as the `date' field is applied.  Match-free listing types
\(agenda, todo, alltodo, stuck) run."
  (cond
   ((memq type '(tags-tree todo-tree occur-tree))
    "builds an in-buffer sparse tree, not an agenda listing")
   ((and (memq type '(tags tags-todo search))
         (or (null match)
             (and (stringp match)
                  (org-mcp--blank-or-nbsp-only-p match))))
    "prompts for a match that the command does not specify")))

(defun org-mcp--agenda-subblock-unrunnable-reason (block)
  "Return why composite sub-BLOCK can't run here, or nil.
BLOCK is a (TYPE MATCH SETTINGS) list from a composite command's block
list; delegates to `org-mcp--agenda-block-unrunnable-reason'."
  (and (consp block)
       (org-mcp--agenda-block-unrunnable-reason
        (car block) (nth 1 block))))

(defun org-mcp--agenda-unrunnable-reason (entry)
  "Return why custom-command ENTRY can't run non-interactively, or nil.
A bare prefix has no command; a sparse-tree block builds a tree, not a
listing; a match-requiring block with no match prompts.  A composite is
blocked by its first un-runnable sub-block.  A function command is
opaque and assumed runnable -- if it builds no agenda buffer that is
caught after dispatch by `org-mcp--agenda-collect'."
  (let ((type (org-mcp--agenda-command-type entry)))
    (cond
     ((string= type "prefix")
      "is a prefix key with no command to run")
     ((string= type "composite")
      (cl-some
       #'org-mcp--agenda-subblock-unrunnable-reason (nth 2 entry)))
     ((string= type "function")
      nil)
     (t
      (org-mcp--agenda-block-unrunnable-reason
       (nth 2 entry) (nth 3 entry))))))

(defun org-mcp--agenda-span (view)
  "Return the reserved span symbol for VIEW, or nil.
VIEW is matched case-insensitively against the builtin spans \"day\",
\"week\", and \"month\"; anything else yields nil and is treated as a
custom-command dispatch key."
  (let ((normalized-view (downcase view)))
    (cond
     ((string= normalized-view "day")
      'day)
     ((string= normalized-view "week")
      'week)
     ((string= normalized-view "month")
      'month))))

(defun org-mcp--agenda-response (view-name date result)
  "Encode an org-get-agenda JSON response.
VIEW-NAME is the reported `view'; DATE the user's date argument or nil;
RESULT the `org-mcp--agenda-collect' triple (TEXT STARTING-DAY BLOCKS).
`start_day' is the anchor day as YYYY-MM-DD, or null for a non-dated
view."
  (let ((starting-day (nth 1 result)))
    (json-encode
     `((view . ,view-name)
       (date . ,(or date "today"))
       (start_day
        . ,(and starting-day (org-mcp--agenda-iso-date starting-day)))
       (agenda . ,(nth 0 result))
       (blocks . ,(vconcat (nth 2 result)))))))

(defun org-mcp--tool-get-agenda (view &optional date)
  "Run agenda VIEW and return its text plus structured items.
VIEW is a builtin span (\"day\", \"week\", or \"month\") or a dispatch
key from `org-agenda-custom-commands'.  The agenda includes only
non-missing files from `org-mcp-allowed-files'; other
`org-agenda-files' entries are ignored.

MCP Parameters:
  view - Agenda view (string, required): the builtin span \"day\",
         \"week\", or \"month\" (case-insensitive), matching
         `org-agenda-day-view', `org-agenda-week-view', and
         `org-agenda-month-view'; or any dispatch key defined in
         `org-agenda-custom-commands' (discover keys with
         org-get-agenda-config).  \"day\"/\"week\"/\"month\" are
         reserved for the builtin spans
  date - Reference day (string, optional).  A string like
         `org-read-date' accepts (e.g. \"2026-04-26\" or \"+2d\");
         how unrecognized input is treated follows the installed
         Org version.  If omitted, today is used; an empty or
         whitespace-only string is rejected -- omit the key instead.
         Applies only to the day/week/month spans; every
         custom-command view ignores it (a custom agenda block always
         anchors on today)"
  (org-mcp--validate-string-field view "view")
  (org-mcp--validate-string-field date "date" t)
  (when (and date (org-mcp--blank-or-nbsp-only-p date))
    (org-mcp--tool-validation-error
     "Field date must not be empty or whitespace-only"))
  (let ((agenda-files (org-mcp--agenda-allowed-file-list))
        (span (org-mcp--agenda-span view)))
    (unless agenda-files
      (org-mcp--tool-validation-error
       "No existing files in org-mcp-allowed-files; cannot build agenda"))
    (if span
        (let* ((start-day (org-mcp--agenda-start-day date span))
               (result
                (org-mcp--agenda-buffer-text
                 agenda-files start-day span)))
          (org-mcp--agenda-response (symbol-name span) date result))
      (let ((entry (assoc view org-agenda-custom-commands)))
        (unless entry
          (org-mcp--tool-validation-error
           "Unknown agenda view \"%s\"; expected \"day\", \"week\", \"month\", or an org-agenda-custom-commands key.  Available keys: %s"
           view
           (if org-agenda-custom-commands
               (mapconcat (lambda (cmd) (format "%S" (car cmd)))
                          org-agenda-custom-commands
                          ", ")
             "(none)")))
        (when-let* ((reason
                     (org-mcp--agenda-unrunnable-reason entry)))
          (org-mcp--tool-validation-error
           "Agenda view \"%s\" %s; only fully-specified listing custom commands are supported"
           view reason))
        (let ((result
               (org-mcp--agenda-collect
                agenda-files (lambda () (org-agenda nil view)))))
          (org-mcp--agenda-response view date result))))))

(defun org-mcp--tool-update-todo-state (uri current_state new_state)
  "Update the TODO state of a headline at URI.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the updated headline.
CURRENT_STATE is the current TODO state (empty string for no state).
NEW_STATE is the new TODO state to set (empty string clears it).

MCP Parameters:
  uri - URI of the headline to update
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  current_state - Expected current TODO state
                  Use empty string \"\" if headline has no TODO state
                  Must match actual state or tool will error
  new_state - New TODO state to set
              Must be a valid keyword from `org-todo-keywords',
              or empty string \"\" to clear to no TODO keyword"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field current_state "current_state")
  (org-mcp--validate-string-field new_state "new_state")
  (pcase-let ((`(,file-path . ,headline-path)
               (org-mcp--parse-resource-uri uri)))
    (org-mcp--validate-todo-state new_state "new_state")
    (org-mcp--modify-and-save file-path
        "update"
        ;; Report the actual post-`org-todo' state,
        ;; not the request: a repeating entry marked
        ;; DONE is reset back to its not-done keyword
        ;; by `org-auto-repeat-maybe'.  The macro
        ;; evaluates this alist after BODY, with point
        ;; still on the heading.
        `((previous_state . ,current_state)
          (new_state . ,(or (org-get-todo-state) "")))
      (org-mcp--goto-headline-for-modify headline-path uri)

      ;; Treat "" and nil as the same "no TODO state":
      ;; `org-get-todo-state' returns nil; the wire protocol uses "".
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

      ;; Update the state, erroring if org-blocker-hook silently vetoes it.
      (org-mcp--org-todo-checked new_state))))

(defun org-mcp--validate-after-uri (after-uri)
  "Validate AFTER-URI at the tool boundary, returning the parsed ID.
Returns the bare UUID extracted from a well-formed `org-id://{uuid}'
URI, or nil if AFTER-URI is nil.

Signals a validation error if AFTER-URI is the empty string or
contains only whitespace: clients that do not want to specify a
sibling must omit the JSON key entirely rather than pass `\"\"'
or any string with no meaningful content.

Signals a validation error if AFTER-URI is non-empty but not in the
`org-id://' form -- the only sibling reference scheme accepted by
the placement walker.  This also covers the bare prefix
`org-id://' with no UUID after it, and `org-id://' followed by a
whitespace-only UUID: both would otherwise extract to a
no-meaningful-content UUID and surface downstream as the misleading
`Sibling with ID  not found under parent'.

Caller preconditions, NOT re-checked here:
- AFTER-URI has already been passed through
  `org-mcp--validate-string-field' at the tool boundary, so its
  only legal values here are nil or a string."
  (when after-uri
    (when (org-mcp--blank-or-nbsp-only-p after-uri)
      (org-mcp--tool-validation-error
       "Field after_uri must not be empty or whitespace-only"))
    (let ((id
           (org-mcp--extract-uri-suffix
            after-uri org-mcp--uri-id-prefix)))
      (when (or (null id) (org-mcp--blank-or-nbsp-only-p id))
        (org-mcp--tool-validation-error
         "Field after_uri is not %s: %s"
         org-mcp--uri-id-prefix after-uri))
      id)))

(defun org-mcp--validate-position (position)
  "Validate POSITION and return it as a symbol.
A nil POSITION normalises to `end' (the default placement); the
strings \"start\" and \"end\" return the matching symbol.  Any
other value signals a validation error.

This validator does NOT check the mutex with `after_uri';
see `org-mcp--check-position-after-uri-mutex' for that.

Caller preconditions, NOT re-checked here:
- POSITION has already been passed through
  `org-mcp--validate-string-field' at the tool boundary, so its
  only legal values here are nil or a string."
  (cond
   ((null position)
    'end)
   ((string= position "start")
    'start)
   ((string= position "end")
    'end)
   (t
    (org-mcp--tool-validation-error
     "Field position must be one of \"start\", \"end\", got: \"%s\""
     position))))

(defun org-mcp--check-position-after-uri-mutex (position after-id)
  "Signal a validation error if POSITION and AFTER-ID are both supplied.
A string POSITION (one that survived
`org-mcp--validate-string-field') counts as supplied; a nil
POSITION (either an absent key or a wire-level JSON `null', both
indistinguishable at this layer) is treated as not supplied and
never trips the mutex.  AFTER-ID is the id extracted from
`after_uri' by `org-mcp--validate-after-uri'.

Caller preconditions, NOT re-checked here:
- POSITION has already been passed through
  `org-mcp--validate-string-field' at the tool boundary, so its
  only legal values here are nil or a string."
  (when (and (stringp position) after-id)
    (org-mcp--tool-validation-error
     "Fields position and after_uri are mutually exclusive")))

(defun org-mcp--check-after-uri-not-top-level
    (after-id parent-path parent-id &optional parent-uri-field-name)
  "Signal a validation error if AFTER-ID names a sibling at top level.
AFTER-ID is the id extracted from `after_uri' by
`org-mcp--validate-after-uri'.  PARENT-PATH and PARENT-ID come from
`org-mcp--parse-parent-uri'; both nil denotes a top-level insert.
A top-level insert has no sibling-reference slot in the placement
contract, so the combination is rejected at the tool boundary
instead of silently falling through to end-of-file.
PARENT-URI-FIELD-NAME is the caller's MCP field name for the parent
URI, named in the error message; it defaults to \"parent_uri\" so
callers whose field is named that need not pass it.

Callers must run this AFTER `org-mcp--parse-parent-uri', which
supplies the parsed parent shape."
  (when (and after-id (not (or parent-path parent-id)))
    (org-mcp--tool-validation-error
     (concat
      "Field after_uri must not be combined with a top-level "
      (or parent-uri-field-name "parent_uri")
      " (no fragment)"))))

(defun org-mcp--tool-add-todo
    (title
     todo_state tags body parent_uri &optional after_uri position)
  "Add a new TODO item to an Org file.
Creates an Org ID for the new headline and returns its ID-based URI.
TITLE is the headline text.
TODO_STATE is the TODO state from `org-todo-keywords', or empty
string for no keyword.
TAGS is a single tag string or list of tag strings.
BODY is optional body text.
PARENT_URI is the URI of the parent item.
AFTER_URI is optional URI of sibling to insert after.
POSITION is optional placement: \"start\" or \"end\".
Defaults to \"end\".

MCP Parameters:
  title - Headline text without TODO state or tags
          Cannot be empty or whitespace-only
          Cannot contain newlines
  todo_state - TODO state from `org-todo-keywords',
               or empty string \"\" for a headline with no keyword
  tags - Tags to add (single string or array of strings)
         Single tag: \"urgent\"; multiple tags: [\"work\", \"urgent\"]
         Validated against `org-tag-alist' if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups
  body - Optional body text content
         Cannot contain headlines at the new item's level or
         shallower (fewer stars)
         If #+BEGIN/#+END blocks are present, they must be balanced
  parent_uri - Parent item URI
               For top-level: org-headline://{absolute-path}
               For child: org-headline://{path}#{parent-path}
                          or org-id://{parent-uuid}
  after_uri - Sibling to insert after (optional)
              Must be org-id://{uuid} format
              Empty string is rejected; omit the key instead
              See tool description for combination rules with
              parent_uri and position
  position - Placement of the new item: \"start\" or \"end\"
             (optional, defaults to \"end\")
             Empty string is rejected; omit the key for default
             placement
             See tool description for placement-scenario contract
             and combination rules with after_uri"
  ;; Validation runs in two phases: first at the tool boundary (no
  ;; file I/O), then inside `modify-and-save' once the file is open.
  ;; Boundary order: per-field shape, then per-field content, then
  ;; tag and `parent_uri' normalisation, then cross-field checks
  ;; (mutex, not-top-level).
  (org-mcp--validate-string-field title "title")
  (org-mcp--validate-string-field todo_state "todo_state")
  (org-mcp--validate-string-field body "body" t)
  (org-mcp--validate-string-field parent_uri "parent_uri")
  (org-mcp--validate-string-field after_uri "after_uri" t)
  (org-mcp--validate-string-field position "position" t)
  (org-mcp--validate-headline-title title)
  (org-mcp--validate-todo-state todo_state "todo_state")
  ;; Collapse empty `body' to nil locally: the validator accepts both
  ;; nil and "" (allow-nil=t), but the downstream body-insertion block
  ;; would emit two unwanted blank lines for "" because
  ;; `(insert "\n" "")' plus the trailing `(string-suffix-p "\n" "")'
  ;; guard both fire.
  (let ((effective-body (and body (not (string-empty-p body)) body)))
    (when effective-body
      (org-mcp--validate-body-no-unbalanced-blocks effective-body))
    (let ((after-id (org-mcp--validate-after-uri after_uri))
          (position-sym (org-mcp--validate-position position)))
      (pcase-let ((tag-list
                   (org-mcp--validate-and-normalize-tags tags))
                  (`(,file-path ,parent-path ,parent-id)
                   (org-mcp--parse-parent-uri parent_uri)))
        ;; Cross-field placement checks, both after per-field
        ;; validation and `parse-parent-uri'.  Mutex receives
        ;; raw `position' (not `position-sym'): absent and
        ;; explicit "end" both normalise to `'end'.
        (org-mcp--check-position-after-uri-mutex position after-id)
        (org-mcp--check-after-uri-not-top-level
         after-id parent-path parent-id)

        ;; Add the TODO item
        (org-mcp--modify-and-save file-path "add TODO"
                                  `((file
                                     .
                                     ,(file-name-nondirectory
                                       file-path))
                                    (title . ,title))
          ;; Bound on every call path so the child-insert path also
          ;; picks up file-header validation; the value itself is only
          ;; consumed by the top-level insert below.
          (let ((header-end (org-mcp--validate-and-skip-file-header))
                (parent-level
                 (and (or parent-path parent-id)
                      (org-mcp--navigate-to-parent
                       parent-path parent-id))))
            (when parent-level
              ;; The `after-id' branch ignores `position-sym': the
              ;; mutex guarantees `position' was nil (so
              ;; `position-sym' is `end') when `after-id' is set.
              (if after-id
                  (org-mcp--position-after-sibling after-id)
                (org-mcp--position-for-new-child position-sym)))

            (let ((new-level
                   (if parent-level
                       (1+ parent-level)
                     1)))
              (when effective-body
                (org-mcp--validate-body-no-headlines
                 effective-body new-level))

              (if parent-level
                  (org-mcp--insert-heading-line new-level title)
                (org-mcp--insert-top-level-heading
                 title position-sym header-end)))

            ;; Empty `todo_state' means no keyword; the freshly
            ;; inserted heading already has none, so skip the no-op
            ;; `org-todo' call and its state-change machinery.
            (unless (string-empty-p todo_state)
              (org-mcp--org-todo-checked todo_state))

            (when tag-list
              (org-set-tags tag-list))

            ;; Restore EOL: `org-todo' / `org-set-tags' preserve
            ;; column, not EOL.
            (end-of-line)
            ;; `save-excursion' to keep point on the new heading:
            ;; body insertion can otherwise leave point past
            ;; deeper-level headings (which the validator permits),
            ;; and the `modify-and-save' URI lookup needs the new
            ;; heading, not whatever is closest backward.
            (save-excursion
              ;; Add body if provided
              (if effective-body
                  (org-mcp--insert-body-after-heading effective-body)
                ;; No body - ensure newline after heading
                (unless (looking-at "\n")
                  (insert "\n"))))))))))

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

(defun org-mcp--tool-edit-headline
    (uri current_title &optional new_title add_tags remove_tags)
  "Edit the title and/or local tags of the headline at URI.
CURRENT_TITLE must match the headline's actual title or the tool
errors.  Supply at least one of NEW_TITLE, ADD_TAGS, REMOVE_TAGS.
The TODO state, planning, properties, and body are preserved; an Org
ID is created if one doesn't exist.  Returns the updated node.

MCP Parameters:
  uri - URI of the headline to edit
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  current_title - Expected current title without TODO state or tags
                  Must match actual title or tool will error
                  Used to prevent race conditions
  new_title - New title without TODO state or tags (optional)
              Cannot be empty or whitespace-only
              Cannot contain newlines
  add_tags - Local tags to add (optional; single string or array)
             Validated against the globally configured tag alist;
             a file's own #+TAGS: additions are not honored.
             Duplicates within add_tags are collapsed.
  remove_tags - Local tags to remove (optional; single string or array)
                A tag may not appear in both add_tags and remove_tags"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field current_title "current_title")
  (org-mcp--validate-string-field new_title "new_title" t)
  (when new_title
    (org-mcp--validate-headline-title new_title))
  (let ((add-list (org-mcp--validate-and-normalize-tags add_tags))
        (remove-list (org-mcp--normalize-tags-to-list remove_tags)))
    (org-mcp--validate-tag-charset remove-list)
    (unless (or new_title add-list remove-list)
      (org-mcp--tool-validation-error
       "Provide at least one of new_title, add_tags, remove_tags"))
    (when-let* ((dup
                 (cl-intersection
                  add-list
                  remove-list
                  :test #'string=)))
      (org-mcp--tool-validation-error
       "Tags cannot be in both add_tags and remove_tags: %s"
       (mapconcat #'identity dup ", ")))
    (pcase-let ((`(,file-path . ,headline-path)
                 (org-mcp--parse-resource-uri uri)))
      (org-mcp--modify-and-save file-path "edit headline"
                                (org-mcp--heading-node-fields
                                 (org-mcp--heading-raw-fields))
        (org-mcp--goto-headline-for-modify headline-path uri)

        ;; Verify current title matches
        (let ((actual-title (org-get-heading t t t t)))
          (unless (string= actual-title current_title)
            (org-mcp--state-mismatch-error
             current_title actual-title "Title")))

        (when new_title
          (org-edit-headline new_title))
        (when (or add-list remove-list)
          (let* ((current (org-get-tags nil t))
                 (kept
                  (cl-remove-if
                   (lambda (tag) (member tag remove-list)) current))
                 (final
                  (append
                   kept
                   (cl-remove-if
                    (lambda (tag) (member tag kept)) add-list))))
            (org-set-tags final)))))))

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
             Cannot introduce headlines at the edit target's
             level or shallower (fewer stars)
             Must maintain balanced #+BEGIN/#+END blocks
  replace_all - Replace all occurrences (optional, default false)
                When false, old_body must be unique in the body"
  (org-mcp--validate-string-field resource_uri "resource_uri")
  (org-mcp--validate-string-field old_body "old_body")
  (org-mcp--validate-string-field new_body "new_body")
  ;; Normalize falsy values to nil so the multi-occurrence guard
  ;; fires; `:json-false' and string "false" are truthy in Elisp.
  (let ((replace_all (org-mcp--normalize-json-boolean replace_all)))
    (org-mcp--validate-body-no-unbalanced-blocks new_body)

    (pcase-let ((`(,file-path . ,headline-path)
                 (org-mcp--parse-resource-uri resource_uri)))

      (org-mcp--modify-and-save file-path "edit body" nil
        (org-mcp--validate-and-skip-file-header)
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

(defconst org-mcp--planning-repeater-body
  "\\(?:\\+\\+?\\|\\.\\+\\)[0-9]+[hdwmy]\\(?:/[0-9]+[hdwmy]\\)?"
  "Regex body matching a repeater cookie, no anchors or leading space.
Prefix `+N'/`++N'/`.+N' (unit h/d/w/m/y) with an optional `/M' habit
interval.  Shared by `org-mcp--split-planning-spec' and
`org-mcp--set-planning-cookies' so the two stay in sync.")

(defconst org-mcp--planning-warning-body "-[0-9]+[hdwmy]"
  "Regex body matching a warning cookie, no anchors or leading space.
A `-N' offset (unit h/d/w/m/y).  Shared by
`org-mcp--split-planning-spec' and `org-mcp--set-planning-cookies'.")

(defun org-mcp--split-planning-spec (spec)
  "Split planning SPEC into a list (DATE-SPEC REPEATER WARNING).
SPEC is the user timestamp content: a date/time chunk optionally
followed by repeater and/or warning cookies.  The first whitespace
token is always part of the date/time chunk, so a leading `+1w' is a
relative date, not a repeater.  Trailing tokens matching a repeater
cookie (`+N', `++N', `.+N', optionally with a `/M' habit interval) or
a warning cookie (`-N'), each with a unit in h/d/w/m/y, are peeled
off.  DATE-SPEC is the remaining chunk; REPEATER and WARNING are
strings or nil."
  (let ((tokens (split-string spec))
        (repeater nil)
        (warning nil))
    (while (and (> (length tokens) 1)
                (let ((last (car (last tokens))))
                  (cond
                   ((and (not repeater)
                         (string-match-p
                          (concat
                           "\\`"
                           org-mcp--planning-repeater-body
                           "\\'")
                          last))
                    (setq repeater last))
                   ((and (not warning)
                         (string-match-p
                          (concat
                           "\\`" org-mcp--planning-warning-body "\\'")
                          last))
                    (setq warning last)))))
      (setq tokens (butlast tokens)))
    (list (string-join tokens " ") repeater warning)))

(defun org-mcp--set-planning-cookies (keyword repeater warning)
  "Replace the repeater/warning cookies of KEYWORD's planning timestamp.
KEYWORD is \"SCHEDULED\" or \"DEADLINE\".  In the current entry, strip
any existing repeater and warning cookies from that timestamp and
append REPEATER and/or WARNING (each a string or nil) before the
closing `>'.  Assumes the entry already has a KEYWORD timestamp."
  (save-excursion
    (org-back-to-heading t)
    (let ((end
           (save-excursion
             (outline-next-heading)
             (point))))
      ;; The caller (`org-mcp--apply-planning-spec') has just placed the
      ;; KEYWORD timestamp via `org-schedule'/`org-deadline', so the
      ;; search always succeeds.  Bind its result before the assert so
      ;; the side-effecting search still runs even if `cl-assert' is
      ;; compiled out under a high-optimization byte-compile.
      (let ((found
             (re-search-forward (concat
                                 "\\<" keyword ": <\\([^>\n]*\\)>")
                                end t)))
        (cl-assert found)
        (let* ((base
                (replace-regexp-in-string
                 (concat
                  "\\(?: "
                  org-mcp--planning-repeater-body
                  "\\| "
                  org-mcp--planning-warning-body
                  "\\)+\\'")
                 "" (match-string 1)))
               (rebuilt
                (concat
                 base
                 (and repeater (concat " " repeater))
                 (and warning (concat " " warning)))))
          (replace-match rebuilt t t nil 1))))))

(defun org-mcp--apply-planning-spec (setter keyword spec)
  "Set KEYWORD planning from SPEC using SETTER, then splice cookies.
SETTER is `org-schedule' or `org-deadline'.  SPEC is parsed by
`org-mcp--split-planning-spec'; SETTER is called with the date/time
chunk so Org resolves and places the timestamp, then any repeater or
warning cookies are spliced into the KEYWORD timestamp."
  (pcase-let ((`(,date ,repeater ,warning)
               (org-mcp--split-planning-spec spec)))
    (funcall setter nil date)
    (when (or repeater warning)
      (org-mcp--set-planning-cookies keyword repeater warning))))

(defun org-mcp--tool-set-planning
    (uri &optional scheduled deadline clear_scheduled clear_deadline)
  "Set or clear SCHEDULED/DEADLINE planning timestamps on the headline at URI.
Creates an Org ID for the headline if one doesn't exist.

MCP Parameters:
  uri - URI of the headline to update
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  scheduled - Timestamp content to schedule, as written inside <...>:
              a date, optional time, and optional repeater/warning
              cookies (e.g. \"2026-06-20\", \"2026-06-20 14:00\",
              \"+1w\", \"2026-06-20 +1w -3d\")
  deadline - Deadline timestamp content, same grammar as scheduled
  clear_scheduled - When true, remove the SCHEDULED entry
  clear_deadline - When true, remove the DEADLINE entry"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field scheduled "scheduled" t)
  (org-mcp--validate-string-field deadline "deadline" t)
  (setq clear_scheduled
        (org-mcp--normalize-json-boolean clear_scheduled))
  (setq clear_deadline
        (org-mcp--normalize-json-boolean clear_deadline))
  (unless (or scheduled deadline clear_scheduled clear_deadline)
    (org-mcp--tool-validation-error
     (concat
      "Provide at least one of scheduled, deadline, "
      "clear_scheduled, clear_deadline")))
  (when (and scheduled clear_scheduled)
    (org-mcp--tool-validation-error
     "Cannot set and clear scheduled in the same call"))
  (when (and deadline clear_deadline)
    (org-mcp--tool-validation-error
     "Cannot set and clear deadline in the same call"))
  (when (and scheduled (org-mcp--blank-or-nbsp-only-p scheduled))
    (org-mcp--tool-validation-error
     "scheduled is empty; use clear_scheduled to remove the entry"))
  (when (and deadline (org-mcp--blank-or-nbsp-only-p deadline))
    (org-mcp--tool-validation-error
     "deadline is empty; use clear_deadline to remove the entry"))
  (pcase-let ((`(,file-path . ,headline-path)
               (org-mcp--parse-resource-uri-with-headline uri)))
    (org-mcp--modify-and-save file-path "set planning"
                              (save-excursion
                                (org-back-to-heading t)
                                (list
                                 (cons
                                  'scheduled
                                  (org-entry-get nil "SCHEDULED"))
                                 (cons
                                  'deadline
                                  (org-entry-get nil "DEADLINE"))))
      (org-mcp--goto-headline-for-modify headline-path uri)
      ;; `org-log-reschedule'/`org-log-redeadline' set to `note' would
      ;; pop an interactive note buffer that an MCP call cannot answer,
      ;; so suppress planning-change logging for this operation.
      (let ((org-log-reschedule nil)
            (org-log-redeadline nil))
        (when clear_scheduled
          (org-schedule '(4)))
        (when scheduled
          (org-mcp--apply-planning-spec
           #'org-schedule "SCHEDULED" scheduled))
        (when clear_deadline
          (org-deadline '(4)))
        (when deadline
          (org-mcp--apply-planning-spec
           #'org-deadline "DEADLINE" deadline))))))

(defun org-mcp--tool-archive-subtree (uri)
  "Archive the subtree at URI using `org-archive-subtree'.
Creates an Org ID for the headline if one doesn't exist.
Returns the archive file path and the headline's `org-id://' URI.
The returned URI is resolvable via `resources/read' only when the
archive file is itself in `org-mcp-allowed-files'; otherwise it is
informational only, since the entry has left the readable set.

MCP Parameters:
  uri - URI of the headline to archive
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}"
  (org-mcp--validate-string-field uri "uri")
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))
    (unless headline-path
      (org-mcp--tool-validation-error
       "URI must identify a headline to archive, not a whole file"))
    (org-mcp--with-visiting-org-file file-path "archive"
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--uri-id-prefix uri))
      (let*
          ((archive-file
            (let* ((location
                    (or (org-entry-get nil "ARCHIVE" 'inherit)
                        org-archive-location))
                   (file
                    (car
                     ;; `org-archive--compute-location' is an
                     ;; Org-internal function with no public
                     ;; equivalent: it expands the `%s'/`::heading'
                     ;; spec that the public `org-archive-location'
                     ;; stores raw.  This pre-flight resolution must
                     ;; match what `org-archive-subtree' itself
                     ;; resolves, or the unsaved-changes guard below
                     ;; checks the wrong file.
                     (condition-case err
                         (org-archive--compute-location location)
                       (error
                        ;; A malformed location (e.g. an `:ARCHIVE:'
                        ;; spec missing `::') is user-controlled, so
                        ;; report it through the tool-error channel
                        ;; rather than as a JSON-RPC internal error.
                        (org-mcp--tool-validation-error
                         "Invalid archive location %S: %s"
                         location (error-message-string err)))))))
              ;; FILE is a string in every supported Org; assert it so
              ;; a future return-shape change fails clearly here
              ;; rather than as an opaque downstream type error.
              (cl-assert (stringp file))
              file))
           (separate-archive
            (not (string= archive-file (buffer-file-name))))
           ;; Snapshot, before `org-archive-subtree' runs, whether the
           ;; user already had the separate archive file open.  The
           ;; cleanup below kills only a buffer the tool itself opened,
           ;; never one the user owned.
           (archive-buffer-preexisting
            (and separate-archive
                 (find-buffer-visiting archive-file))))
        ;; Guard the archive file before creating the ID, so a failed
        ;; guard cannot leave a stale `org-id-locations' entry
        ;; registered against the source for an archive that never ran.
        (when separate-archive
          (org-mcp--fail-if-modified archive-file "archive"))
        ;; Create the ID before archiving so it travels into the
        ;; archive with the subtree; afterwards point no longer sits
        ;; in the entry.
        (let ((id (org-id-get-create)))
          (unwind-protect
              (progn
                ;; `org-archive-subtree' opens the separate archive file
                ;; (via `find-file-noselect') before it finishes the
                ;; move, so it must run inside this `unwind-protect': a
                ;; throw mid-archive would otherwise leak the buffer it
                ;; opened, since the cleanup below would never run.
                (org-archive-subtree)
                (let ((archive-buffer
                       (and separate-archive
                            (find-buffer-visiting archive-file))))
                  ;; Persist the archive before emptying the source on
                  ;; disk: should the source write fail, the subtree
                  ;; survives in both files (a recoverable duplicate)
                  ;; rather than vanishing from both.
                  ;; `org-archive-subtree' may not save the archive
                  ;; itself (`org-archive-subtree-save-file-p'), so
                  ;; save it explicitly.
                  (when archive-buffer
                    (with-current-buffer archive-buffer
                      (save-buffer))
                    ;; Revert any *other* buffers visiting the archive
                    ;; file: org-archive wrote into and we saved only
                    ;; this one, mirroring the source refresh below.
                    (org-mcp--refresh-file-buffers archive-file
                                                   archive-buffer))
                  (write-region (point-min) (point-max) file-path)
                  (org-mcp--refresh-file-buffers file-path
                                                 (current-buffer))))
            ;; Kill a buffer the tool itself opened, even on the error
            ;; path, so a long-running server does not accumulate
            ;; buffers visiting archive files.  Re-discover it here
            ;; rather than reuse a binding from the body: a throw inside
            ;; `org-archive-subtree' unwinds before any such binding
            ;; exists, yet the buffer it opened still needs reclaiming.
            ;; A buffer the user already had open is left untouched.
            ;; Guard on liveness too: a save hook may have killed it
            ;; already, and `with-current-buffer' on a dead buffer would
            ;; error.
            (let ((archive-buffer
                   (and separate-archive
                        (find-buffer-visiting archive-file))))
              (when (and archive-buffer
                         (buffer-live-p archive-buffer)
                         (not archive-buffer-preexisting))
                (with-current-buffer archive-buffer
                  (set-buffer-modified-p nil))
                (kill-buffer archive-buffer))))
          (json-encode
           `((success . t)
             (archive_file . ,archive-file)
             (uri . ,(concat org-mcp--uri-id-prefix id)))))))))

(defun org-mcp--refile-position-at-target
    (parent-path parent-id after-id position-sym header-end)
  "Position point in the current buffer for a refiled subtree.
Resolves the target parent from PARENT-PATH or PARENT-ID (both nil
means top level), positions point per AFTER-ID or POSITION-SYM, and
returns the level for the moved subtree's root.  HEADER-END is the
position past the file header block (see
`org-mcp--validate-and-skip-file-header'), used for top-level
placement.  Leaves point where `org-paste-subtree' should insert."
  (let ((parent-level
         (and (or parent-path parent-id)
              (org-mcp--navigate-to-parent parent-path parent-id))))
    (cond
     ((and parent-level after-id)
      (org-mcp--position-after-sibling after-id))
     (parent-level
      (org-mcp--position-for-new-child position-sym))
     (t
      (goto-char
       (org-mcp--point-for-top-level-insert
        position-sym header-end))))
    (if parent-level
        (1+ parent-level)
      1)))

(defun org-mcp--refile-noop-p
    (parent-path parent-id after-id position-sym)
  "Return non-nil if refiling the headline at point would change nothing.
Point must be at the source heading.  A no-op requires the node to
already carry an ID and to already occupy the requested slot under the
requested parent: same parent (PARENT-PATH/PARENT-ID, both nil for top
level) and AFTER-ID or POSITION-SYM already satisfied.  Purely
structural -- compares parent and sibling relationships and never
mutates the buffer."
  (and (org-id-get)
       (equal
        (save-excursion (and (org-up-heading-safe) (point)))
        (and (or parent-path parent-id)
             (save-excursion
               (org-mcp--navigate-to-parent parent-path parent-id)
               (point))))
       (cond
        (after-id
         (save-excursion
           (and (org-get-previous-sibling)
                (equal (org-entry-get nil "ID") after-id))))
        ((eq position-sym 'start)
         (save-excursion (not (org-get-previous-sibling))))
        (t
         (save-excursion (not (org-get-next-sibling)))))))

(defun org-mcp--refile-capture-source (src-beg source-buf)
  "Return list (ID SRC-END TREE) for the subtree at SRC-BEG in SOURCE-BUF.
Creates an Org ID if the subtree's root lacks one.  The ID is read
first because creating it inserts a `:PROPERTIES:' drawer that shifts
the subtree's end and changes its text, so SRC-END and TREE are
captured after.  Run this only after the target placement has been
resolved, so a rejected move mints no ID."
  (with-current-buffer source-buf
    (goto-char src-beg)
    (let* ((id (org-id-get-create))
           (src-end
            (save-excursion
              (org-end-of-subtree t t)
              (point)))
           (tree (buffer-substring-no-properties src-beg src-end)))
      (list id src-end tree))))

(defun org-mcp--refile-paste-and-persist
    (source-buf
     src-beg src-end tree new-level insertion source-file target-file)
  "Move subtree TREE from SOURCE-BUF into the current (target) buffer.
[SRC-BEG, SRC-END) delimits the subtree in SOURCE-BUF; NEW-LEVEL is the
level for its pasted root; INSERTION is a marker in the current buffer
at the paste point.  The cut uses `org-save-markers-in-region' plus
`delete-region' (not `org-cut-subtree'), leaving the user's kill ring
untouched.  When SOURCE-BUF is the current buffer (a same-file move),
SOURCE-FILE is written once.  Otherwise TARGET-FILE is written before
SOURCE-FILE is emptied, so a failed source write leaves the subtree in
both files -- a recoverable duplicate -- rather than dropping it from
both."
  (let ((target-buf (current-buffer)))
    (with-current-buffer source-buf
      (org-save-markers-in-region src-beg src-end)
      (delete-region src-beg src-end))
    (goto-char insertion)
    ;; `org-paste-subtree' runs `org-id-paste-tracker', which
    ;; re-registers the moved subtree's IDs (root and children) against
    ;; the target file, so no manual `org-id-add-location' is needed.
    (org-paste-subtree new-level tree)
    (if (eq source-buf target-buf)
        (progn
          (write-region (point-min) (point-max) source-file)
          (org-mcp--refresh-file-buffers source-file target-buf))
      (write-region (point-min) (point-max) target-file)
      (with-current-buffer source-buf
        (write-region (point-min) (point-max) source-file))
      (org-mcp--refresh-file-buffers target-file target-buf)
      (org-mcp--refresh-file-buffers source-file source-buf))))

(defun org-mcp--refile-capture-and-persist
    (src-beg source-buf new-level insertion source-file target-file)
  "Capture the subtree at SRC-BEG in SOURCE-BUF and move it, returning its ID.
Mints the subtree's Org ID via `org-mcp--refile-capture-source', then
pastes it into the current (target) buffer at INSERTION as a level
NEW-LEVEL subtree and persists via `org-mcp--refile-paste-and-persist'
(SOURCE-FILE, TARGET-FILE).  Must be called with the target buffer
current and after the target placement is resolved, so a rejected move
mints no ID."
  (pcase-let ((`(,id ,src-end ,tree)
               (org-mcp--refile-capture-source src-beg source-buf)))
    (org-mcp--refile-paste-and-persist
     source-buf
     src-beg
     src-end
     tree
     new-level
     insertion
     source-file
     target-file)
    id))

(defun org-mcp--tool-refile-headline
    (uri current_title target_parent_uri &optional after_uri position)
  "Move the headline at URI and its subtree under TARGET_PARENT_URI.
Preserves the moved node's TODO state, tags, properties, ID, body,
and child structure; creates an Org ID if it has none and returns
its `org-id://' URI.

CURRENT_TITLE is the expected current title, guarding against races.
AFTER_URI and POSITION select placement among the target's children,
mirroring `org-add-todo' (mutually exclusive).

MCP Parameters:
  uri - URI of the headline to move
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
        Must identify a headline, not a whole file
  current_title - Expected current title without TODO state or tags
                  Must match actual title or tool will error
                  Used to prevent race conditions
  target_parent_uri - URI of the new parent
                      For top-level: org-headline://{absolute-path}/
                      For child: org-headline://{path}#{parent-path}
                                 or org-id://{parent-uuid}
  after_uri - Sibling to place after (optional)
              Must be org-id://{uuid} format
              See tool description for combination rules
  position - Placement among the target's children: \"start\" or
             \"end\" (optional, defaults to \"end\")
             Mutually exclusive with after_uri"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field current_title "current_title")
  (org-mcp--validate-string-field
   target_parent_uri "target_parent_uri")
  (org-mcp--validate-string-field after_uri "after_uri" t)
  (org-mcp--validate-string-field position "position" t)
  (let ((position-sym (org-mcp--validate-position position))
        (after-id (org-mcp--validate-after-uri after_uri)))
    (org-mcp--check-position-after-uri-mutex position after-id)
    (pcase-let ((`(,source-file . ,source-path)
                 (org-mcp--parse-resource-uri uri))
                (`(,target-file ,parent-path ,parent-id)
                 (org-mcp--parse-parent-uri target_parent_uri)))
      (unless source-path
        (org-mcp--tool-validation-error
         "URI must identify a headline to refile, not a whole file"))
      (org-mcp--check-after-uri-not-top-level
       after-id parent-path parent-id
       "target_parent_uri")
      (org-mcp--with-visiting-org-file source-file "refile"
        (let ((header-end (org-mcp--validate-and-skip-file-header))
              (same-file (file-equal-p source-file target-file)))
          (org-mcp--goto-headline-from-uri
           source-path (string-prefix-p org-mcp--uri-id-prefix uri))
          (beginning-of-line)
          (let ((title (org-get-heading t t t t)))
            (unless (string= title current_title)
              (org-mcp--state-mismatch-error
               current_title title "Title"))
            (when (and after-id (equal (org-id-get) after-id))
              (org-mcp--tool-validation-error
               "Field after_uri refers to the node being refiled"))
            (if (and same-file
                     (org-mcp--refile-noop-p
                      parent-path parent-id after-id position-sym))
                ;; Already in the requested slot with an ID: change
                ;; nothing, return the existing URI.
                (json-encode
                 `((success . t)
                   (uri
                    . ,(concat org-mcp--uri-id-prefix (org-id-get)))
                   (title . ,title)
                   (source_file
                    . ,(file-name-nondirectory source-file))
                   (target_file
                    . ,(file-name-nondirectory target-file))))
              ;; Reject a move that would make the node its own
              ;; descendant (same-file only; across files no
              ;; containment is possible).  Checked before creating the
              ;; ID so a rejected move mutates nothing.
              (when (and same-file (or parent-path parent-id))
                (let ((node-beg (line-beginning-position))
                      (node-end
                       (save-excursion
                         (org-end-of-subtree t t)
                         (point)))
                      (parent-pos
                       (save-excursion
                         (org-mcp--navigate-to-parent
                          parent-path parent-id)
                         (point))))
                  (when (and (>= parent-pos node-beg)
                             (< parent-pos node-end))
                    (org-mcp--tool-validation-error
                     (concat
                      "Cannot refile a headline into itself or "
                      "its own subtree")))))
              (let*
                  ((src-beg (copy-marker (line-beginning-position)))
                   (source-buf (current-buffer))
                   (id
                    (if same-file
                        (let* ((new-level
                                (org-mcp--refile-position-at-target
                                 parent-path
                                 parent-id
                                 after-id
                                 position-sym
                                 header-end))
                               (insertion (point-marker)))
                          ;; Resolve placement before creating the ID,
                          ;; so a rejected move (a missing `after_uri'
                          ;; sibling) leaves no stale `org-id-locations'
                          ;; entry -- mirroring the cross-file branch.
                          (org-mcp--refile-capture-and-persist
                           src-beg
                           source-buf
                           new-level
                           insertion
                           source-file
                           target-file))
                      (org-mcp--with-visiting-org-file target-file
                          "refile"
                        ;; Resolve and validate the target before
                        ;; creating the ID, so a rejected move leaves no
                        ;; stale `org-id-locations' entry.  The macro's
                        ;; entry guard rejects an unsaved target buffer
                        ;; before the body validates the header and
                        ;; resolves the parent / `after_uri' sibling --
                        ;; all ahead of the ID mint.
                        (let*
                            ((target-header-end
                              (org-mcp--validate-and-skip-file-header))
                             (new-level
                              (org-mcp--refile-position-at-target
                               parent-path
                               parent-id
                               after-id
                               position-sym
                               target-header-end))
                             (insertion (point-marker)))
                          (org-mcp--refile-capture-and-persist
                           src-beg
                           source-buf
                           new-level
                           insertion
                           source-file
                           target-file))))))
                (json-encode
                 `((success . t)
                   (uri . ,(concat org-mcp--uri-id-prefix id))
                   (title . ,title)
                   (source_file
                    . ,(file-name-nondirectory source-file))
                   (target_file
                    . ,(file-name-nondirectory target-file))))))))))))

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
                  Use full URL encoding: spaces as %20, # as
                  %23, / as %2F, % as %25, etc.
                  Example: \"My%20Project/Planning\" for nested
                  headlines with spaces
                  Example: \"A%2FB%20Testing\" for headline titled
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

(defun org-mcp--build-headline-uri (file-path headline-path)
  "Build an org-headline:// URI for FILE-PATH and HEADLINE-PATH.
HEADLINE-PATH is a list of title strings; nil or empty yields the
file-level URI (trailing slash form)."
  (let ((encoded-file (replace-regexp-in-string "#" "%23" file-path)))
    (if headline-path
        (concat
         org-mcp--uri-headline-prefix
         encoded-file
         "#"
         (mapconcat #'url-hexify-string headline-path "/"))
      (concat org-mcp--uri-headline-prefix encoded-file "/"))))

(defun org-mcp--grep-section-matches (pat start end)
  "Return matching lines for PAT in buffer range [START, END).
Each entry is an alist with keys `line' (1-based) and `text'."
  (let ((matches '()))
    (save-excursion
      (goto-char start)
      (while (re-search-forward pat end t)
        (let ((line-start (line-beginning-position)))
          (push `((line . ,(line-number-at-pos line-start))
                  (text
                   .
                   ,(buffer-substring-no-properties
                     line-start (line-end-position))))
                matches)
          (forward-line 1))))
    (nreverse matches)))

(defun org-mcp--grep-file (file-path pattern case-fold)
  "Search FILE-PATH for literal PATTERN; return list of group alists.
CASE-FOLD non-nil means case-insensitive search."
  (org-mcp--with-org-file file-path
    (let* ((case-fold-search case-fold)
           (pat (regexp-quote pattern))
           (groups '())
           (first-heading-pos
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward "^\\*+ " nil t)
                  (line-beginning-position)
                (point-max)))))
      (when-let* ((pre-matches
                   (org-mcp--grep-section-matches
                    pat (point-min) first-heading-pos)))
        (push `((file . ,file-path)
                (headline_path . [])
                (uri . ,(org-mcp--build-headline-uri file-path nil))
                (matches . ,(vconcat pre-matches)))
              groups))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (beginning-of-line)
        (let* ((nodes (org-mcp--headline-path-nodes file-path))
               ;; The deepest node is the matched heading, so its URI
               ;; is the group URI.
               (uri (alist-get 'uri (car (last nodes))))
               (section-start (point))
               (section-end
                (save-excursion
                  (forward-line 1)
                  (if (re-search-forward "^\\*+ " nil t)
                      (line-beginning-position)
                    (point-max))))
               (matches
                (org-mcp--grep-section-matches
                 pat section-start section-end)))
          (when matches
            (push `((file . ,file-path)
                    (headline_path . ,(vconcat nodes))
                    (uri . ,uri)
                    (matches . ,(vconcat matches)))
                  groups))
          (goto-char section-end)))
      (nreverse groups))))

(defun org-mcp--tool-grep (pattern &optional file case_sensitive)
  "Search for PATTERN in FILE or all allowed Org files.
PATTERN is a literal substring; non-empty, single-line string required.
FILE is an optional absolute path limiting the search to one file.
CASE_SENSITIVE when non-nil requires an exact case match.

MCP Parameters:
  pattern - Literal substring to search for (required, non-empty,
            single-line, not a regex)
  file - Absolute path to an allowed Org file (optional)
         When omitted, all org-mcp-allowed-files are searched
  case_sensitive - Exact-case match when true (optional, default false)"
  (org-mcp--validate-string-field pattern "pattern")
  (when (string-empty-p pattern)
    (org-mcp--tool-validation-error
     "Field pattern must be non-empty"))
  (when (string-match-p "\n" pattern)
    (org-mcp--tool-validation-error
     "Field pattern must be a single line"))
  (org-mcp--validate-string-field file "file" t)
  (let ((canonical-file
         (when file
           (org-mcp--find-allowed-file file))))
    (when (and file (not canonical-file))
      (org-mcp--tool-validation-error
       "'%s': the referenced file not in allowed list"
       file))
    (let* ((case_sensitive
            (org-mcp--normalize-json-boolean case_sensitive))
           (case-fold (not case_sensitive))
           (files
            (if file
                (list canonical-file)
              (seq-filter
               #'file-exists-p
               (mapcar #'expand-file-name org-mcp-allowed-files))))
           (groups
            (apply #'append
                   (mapcar
                    (lambda (f)
                      (org-mcp--grep-file f pattern case-fold))
                    files))))
      (json-encode `((groups . ,(vconcat groups)))))))

(defun org-mcp--tool-find-tagged-ancestor
    (uri tag &optional include_self)
  "Find the nearest headline at or above URI carrying TAG as a local tag.
INCLUDE_SELF non-nil checks the headline at URI itself before its
ancestors; otherwise the walk starts at its parent.

MCP Parameters:
  uri - URI of the headline to start from
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}
  tag - Single tag name to test against each level's local tags
        (non-empty)
  include_self - When true, the headline itself is checked before
                 its ancestors (optional, default false)"
  (org-mcp--validate-string-field uri "uri")
  (org-mcp--validate-string-field tag "tag")
  (when (string-empty-p tag)
    (org-mcp--tool-validation-error "Field tag must be non-empty"))
  (setq include_self (org-mcp--normalize-json-boolean include_self))
  (pcase-let ((`(,file-path . ,headline-path)
               (org-mcp--parse-resource-uri-with-headline uri)))
    (org-mcp--with-org-file file-path
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--uri-id-prefix uri))
      (let* ((nodes (org-mcp--headline-path-nodes file-path))
             (candidates
              (if include_self
                  nodes
                (butlast nodes)))
             (found
              (cl-find-if
               (lambda (node)
                 (seq-contains-p (alist-get 'tags node) tag))
               candidates
               :from-end t)))
        (json-encode `((found . ,found)))))))

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-server
   :id org-mcp--server-id
   :name org-mcp--server-id
   :version org-mcp--version
   :instructions org-mcp--instructions
   :tools
   (list
    (list
     #'org-mcp--tool-get-todo-config
     :id "org-get-todo-config"
     :description
     "Get the TODO keyword configuration from the current Emacs
Org-mode settings.  Returns information about task state sequences
and their semantics.

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
     :read-only t)
    (list
     #'org-mcp--tool-get-tag-config
     :id "org-get-tag-config"
     :description
     "Get tag-related configuration from the current Emacs Org-mode
settings.  Returns literal Elisp variable values as strings for tag
configuration introspection.

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
     :read-only t)
    (list
     #'org-mcp--tool-get-allowed-files
     :id "org-get-allowed-files"
     :description
     "Get the list of Org files accessible through the org-mcp
server.  Returns the configured allowed files exactly as specified in
org-mcp-allowed-files.

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
     :read-only t)
    (list
     #'org-mcp--tool-get-agenda-config
     :id "org-get-agenda-config"
     :description
     "List the custom agenda commands defined in
`org-agenda-custom-commands'.  A runnable key can be passed as
org-get-agenda's view.

Returns JSON object:
  commands (array) - One entry per custom command, each with:
    key - The command's dispatch key string
    description - The command's description
    type - The command's kind, one of \"prefix\" (a bare key group),
           \"agenda\", \"agenda*\", \"todo\", \"todo-tree\",
           \"alltodo\", \"tags\", \"tags-todo\", \"tags-tree\",
           \"search\", \"occur-tree\", \"stuck\", \"composite\" (a
           multi-block command), or \"function\" (a user function)
    raw - Present only for \"function\" entries: the entry's literal
          Elisp, as a fallback for a client that wants to inspect it

type is reported as a fact and does not promise runnability."
     :read-only t)
    (list
     #'org-mcp--tool-get-agenda
     :id "org-get-agenda"
     :description
     "Run an Org agenda view and return both its plain text and its
entries as structured data.  view is either a builtin span (\"day\",
\"week\", or \"month\", as produced by `org-agenda-list') or a dispatch
key from `org-agenda-custom-commands' (discover keys with
org-get-agenda-config).  The agenda is built only from non-missing
files in `org-mcp-allowed-files' (it does not use other
`org-agenda-files' configuration).  Caveat: a custom command whose own
settings set `org-agenda-files' or a restriction can override this and
read files outside the allow-list.

Returns JSON object:
  view - The span name (\"day\", \"week\", \"month\") or the custom
         dispatch key you passed
  date - The date argument you passed, or the string \"today\"
  start_day - For a span, the first day the agenda covers, as
              YYYY-MM-DD (resolved; a month snapped to the first of the
              calendar month; a week aligned per the user's
              `org-agenda-start-on-weekday', the reference day itself
              when that is nil).  null for a non-dated custom view
  agenda - The full agenda buffer text, unchanged, including any lines
           (diary, clock, informational) that have no source heading
  blocks - Array of agenda blocks (one for a span view -- its items may
           be empty; several for a composite custom command).  Each
           block has:
             header - The block's header line, or null
             items - Array of entry nodes, each with title, todo,
                     priority, tags, scheduled, deadline, and uri
                     (org-id:// or org-headline://), the same node
                     shape org-grep emits.  A heading appears at most
                     once per block (a multi-day span entry that the
                     text shows on several days is one item here).
                     Lines with no source heading appear only in
                     agenda text, not here

A custom command that prompts for input (e.g. a tags or search command
with no baked-in match, or a bare prefix key) cannot run
non-interactively and is rejected with an error."
     :read-only t)
    (list
     #'org-mcp--tool-update-todo-state
     :id "org-update-todo-state"
     :description
     "Update the TODO state of an Org headline.  Changes the task state
while preserving the headline title, tags, and other properties.
Creates an Org ID property for the headline if one doesn't exist.

Returns JSON object:
  success - Always true on success (boolean)
  previous_state - The previous TODO state (string, empty for none)
  new_state - The actual resulting TODO state, which may differ from
              the request -- e.g. a repeating entry marked done is
              reset to its not-done keyword (string, empty if cleared)
  uri - ID-based URI (org-id://{uuid}) for the updated headline"
     :read-only nil)
    (list
     #'org-mcp--tool-add-todo
     :id "org-add-todo"
     :description
     "Add a new TODO item to an Org file at a specified location.
Creates the headline with TODO state, tags, and optional body content.
Automatically creates an Org ID property for the new headline.

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the new headline
  file - Filename (not full path) where item was added
  title - The headline title that was created

Positioning behavior.  Five scenarios, selected by parent_uri shape
and the optional after_uri / position parameters:

  Child, end (default).
    Inputs: parent_uri with fragment (#PATH) or org-id://UUID; no
    after_uri; position omitted or \"end\".
    Effect: appended as the last child of the parent.

  Child, after sibling.
    Inputs: parent_uri + after_uri=org-id://UUID-of-sibling; no
    position.
    Effect: inserted as the immediate next sibling of after_uri's
    headline.

  Child, start.
    Inputs: parent_uri + position=\"start\"; no after_uri.
    Effect: inserted as the first child of the parent, after the
    parent's property drawer, planning line, logbook entries, and
    plain-text body -- i.e. just before the parent's first
    existing child heading.  When the parent has no children,
    `start' collapses to `end' at end-of-subtree.

  Top-level, end (default).
    Inputs: parent_uri with no fragment; no after_uri; position
    omitted or \"end\".
    Effect: appended at end of file.

  Top-level, start.
    Inputs: parent_uri (no fragment) + position=\"start\"; no
    after_uri.
    Effect: inserted before the first existing top-level heading
    but after the file's entire header block.  The header block
    is every leading line starting with `#' (including
    #+-prefixed keywords like #+TITLE:, file-local variable lines
    like `# -*- mode: org -*-', and Org comment lines), plus any
    :NAME:...:END: drawer in the leading run (:PROPERTIES:,
    :LOGBOOK:, and any custom drawer keyword are all consumed
    uniformly, even when Org's grammar would classify the drawer
    as a generic drawer rather than a file-level property drawer,
    to avoid silently reparenting it onto the new heading).  All
    header-class lines may have optional leading whitespace,
    matching Org's parser; headings themselves still require
    column 0.  When the file has no existing top-level heading
    (empty file, header-only, or zeroth-section content like a
    plain paragraph after the header), `start' collapses to `end'
    at point-max rather than inserting after the header block.
    This preserves any zeroth-section content in place instead of
    absorbing it into the new heading's body.

Combinations outside these five are rejected at the tool boundary.
After_uri cannot combine with an explicit position (which includes
position=\"end\", even though that is the default -- omit position
entirely to use after_uri-based placement), nor with a top-level
parent_uri (which has no sibling slot)."
     :read-only nil)
    (list
     #'org-mcp--tool-edit-headline
     :id "org-edit-headline"
     :description
     "Edit an Org headline's title and/or local tags, preserving its
TODO state, planning, properties, and body content.  Supply at least
one of new_title, add_tags, remove_tags; a tag may not appear in both
add_tags and remove_tags.  Tag edits are a delta over the headline's
own (local) tags and are idempotent -- adding a present tag or
removing an absent one is a no-op, and duplicates within add_tags are
collapsed.  add_tags are validated against the globally configured tag
alist (a file's own #+TAGS: additions are not honored); remove_tags
accept any syntactically valid tag.  Local tags only: an inherited tag
cannot be shed here (refile the headline out of the tagged subtree
instead), mutual-exclusion groups are not auto-enforced across the
result (swap within a group by removing the old tag and adding the new
one in one call; a single add_tags list may not itself hold two members
of one group), and the crypt tag is treated as a plain tag (no
org-crypt encryption).
Creates an Org ID property for the headline if one doesn't exist.

Returns JSON object:
  success - Always true on success (boolean)
  title - The resulting headline title (string)
  todo - The TODO keyword, or null
  priority - The priority cookie letter, or null
  tags - The resulting local tags (array of strings)
  scheduled - The SCHEDULED timestamp, or null
  deadline - The DEADLINE timestamp, or null
  uri - ID-based URI (org-id://{uuid}) for the headline"
     :read-only nil)
    (list
     #'org-mcp--tool-edit-body
     :id "org-edit-body"
     :description
     "Edit the body content of an Org headline using partial string
replacement.  Finds and replaces a substring within the headline's
body text.  Creates an Org ID property for the headline if one doesn't
exist.

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the edited headline"
     :read-only nil)
    (list
     #'org-mcp--tool-set-planning
     :id "org-set-planning"
     :description
     "Set, change, or clear a headline's SCHEDULED and/or DEADLINE
planning timestamps via `org-schedule'/`org-deadline'.  Org produces
the relative-date resolution, weekday formatting, and planning-line
placement; trailing repeater (+1w, ++1w, .+1w) and warning (-3d)
cookies in a date string are spliced into the timestamp.  When
re-scheduling, omitting cookies preserves an existing repeater/warning
while including any cookie replaces the field's cookies.  Provide at
least one of scheduled, deadline, clear_scheduled, clear_deadline; a
field and its clear_* cannot be combined.  Creates an Org ID property
for the headline if one doesn't exist.

Returns JSON object:
  success - Always true on success (boolean)
  scheduled - Resulting SCHEDULED timestamp string, or null
  deadline - Resulting DEADLINE timestamp string, or null
  uri - ID-based URI (org-id://{uuid}) for the headline"
     :read-only nil)
    (list
     #'org-mcp--tool-archive-subtree
     :id "org-archive-subtree"
     :description
     "Archive an Org headline subtree to its configured archive
location using `org-archive-subtree'.  Respects the headline's
ARCHIVE property, the file's #+ARCHIVE: setting, and the global
`org-archive-location' (in that order).  Creates an Org ID
property for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline to archive (string, required)
        Formats:
          - org-headline://{absolute-path}#{url-encoded-path}
          - org-id://{uuid}

Returns JSON object:
  success - Always true on success (boolean)
  archive_file - Absolute path to the archive file (string);
        equals the source file's own path when the archive
        location is in-file (empty file part before ::)
  uri - org-id:// URI of the archived headline (string);
        resolvable via resources/read only if the archive file is
        in org-mcp-allowed-files, otherwise informational only"
     :read-only nil)
    (list
     #'org-mcp--tool-refile-headline
     :id "org-refile-headline"
     :description
     "Move an existing headline and its entire subtree to a new parent
or file, preserving its TODO state, tags, properties, ID, body, and
child structure.  The whole subtree is re-leveled to fit the new
parent.  Creates an Org ID for the moved headline if it has none.

Both the source file and the target file must be members of
org-mcp-allowed-files.  The existing ID is preserved, including across
files, and the returned org-id:// URI resolves to the moved node in
its new location.

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the moved headline
  title - The moved headline's title
  source_file - Filename (not full path) the node moved from
  target_file - Filename (not full path) the node moved to

Placement under target_parent_uri mirrors org-add-todo:

  Child, end (default).
    target_parent_uri with fragment (#PATH) or org-id://UUID; no
    after_uri; position omitted or \"end\".  Appended as the last
    child of the parent.

  Child, after sibling.
    target_parent_uri + after_uri=org-id://UUID-of-sibling; no
    position.  Inserted as the immediate next sibling of after_uri's
    headline.

  Child, start.
    target_parent_uri + position=\"start\"; no after_uri.  Inserted
    as the first child of the parent.

  Top-level, end (default) / start.
    target_parent_uri with no fragment (org-headline://FILE/); no
    after_uri.  Appended at end of file, or with position=\"start\"
    inserted before the first existing top-level heading (after the
    file header block).

after_uri cannot combine with an explicit position, nor with a
top-level target_parent_uri, nor reference the node being moved.

Moving a node into itself or its own subtree is rejected.  When the
node already sits in the requested place and already has an ID, the
file is left unchanged."
     :read-only nil)
    (list
     #'org-mcp--tool-read-file
     :id "org-read-file"
     :description
     "Read complete raw content of an Org file. Returns entire file as
plain text with all formatting, properties, and structure preserved.
File must be in org-mcp-allowed-files.

Returns: Plain text content of the entire Org file"
     :read-only t)
    (list
     #'org-mcp--tool-read-outline
     :id "org-read-outline"
     :description
     "Get hierarchical structure of Org file as JSON outline. Returns
   all headline titles and nesting relationships up to 2 levels
   deep. File must be in org-mcp-allowed-files.

Returns: JSON object with hierarchical outline structure. Each node
carries title, level, children, plus todo, priority, local tags,
scheduled, deadline, and uri; see the org-outline:// resource for
null/empty semantics."
     :read-only t)
    (list
     #'org-mcp--tool-read-headline
     :id "org-read-headline"
     :description
     "Read specific Org headline by hierarchical path. Returns headline
with TODO state, tags, properties, body text, and all nested
subheadings, plus the chain of its ancestors. File must be in
org-mcp-allowed-files.

Returns JSON object:
  headline_path - Array of node objects tracing the path from the
                  outermost ancestor to the read headline itself
                  (the last entry).  Same node shape as org-grep's
                  headline_path: title, todo, priority, tags,
                  scheduled, deadline, and uri.
  content       - Text of the headline and its subtree"
     :read-only t)
    (list
     #'org-mcp--tool-read-by-id
     :id "org-read-by-id"
     :description
     "Read Org headline by its unique ID property. More stable than
path-based access since IDs don't change when headlines are renamed
or moved. File containing the ID must be in org-mcp-allowed-files.

Returns: JSON object with headline_path and content, same shape as
org-read-headline"
     :read-only t)
    (list
     #'org-mcp--tool-grep
     :id "org-grep"
     :description
     "Search for a literal substring across one or all allowed Org
files.  Returns per-section groups of matching lines, each annotated
with a headline path and a resource URI for each matching section.

Returns JSON object:
  groups - Array of match groups, each containing:
    file           - Absolute path of the source file
    headline_path  - Array of node objects tracing the path to the
                     containing section, from the outermost ancestor
                     to the section's own headline (empty array for
                     pre-heading content).  Each object has title,
                     todo, priority, tags, scheduled, deadline, and
                     uri.  The last entry's uri equals the group uri
                     below.
    uri            - Resource URI: org-id:// when the section has an
                     ID property; org-headline:// otherwise.
                     Pass directly to resources/read.  To use the
                     read tools instead, extract the uuid
                     (org-read-by-id) or file + fragment path
                     (org-read-headline) from the URI.
    matches        - Array of {line, text} objects (line is 1-based)

Groups appear in document order, per file in org-mcp-allowed-files
order.  A new group starts when the containing section changes.
One entry per source line.

Returns {\"groups\": []} when no files are configured and no file
is given, or when the pattern has no matches."
     :read-only t)
    (list
     #'org-mcp--tool-find-tagged-ancestor
     :id "org-find-tagged-ancestor"
     :description
     "Find the nearest enclosing headline that carries a given tag.
Starting from the headline at uri, tests each level's own (local)
tag list for tag -- the headline itself first when include_self is
true, then each ancestor outward -- and returns the first level
that declares it.  Tags a headline merely inherits from an ancestor
or from #+FILETAGS never match, and org-tags-exclude-from-inheritance
is deliberately ignored.  A tag declared only in #+FILETAGS has no
declaring headline, so the result is null.  Under Org's default tag
inheritance, a non-null result with include_self=true means the
headline carries the tag.  File must be in org-mcp-allowed-files.

Returns JSON object:
  found - Node object of the nearest self-or-ancestor headline whose
          own tags contain tag, or null when no enclosing headline
          declares it.  Same node shape as org-grep's headline_path
          entries: title, todo, priority, tags, scheduled, deadline,
          uri (org-id:// when the headline has an ID property,
          org-headline:// otherwise)."
     :read-only t))
   :resources
   (list
    ;; Register template resources for org files
    (list
     "org://{filename}" #'org-mcp--handle-file-resource
     :name "Org file"
     :description
     "Access the complete raw content of an Org file.  Returns the
entire file as plain text, preserving all formatting, properties, and
structure.

URI format: org://{filename}
  filename - Absolute path to the Org file (required)

Returns: Plain text content of the entire Org file"
     :mime-type "text/plain")
    (list
     "org-outline://{filename}" #'org-mcp--handle-outline-resource
     :name "Org file outline"
     :description
     "Get the hierarchical structure of an Org file as a JSON
outline.  Extracts headline titles and their nesting relationships up
to 2 levels deep.

URI format: org-outline://{filename}
  filename - Absolute path to the Org file (required)

Returns: JSON object with structure:
  {
    \"headings\": [
      {
        \"title\": \"Top-level heading\",
        \"todo\": \"TODO\",
        \"priority\": \"A\",
        \"tags\": [\"work\"],
        \"scheduled\": null,
        \"deadline\": null,
        \"uri\": \"org-headline:///path/to/file.org#Top-level%20heading\",
        \"level\": 1,
        \"children\": [
          {
            \"title\": \"Subheading\",
            \"todo\": null,
            \"priority\": null,
            \"tags\": [],
            \"scheduled\": null,
            \"deadline\": null,
            \"uri\": \"org-headline:///path/to/file.org#Top-level%20heading/Subheading\",
            \"level\": 2,
            \"children\": []
          }
        ]
      }
    ]
  }

Node fields:
  title     - Headline text
  todo      - TODO state string, or null when none
  priority  - Single-letter priority string, or null when no
              [#x] cookie
  tags      - Array of the headline's own (local) tags, [] when
              none; inherited tags are not included
  scheduled - Raw SCHEDULED timestamp string, or null
  deadline  - Raw DEADLINE timestamp string, or null
  uri       - org-id:// when the headline has an ID property,
              org-headline:// otherwise
  level     - Heading level (1 or 2)
  children  - Array of child nodes (empty beyond level 2)

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
     :mime-type "application/json")
    (list
     (concat org-mcp--uri-headline-prefix "{filename}")
     #'org-mcp--handle-headline-resource
     :name "Org headline content"
     :description
     "Access content of a specific Org headline by its path in the
file hierarchy.  Returns the headline and all its subheadings, plus
the chain of its ancestors.

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

Returns (with a headline path): JSON object with headline_path (the
ancestor chain in org-grep's node shape, from the outermost ancestor
to this headline itself) and content (the headline with TODO state,
tags, properties drawer, body text, and all nested subheadings).

Returns (no fragment): the entire file as plain text, not JSON.

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
    → Entire file as plain text (no fragment means whole file)

Use this resource to:
  - Read specific sections of an Org file
  - Access headline content by hierarchical path
  - Get complete subtree including all children"
     :mime-type "text/plain")
    (list
     (concat org-mcp--uri-id-prefix "{uuid}")
     #'org-mcp--handle-id-resource
     :name "Org node by ID"
     :description
     "Access content of an Org headline by its unique ID property.
More stable than path-based access since IDs don't change when
headlines are renamed or moved.

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

Returns: JSON object with headline_path (the ancestor chain in
org-grep's node shape, ending with this headline itself) and content
(the headline with TODO state, tags, properties drawer, body text,
and all nested subheadings)

Example URIs:
  org-id://550e8400-e29b-41d4-a716-446655440000
    → Headline with that ID property

Use this resource to:
  - Access headlines by stable identifier
  - Reference content that may be renamed or moved
  - Build cross-references between Org nodes"
     :mime-type "application/json"))))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-server org-mcp--server-id))

(provide 'org-mcp)
;;; org-mcp.el ends here
