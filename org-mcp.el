;;; org-mcp.el --- MCP server for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 org-mcp contributors

;; Author: org-mcp contributors
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.2.0"))
;; Homepage: https://github.com/laurynas-biveinis/org-mcp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a Model Context Protocol (MCP) server for
;; Org-mode, enabling AI assistants and other MCP clients to interact
;; with Org files.

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

;; Internal constants for URI prefixes
(defconst org-mcp--org-headline-prefix "org-headline://"
  "URI prefix for headline resources.")

(defconst org-mcp--org-id-prefix "org-id://"
  "URI prefix for ID-based resources.")

(defconst org-mcp--server-id "org-mcp"
  "Server ID for org-mcp MCP server registration.")

;; Helper macros

(defmacro org-mcp--with-org-file-buffer
    (file-path response-alist &rest body)
  "Execute BODY in a temp buffer set up for Org file at FILE-PATH.
Sets up the buffer with:
- File name set for org-id functionality
- File contents loaded
- `org-mode' enabled
- Point at beginning of buffer
After BODY executes, saves the buffer and returns the result of
`org-mcp--complete-and-save' with FILE-PATH and RESPONSE-ALIST.
BODY can access FILE-PATH and RESPONSE-ALIST as variables."
  (declare (indent 2) (debug (form form body)))
  `(with-temp-buffer
     (set-visited-file-name ,file-path t)
     (insert-file-contents ,file-path)
     (org-mode)
     (goto-char (point-min))
     ,@body
     (org-mcp--complete-and-save ,file-path ,response-alist)))

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
  "Throw validation error for tool operations.
MESSAGE is the format string, ARGS are format arguments."
  (mcp-server-lib-tool-throw (apply #'format message args)))

(defun org-mcp--resource-validation-error (message &rest args)
  "Signal validation error for resource operations.
MESSAGE is the format string, ARGS are format arguments."
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
   (format "Cannot find %s: '%s'" resource-type identifier)))

(defun org-mcp--tool-file-access-error (locator)
  "Throw file access error for tool operations.
LOCATOR is the resource identifier (file path or ID) that was
denied access."
  (mcp-server-lib-tool-throw
   (format "'%s': the referenced file not in allowed list" locator)))

(defun org-mcp--resource-file-access-error (locator)
  "Signal file access error for resource operations.
LOCATOR is the resource identifier (file path or ID) that was
denied access."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "'%s': the referenced file not in allowed list" locator)))

(defmacro org-mcp--with-uri-prefix-dispatch
    (uri headline-body id-body)
  "Dispatch tool URI handling based on prefix.
URI is the URI string to dispatch on.
HEADLINE-BODY is executed for org-headline:// URIs,
with the URI after the prefix bound to `uri-without-prefix'.
ID-BODY is executed when URI starts with `org-mcp--org-id-prefix',
with the URI after the prefix bound to `id'.
Throws an error if neither prefix matches."
  (declare (indent 1))
  `(cond
    ((string-prefix-p org-mcp--org-headline-prefix ,uri)
     (let ((uri-without-prefix
            (substring ,uri (length org-mcp--org-headline-prefix))))
       ,headline-body))
    ((string-prefix-p org-mcp--org-id-prefix ,uri)
     (let ((id (substring ,uri (length org-mcp--org-id-prefix))))
       ,id-body))
    (t
     (org-mcp--tool-validation-error "Invalid resource URI format: %s"
                                     ,uri))))

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
              (push `((state . ,(car (org-remove-keyword-keys (list kw))))
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
  (json-encode
   `((files . ,(vconcat org-mcp-allowed-files)))))

(defun org-mcp--read-file-resource (file-path)
  "Read and return the contents of FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))


(defun org-mcp--paths-equal-p (path1 path2)
  "Return t if PATH1 and PATH2 refer to the same file.
Handles symlinks and path variations by normalizing both paths."
  (string= (file-truename path1) (file-truename path2)))

(defun org-mcp--find-allowed-file (filename)
  "Find FILENAME in `org-mcp-allowed-files' and return the full path.
FILENAME must be an absolute path.
Returns nil if the file is not in the allowed list."
  ;; Normalize the filename to handle symlinks and path variations
  (let ((normalized-filename (file-truename filename)))
    (cl-find
     normalized-filename
     org-mcp-allowed-files
     :test #'org-mcp--paths-equal-p)))

(defun org-mcp--find-allowed-file-with-id (id)
  "Find an allowed file containing the Org ID.
First looks up in the org-id database, then validates the file is in
the allowed list.
Returns the expanded file path if found and allowed,
nil otherwise.
Throws a tool error if ID exists but file is not allowed."
  (if-let* ((id-file (org-id-find-id-file id)))
    ;; ID found in database, check if file is allowed
    (let ((allowed-file (org-mcp--find-allowed-file id-file)))
      (unless allowed-file
        (org-mcp--tool-file-access-error id))
      (expand-file-name allowed-file))
    ;; ID not in database - might not exist or DB is stale
    ;; Fall back to searching allowed files manually
    (let ((found-file nil))
      (dolist (allowed-file org-mcp-allowed-files)
        (unless found-file
          (when (file-exists-p allowed-file)
            (with-temp-buffer
              (insert-file-contents allowed-file)
              (org-mode)
              (goto-char (point-min))
              (when (org-find-property "ID" id)
                (setq found-file (expand-file-name allowed-file)))))))
      found-file)))

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

(defun org-mcp--generate-outline (file-path)
  "Generate JSON outline structure for FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (let ((headings (org-mcp--extract-headings)))
      `((headings . ,headings)))))

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

(defun org-mcp--build-heading-tree (flat-headings)
  "Build hierarchical tree from FLAT-HEADINGS list."
  (let ((result '())
        (stack '()))
    (dolist (heading flat-headings)
      (let ((level (alist-get 'level heading)))
        ;; Pop stack until we find the right parent level
        (while (and stack (>= (car (car stack)) level))
          (pop stack))
        ;; If we have a parent, add to its children
        (if stack
            (let* ((parent-entry (car stack))
                   (parent-heading (cdr parent-entry))
                   (children (alist-get 'children parent-heading))
                   (new-children (vconcat children (vector heading))))
              (setcdr (assq 'children parent-heading) new-children))
          ;; No parent, this is a top-level heading
          (push heading result))
        ;; Push this heading to stack as potential parent
        ;; Store level and heading separately to avoid circular ref
        (push (cons level heading) stack)))
    (vconcat (nreverse result))))

(defun org-mcp--handle-outline-resource (params)
  "Handler for org://{filename}/outline template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (org-mcp--validate-file-access filename)))
    (let ((outline
           (org-mcp--generate-outline
            (expand-file-name allowed-file))))
      (json-encode outline))))

(defun org-mcp--handle-file-resource (params)
  "Handler for org://{filename} template.
PARAMS is an alist containing the filename parameter."
  (let* ((filename (alist-get "filename" params nil nil #'string=))
         (allowed-file (org-mcp--validate-file-access filename)))
    (org-mcp--read-file-resource (expand-file-name allowed-file))))

(defun org-mcp--encode-file-path (file-path)
  "Encode special characters in FILE-PATH for URI.
Encodes # as %23 to avoid fragment separator confusion."
  (replace-regexp-in-string "#" "%23" file-path))

(defun org-mcp--decode-file-path (encoded-path)
  "Decode special characters from ENCODED-PATH.
Specifically decodes %23 back to #."
  (replace-regexp-in-string "%23" "#" encoded-path))

(defun org-mcp--split-headline-uri (path-after-protocol)
  "Split PATH-AFTER-PROTOCOL into (file-path . headline-path).
PATH-AFTER-PROTOCOL is the part after `org-headline://'.
Returns (FILE . HEADLINE) where FILE is the decoded file path and
HEADLINE is the part after the fragment separator.
File paths with # characters should be encoded as %23."
  (if-let* ((hash-pos (string-match "#" path-after-protocol)))
    (cons
     (org-mcp--decode-file-path
      (substring path-after-protocol 0 hash-pos))
     (substring path-after-protocol (1+ hash-pos)))
    (cons (org-mcp--decode-file-path path-after-protocol) nil)))

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
                (expand-file-name allowed-file) headline-path)))
          (unless content
            (org-mcp--resource-not-found-error
             "headline" (mapconcat #'identity headline-path "/")))
          content)
      ;; No headline path means get entire file
      (org-mcp--read-file-resource (expand-file-name allowed-file)))))

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

(defun org-mcp--get-headline-content (file-path headline-path)
  "Get content for headline at HEADLINE-PATH in FILE-PATH.
HEADLINE-PATH is a list of headline titles to traverse.
Returns the content string or nil if not found."
  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))
    (when (org-mcp--navigate-to-headline headline-path)
      (org-mcp--extract-headline-content))))

(defun org-mcp--extract-headline-content ()
  "Extract content of current headline including the headline itself.
Point should be at the headline."
  (let ((start (line-beginning-position)))
    (org-end-of-subtree t t)
    ;; Remove trailing newline if present
    (when (and (> (point) start) (= (char-before) ?\n))
      (backward-char))
    (buffer-substring-no-properties start (point))))


(defun org-mcp--handle-id-resource (params)
  "Handler for org-id://{uuid} template.
PARAMS is an alist containing the uuid parameter."
  (let* ((id (alist-get "uuid" params nil nil #'string=))
         ;; Use org-id-find-id-file to get the file path
         (file-path (org-id-find-id-file id)))
    (unless file-path
      (org-mcp--resource-not-found-error "ID" id))
    ;; Validate that the file is in allowed list
    (let ((allowed-file (org-mcp--find-allowed-file file-path)))
      (unless allowed-file
        (org-mcp--resource-file-access-error id))
      ;; Get the content
      (org-mcp--get-content-by-id
       (expand-file-name allowed-file) id))))

(defun org-mcp--parse-resource-uri (uri)
  "Parse URI and return (file-path . headline-path).
Validates file access and returns expanded file path."
  (let (file-path
        headline-path)
    (org-mcp--with-uri-prefix-dispatch
        uri
      ;; Handle org-headline:// URIs
      (let* ((split-result
              (org-mcp--split-headline-uri uri-without-prefix))
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
        (unless file-path
          (org-mcp--id-not-found-error id))
        (setq headline-path (list id))))
    (cons file-path headline-path)))

(defun org-mcp--goto-headline-from-uri (headline-path is-id)
  "Navigate to headline based on HEADLINE-PATH and IS-ID flag.
If IS-ID is non-nil, treats HEADLINE-PATH as containing an ID.
Otherwise, navigates using HEADLINE-PATH as title hierarchy."
  (if is-id
      ;; ID case - headline-path contains single ID
      (let ((pos (org-find-property "ID" (car headline-path))))
        (if pos
            (goto-char pos)
          (org-mcp--id-not-found-error (car headline-path))))
    ;; Path case - headline-path contains title hierarchy
    (unless (org-mcp--navigate-to-headline headline-path)
      (org-mcp--headline-not-found-error headline-path))))

(defun org-mcp--check-buffer-modifications (file-path operation)
  "Check if FILE-PATH has unsaved change in any buffer.
OPERATION is a string describing the operation for error messages."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (string= (buffer-file-name) file-path)
                 (buffer-modified-p))
        (org-mcp--tool-validation-error
         "Cannot %s: file has unsaved changes in buffer"
         operation)))))

(defun org-mcp--refresh-file-buffers (file-path)
  "Refresh all buffers visiting FILE-PATH."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (string= (buffer-file-name) file-path))
        (revert-buffer t t t)))))

(defun org-mcp--get-content-by-id (file-path id)
  "Get content for org node with ID in FILE-PATH.
Returns the content string or nil if not found."
  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))
    ;; Find the headline with this ID
    (let ((pos (org-find-property "ID" id)))
      (when pos
        (goto-char pos)
        (org-mcp--extract-headline-content)))))

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
      `((uri . ,(concat org-mcp--org-id-prefix id)))))))

(defun org-mcp--get-valid-todo-states ()
  "Get list of valid TODO states without annotations or separators.
Processes `org-todo-keywords' by extracting all keywords, stripping
annotations like \"(t!)\" using `org-remove-keyword-keys', and removing
the \"|\" separator."
  (delete
   "|"
   (org-remove-keyword-keys
    (apply #'append (mapcar #'cdr org-todo-keywords)))))

(defun org-mcp--tool-update-todo-state (uri current_state new_state)
  "Update the TODO state of a headline.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the updated headline.
URI is the URI of the headline to update.
CURRENT_STATE is the current TODO state (empty string for no state).
NEW_STATE is the new TODO state to set.

MCP Parameters:
  uri - URI of the headline (org-headline://{absolute-path}#{headline-path}
        or org-id://{id})
  current_state - Current TODO state (empty string for no state)
  new_state - New TODO state (must be in `org-todo-keywords')"
  ;; Parse the resource URI
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Validate new state is in org-todo-keywords
    (let ((valid-states (org-mcp--get-valid-todo-states)))
      (unless (member new_state valid-states)
        (org-mcp--tool-validation-error
         "Invalid TODO state: '%s'.  Valid states: %s"
         new_state (mapconcat #'identity valid-states ", "))))

    ;; Check for unsaved changes
    (org-mcp--check-buffer-modifications file-path "update")

    ;; Update the TODO state in the file
    (org-mcp--with-org-file-buffer file-path
        `((previous_state . ,(or current_state ""))
          (new_state . ,new_state))
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--org-id-prefix uri))

      ;; Check current state matches
      (beginning-of-line)
      (let ((actual-state (org-get-todo-state)))
        (unless (string= actual-state current_state)
          (org-mcp--state-mismatch-error
           (or current_state "(no state)")
           (or actual-state "(no state)") "State")))

      ;; Update the state
      (org-todo new_state))))

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
    (let
        ((current-block nil)) ; Current block type or nil
      ;; Scan forward for all block markers
      ;; Block names can be any non-whitespace chars
      (while (re-search-forward
              "^#\\+\\(BEGIN\\|END\\|begin\\|end\\)_\\(\\S-+\\)"
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
  "Normalize TAGS parameter to a list format.
TAGS can be:
- nil or empty list -> returns nil
- vector (JSON array) -> converts to list
- string -> wraps in list
- list -> returns as-is
Throws error for invalid types."
  (cond
   ((null tags)
    nil) ; No tags (nil or empty list)
   ((vectorp tags)
    (append tags nil)) ; Convert JSON array (vector) to list
   ((listp tags)
    tags) ; Already a list
   ((stringp tags)
    (list tags)) ; Single tag string
   (t
    (org-mcp--tool-validation-error "Invalid tags format: %s" tags))))

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
  title - The headline text
  todo_state - TODO state from `org-todo-keywords'
  tags - Tags to add (single string or array of strings)
  body - Optional body text content
  parent_uri - Parent item URI
               (org-headline://{absolute-path}#{headline-path}
               or org-id://{id}) (required)
  after_uri - Sibling to insert after
              (org-headline://{absolute-path}#{headline-path}
              or org-id://{id}) (optional)"
  (org-mcp--validate-headline-title title)

  ;; Normalize tags and get valid TODO states
  (let ((tag-list (org-mcp--normalize-tags-to-list tags))
        (valid-states (org-mcp--get-valid-todo-states)))

    ;; Validate TODO state
    (unless (member todo_state valid-states)
      (org-mcp--tool-validation-error
       "Invalid TODO state: '%s'.  Valid states: %s"
       todo_state (mapconcat #'identity valid-states ", ")))

    ;; Validate tags
    ;; Get all allowed tags from tag alists
    (let ((allowed-tags
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
         tag-list org-tag-persistent-alist)))

    ;; Parse parent URI to get file path
    (let (file-path)
      (org-mcp--with-uri-prefix-dispatch
          parent_uri
        ;; Handle org-headline:// URIs
        (let* ((split-result
                (org-mcp--split-headline-uri uri-without-prefix))
               (filename (car split-result))
               (allowed-file
                (org-mcp--validate-file-access filename)))
          (setq file-path (expand-file-name allowed-file)))
        ;; Handle org-id:// URIs
        (let ((allowed-file (org-mcp--find-allowed-file-with-id id)))
          (unless allowed-file
            (org-mcp--id-not-found-error id))
          (setq file-path allowed-file)))

      ;; Check for unsaved changes
      (org-mcp--check-buffer-modifications file-path "add TODO")

      ;; Add the TODO item
      (org-mcp--with-org-file-buffer file-path
          `((file . ,(file-name-nondirectory file-path))
            (title . ,title))
        ;; Navigate to insertion point based on parentUri
        (let ((parent-path nil)
              (parent-id nil)
              (parent-level nil))
          ;; Parse parent URI (org-headline:// or org-id://)
          (org-mcp--with-uri-prefix-dispatch
              parent_uri
            ;; org-headline:// format
            (let* ((split-result
                    (org-mcp--split-headline-uri uri-without-prefix))
                   (path-str (cdr split-result)))
              (when (and path-str (> (length path-str) 0))
                (setq parent-path
                      (mapcar
                       #'url-unhex-string
                       (split-string path-str "/")))))
            ;; org-id:// format
            (setq parent-id id))

          ;; Navigate to parent if specified
          (if (or parent-path parent-id)
              (progn
                (org-mcp--goto-headline-from-uri
                 (or (and parent-id (list parent-id))
                     parent-path)
                 parent-id)
                ;; Save parent level before moving point
                ;; Ensure we're at the beginning of headline
                (org-back-to-heading t)
                (setq parent-level (org-current-level)))
            ;; No parent specified - top level
            ;; Skip past any header comments (#+TITLE, #+AUTHOR, etc.)
            (while (and (not (eobp)) (looking-at "^#\\+"))
              (forward-line))
            ;; Position correctly: if blank line after headers,
            ;; skip it; if headline immediately after, stay
            (when (and (not (eobp)) (looking-at "^[ \t]*$"))
              ;; On blank line after headers, skip
              (while (and (not (eobp)) (looking-at "^[ \t]*$"))
                (forward-line))))

          ;; Handle positioning after navigation to parent
          (when (or parent-path parent-id)
            ;; Handle after_uri positioning
            (if (and after_uri (not (string-empty-p after_uri)))
                (progn
                  ;; Parse afterUri to get the ID
                  (let ((after-id
                         (if (string-match
                              "^org-id://\\(.+\\)$" after_uri)
                             (match-string 1 after_uri)
                           (org-mcp--tool-validation-error
                            "Field after_uri is not org-id://: %s"
                            after_uri))))
                    ;; Find the sibling with the specified ID
                    (org-back-to-heading t) ;; At parent
                    (let ((found nil)
                          (parent-end
                           (save-excursion
                             (org-end-of-subtree t t)
                             (point))))
                      ;; Search sibling in parent's subtree
                      ;; Move to first child
                      (if (org-goto-first-child)
                          (progn
                            ;; Now search among siblings
                            (while (and (not found)
                                        (< (point) parent-end))
                              (let ((current-id
                                     (org-entry-get nil "ID")))
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
                         after-id)))))
              ;; No after_uri - insert at end of parent's subtree
              (org-end-of-subtree t t)
              ;; If we're at the start of a sibling, go back one char
              ;; to be at the end of parent's content
              (when (looking-at "^\\*+ ")
                (backward-char 1))))

          ;; Validate body before inserting heading
          ;; Calculate the target level for validation
          (let ((target-level
                 (if (or parent-path parent-id)
                     ;; Child heading - parent level + 1
                     (1+ (or parent-level 0))
                   ;; Top-level heading
                   1)))

            ;; Validate body content if provided
            (when body
              (org-mcp--validate-body-no-headlines body target-level)
              (org-mcp--validate-body-no-unbalanced-blocks body)))

          ;; Insert the new heading using Org functions
          (if (or parent-path parent-id)
              ;; We're inside a parent
              (progn
                ;; Ensure we have a newline before inserting
                (unless (or (bobp) (looking-back "\n" 1))
                  (insert "\n"))
                (if (and after_uri (not (string-empty-p after_uri)))
                    ;; With after_uri, positioned after sibling
                    ;; Use org-insert-heading to insert right here
                    (progn
                      (org-insert-heading)
                      (insert title))
                  ;; No after_uri - at parent's end
                  ;; Need to create a child heading
                  (progn
                    ;; Ensure we have a newline before inserting
                    (unless (or (bobp) (looking-back "\n" 1))
                      (insert "\n"))
                    ;; Insert child heading at parent-level + 1
                    (insert (make-string (1+ parent-level) ?*) " " title))))
            ;; Top-level heading
            (progn
              ;; Check if there are no headlines yet
              ;; (empty buffer or only headers before us)
              (let ((has-headline
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "^\\*+ " nil t))))
                (if (not has-headline)
                    (progn
                      (unless (or (bobp) (looking-back "\n" 1))
                        (insert "\n"))
                      (insert "* "))
                  (progn
                    ;; Has headlines - use org-insert-heading
                    ;; Ensure proper spacing before inserting
                    (unless (or (bobp) (looking-back "\n" 1))
                      (insert "\n"))
                    (org-insert-heading nil nil t)))
                (insert title))))
          ;; Set the TODO state using Org functions
          (org-todo todo_state)

          ;; Set tags using Org functions
          (when tag-list
            (org-set-tags tag-list))

          ;; Add body if provided
          (when body
            (end-of-line)
            (insert "\n" body)
            (unless (string-suffix-p "\n" body)
              (insert "\n")))

          ;; Move back to the heading for org-id-get-create
          ;; org-id-get-create requires point to be on a heading
          (when body
            (org-back-to-heading t)))))))

(defun org-mcp--tool-rename-headline (uri current_title new_title)
  "Rename headline title, preserve TODO state and tags.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the renamed headline.
URI is the URI of the headline to rename.
CURRENT_TITLE is the current title (without TODO/tags) for validation.
NEW_TITLE is the new title to set (without TODO/tags).

MCP Parameters:
  uri - URI of the headline (org-headline://{absolute-path}#{headline-path}
        or org-id://{id})
  current_title - Current title without TODO state or tags
  new_title - New title without TODO state or tags"
  (org-mcp--validate-headline-title new_title)

  ;; Parse the resource URI
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Check for unsaved changes
    (org-mcp--check-buffer-modifications file-path "rename")

    ;; Rename the headline in the file
    (org-mcp--with-org-file-buffer file-path
        `((previous_title . ,current_title) (new_title . ,new_title))
      ;; Navigate to the headline
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--org-id-prefix uri))

      ;; Verify current title matches
      (beginning-of-line)
      (let ((actual-title (org-get-heading t t t t)))
        (unless (string= actual-title current_title)
          (org-mcp--state-mismatch-error
           current_title actual-title "Title")))

      (org-edit-headline new_title))))

(defun org-mcp--tool-edit-body
    (resource_uri old_body new_body replace_all)
  "Edit body content of an Org node using partial string replacement.
RESOURCE_URI is the URI of the node to edit.
OLD_BODY is the substring to search for within the node's body.
         Use empty string \"\" to add content to an empty node.
NEW_BODY is the replacement text.
REPLACE_ALL if non-nil, replace all occurrences.

MCP Parameters:
  resource_uri - URI of the node (org-headline://{absolute-path}#{headline-path}
                 or org-id://{id})
  old_body - Substring to replace within the body (must be unique
            unless replace_all).  Use \"\" to add to empty nodes
  new_body - Replacement text
  replace_all - Replace all occurrences (optional, default false)

Special behavior:
  When old_body is an empty string (\"\"), the tool will only work if
  the node has no body content, allowing you to add initial content
  to empty nodes."
  ;; Normalize JSON false to nil for proper boolean handling
  ;; JSON false can arrive as :false (keyword) or "false" (string)
  (let ((replace_all
         (cond
          ((eq replace_all :false)
           nil)
          ((equal replace_all "false")
           nil)
          (t
           replace_all))))
    ;; Check for unbalanced blocks in new_body
    (org-mcp--validate-body-no-unbalanced-blocks new_body)

    ;; Parse the resource URI
    (let* ((parsed (org-mcp--parse-resource-uri resource_uri))
           (file-path (car parsed))
           (headline-path (cdr parsed)))

      ;; Check for unsaved changes
      (org-mcp--check-buffer-modifications file-path "edit body")

      ;; Process the file
      (org-mcp--with-org-file-buffer file-path nil
        ;; Navigate to the headline
        (org-mcp--goto-headline-from-uri
         headline-path
         (string-prefix-p org-mcp--org-id-prefix resource_uri))

        ;; Validate headlines in newBody based on current level
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
          ;; (org-end-of-meta-data includes it)
          (when (and (> (length body-content) 0)
                     (= (aref body-content 0) ?\n))
            (setq body-content (substring body-content 1))
            (setq body-begin (1+ body-begin)))

          ;; Check if body is empty
          (when (string-match-p "\\`[[:space:]]*\\'" body-content)
            ;; Empty oldBody + empty body -> add content
            (if (string= old_body "")
                (setq occurrence-count 1) ; Treat as single replacement
              (org-mcp--tool-validation-error
               "Node has no body content")))

          ;; Count occurrences (unless already handled above)
          (unless (= occurrence-count 1) ; Skip if already set above
            ;; Empty oldBody with non-empty body is an error
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
             (concat "Text appears %d times (use replace_all)")
             occurrence-count)))

          ;; Perform replacement
          (let ((new-body-content
                 (cond
                  ;; Special case: empty oldBody with empty body
                  ((and (string= old_body "")
                        (string-match-p
                         "\\`[[:space:]]*\\'" body-content))
                   new_body)
                  ;; Normal replacement with replaceAll
                  (replace_all
                   (replace-regexp-in-string
                    (regexp-quote old_body) new_body body-content
                    t t))
                  ;; Normal single replacement
                  (t
                   (let ((pos
                          (string-match
                           (regexp-quote old_body) body-content)))
                     (if pos
                         (concat
                          (substring body-content 0 pos) new_body
                          (substring body-content
                                     (+ pos (length old_body))))
                       body-content))))))

            ;; Replace the body content
            (if (< body-begin body-end)
                (delete-region body-begin body-end)
              ;; Empty body - ensure we're at the right position
              (goto-char body-begin))
            (insert new-body-content)))))))

(defun org-mcp--check-refile-cycle (source-pos target-pos)
  "Check if refiling would create a cycle.
SOURCE-POS is the position of the headline to refile.
TARGET-POS is the position of the target parent.
Signals an error if TARGET-POS is within SOURCE-POS's subtree."
  (save-excursion
    (goto-char source-pos)
    (org-back-to-heading t)
    (let ((source-begin (point)))
      (org-end-of-subtree t t)
      (when (and (> target-pos source-begin)
                 (< target-pos (point)))
        (org-mcp--tool-validation-error
         "Cannot refile headline under its own descendant")))))

(defun org-mcp--tool-refile-headline (source_uri target_parent_uri)
  "Refile a headline to become a child of a different parent.
Creates an Org ID for the source headline if one doesn't exist.
Returns the ID-based URI for the refiled headline.
SOURCE_URI is the URI of the headline to move.
TARGET_PARENT_URI is the URI of the new parent headline.

MCP Parameters:
  source_uri - URI of the headline to move (org-headline://{path}
               or org-id://{id})
  target_parent_uri - URI of the new parent headline"
  ;; Parse both URIs
  (let* ((source-parsed (org-mcp--parse-resource-uri source_uri))
         (source-file (car source-parsed))
         (source-path (cdr source-parsed))
         (target-parsed (org-mcp--parse-resource-uri target_parent_uri))
         (target-file (car target-parsed))
         (target-path (cdr target-parsed)))

    ;; Check for unsaved changes in both files
    (org-mcp--check-buffer-modifications source-file "refile")
    (unless (string= source-file target-file)
      (org-mcp--check-buffer-modifications target-file "refile"))

    ;; Setup: Open source buffer and get/create ID
    (with-temp-buffer
      (set-visited-file-name source-file t)
      (insert-file-contents source-file)
      (org-mode)
      (goto-char (point-min))
      (org-mcp--goto-headline-from-uri
       source-path
       (string-prefix-p org-mcp--org-id-prefix source_uri))
      (let ((source-id (org-id-get-create)))
        ;; Save source file with ID (may be new)
        (write-region (point-min) (point-max) source-file)
        (org-mcp--refresh-file-buffers source-file)

        ;; Handle same-file and cross-file refiling differently
        (if (string= source-file target-file)
            ;; Same-file refile: reuse current buffer
            (let ((source-pos (point)))
              ;; Navigate to target and build RFLOC
              (goto-char (point-min))
              (org-mcp--goto-headline-from-uri
               target-path
               (string-prefix-p org-mcp--org-id-prefix target_parent_uri))
              (let ((target-rfloc (list (org-get-heading t t t t)
                                        source-file
                                        nil
                                        (point)))
                    (target-pos (point)))

                (org-mcp--check-refile-cycle source-pos target-pos)

                ;; Perform the refile
                (goto-char source-pos)
                (org-refile nil nil target-rfloc)

                ;; Save and return
                (write-region (point-min) (point-max) source-file)
                (org-mcp--refresh-file-buffers source-file)
                (json-encode
                 `((success . t)
                   (uri . ,(concat org-mcp--org-id-prefix source-id))))))

          ;; Cross-file refile: use separate buffer for target
          (let (target-rfloc)
            ;; Get target RFLOC in target file
            (with-temp-buffer
              (set-visited-file-name target-file t)
              (insert-file-contents target-file)
              (org-mode)
              (goto-char (point-min))
              (org-mcp--goto-headline-from-uri
               target-path
               (string-prefix-p org-mcp--org-id-prefix target_parent_uri))
              (setq target-rfloc (list (org-get-heading t t t t)
                                       target-file
                                       nil
                                       (point))))

            ;; Perform refile in current (source) buffer
            (goto-char (point-min))
            (org-mcp--goto-headline-from-uri
             source-path
             (string-prefix-p org-mcp--org-id-prefix source_uri))
            (org-refile nil nil target-rfloc)
            ;; Save source file after refile
            (write-region (point-min) (point-max) source-file)
            (org-mcp--refresh-file-buffers source-file)

            ;; Save target file if it's already open
            (let ((target-buffer (find-buffer-visiting target-file)))
              (when target-buffer
                (with-current-buffer target-buffer
                  (save-buffer))))

            ;; Return success
            (json-encode
             `((success . t)
               (uri . ,(concat org-mcp--org-id-prefix source-id))))))))))

;;; Resource template workaround tools

(defun org-mcp--tool-read-file (file)
  "Tool wrapper for org://{filename} resource template.

MCP Parameters:
  file - Absolute path to an Org file"
  (org-mcp--handle-file-resource `(("filename" . ,file))))

(defun org-mcp--tool-read-outline (file)
  "Tool wrapper for org-outline://{filename} resource template.

MCP Parameters:
  file - Absolute path to an Org file"
  (org-mcp--handle-outline-resource `(("filename" . ,file))))

(defun org-mcp--tool-read-headline (file headline_path)
  "Tool wrapper for org-headline://{filename}#{path} resource template.

MCP Parameters:
  file - Absolute path to an Org file
  headline_path - Non-empty slash-separated path to headline (string)
                  Only slashes in headline titles must be encoded as %2F.
                  Example: \"Project/Planning\" for nested headlines
                  Example: \"A%2FB Testing\" for headline titled \"A/B Testing\"
                  To read entire files, use org-read-file instead"
  (unless (stringp headline_path)
    (org-mcp--tool-validation-error
     "headline_path must be a string, got: %S (type: %s)" headline_path (type-of headline_path)))
  (when (string-empty-p headline_path)
    (org-mcp--tool-validation-error
     "headline_path must be a non-empty string. Use the org-read-file tool to read entire files."))
  (let ((full-path (concat file "#" headline_path)))
    (org-mcp--handle-headline-resource `(("filename" . ,full-path)))))

(defun org-mcp--tool-read-by-id (uuid)
  "Tool wrapper for org-id://{uuid} resource template.

MCP Parameters:
  uuid - UUID from headline's ID property"
  (org-mcp--handle-id-resource `(("uuid" . ,uuid))))

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-todo-config
   :id "org-get-todo-config"
   :description
   "Get the TODO keyword configuration from the current Emacs Org-mode \
settings.  Returns information about task state sequences and their \
semantics.

Parameters: None

Returns JSON object with two arrays:
  sequences - Array of TODO keyword sequences, each containing:
    - type: Sequence type (e.g., \"sequence\", \"type\")
    - keywords: Array of keywords including \"|\" separator between \
active and done states
  semantics - Array of keyword semantics, each containing:
    - state: The TODO keyword (e.g., \"TODO\", \"DONE\")
    - isFinal: Whether this is a final (done) state (boolean)
    - sequenceType: The sequence type this keyword belongs to

The \"|\" separator in sequences marks the boundary between active \
states (before) and done states (after).  If no \"|\" is present, the \
last keyword is treated as the done state.

Use this tool to understand the available task states in the Org \
configuration before creating or updating TODO items."
   :read-only t
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-tag-config
   :id "org-get-tag-config"
   :description
   "Get tag-related configuration from the current Emacs Org-mode \
settings.  Returns literal Elisp variable values as strings for tag \
configuration introspection.

Parameters: None

Returns JSON object with literal Elisp expressions (as strings) for:
  org-use-tag-inheritance - Controls tag inheritance behavior
  org-tags-exclude-from-inheritance - Tags that don't inherit
  org-tag-alist - List of allowed tags with optional key bindings and \
groups
  org-tag-persistent-alist - Additional persistent tags (or nil)

The org-tag-alist format includes:
  - Simple tags: (\"tagname\" . key-char)
  - Group markers: :startgroup, :endgroup for mutually exclusive tags
  - Grouptags: :startgrouptag, :grouptags, :endgrouptag for tag \
hierarchies

Use this tool to understand:
  - Which tags are allowed
  - Tag inheritance rules
  - Mutually exclusive tag groups
  - Tag hierarchy relationships

This helps validate tag usage and understand tag semantics before \
adding or modifying tags on TODO items."
   :read-only t
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-allowed-files
   :id "org-get-allowed-files"
   :description
   "Get the list of Org files accessible through the org-mcp server.  \
Returns the configured allowed files exactly as specified in \
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
  - URI Construction: I need to build an org-headline:// URI - what's \
the exact path?
  - Access Troubleshooting: Why is my file access failing?
  - Configuration Verification: Did my org-mcp-allowed-files setting \
work correctly?

Behavior:
  - Returns configured paths exactly as-is (no validation or normalization)
  - Always succeeds (returns empty array if no files configured)
  - No metadata included (only file paths)"
   :read-only t
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-update-todo-state
   :id "org-update-todo-state"
   :description
   "Update the TODO state of an Org headline.  Changes the task state \
while preserving the headline title, tags, and other properties.  \
Creates an Org ID property for the headline if one doesn't exist.

Parameters:
  uri - URI of the headline to update (string, required)
        Formats: org-headline://{absolute-path}#{url-encoded-path}
                 org-id://{uuid}
  current_state - Expected current TODO state (string, required)
                  Use empty string \"\" if headline has no TODO state
                  Must match actual state or tool will error
  new_state - New TODO state to set (string, required)
              Must be valid keyword from org-todo-keywords

Returns JSON object:
  success - Always true on success (boolean)
  previous_state - The previous TODO state (string, empty for none)
  new_state - The new TODO state that was set (string)
  uri - ID-based URI (org-id://{uuid}) for the updated headline

Error cases:
  - State mismatch: current_state doesn't match actual headline state
  - Invalid TODO state: new_state not in org-todo-keywords
  - File access denied: Referenced file not in org-mcp-allowed-files
  - Headline not found: URI path doesn't resolve to a headline
  - Unsaved changes: File has unsaved modifications in a buffer

Security: Only files in org-mcp-allowed-files can be modified.  The \
tool validates the current state to prevent race conditions."
   :read-only nil
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-add-todo
   :id "org-add-todo"
   :description
   "Add a new TODO item to an Org file at a specified location.  \
Creates the headline with TODO state, tags, and optional body content.  \
Automatically creates an Org ID property for the new headline.

Parameters:
  title - Headline text without TODO state or tags (string, required)
          Cannot be empty or whitespace-only
          Cannot contain newlines
  todo_state - TODO keyword from org-todo-keywords (string, required)
  tags - Tags for the headline (string or array, required)
         Single tag: \"urgent\"
         Multiple tags: [\"work\", \"urgent\"]
         Validated against org-tag-alist if configured
         Must follow Org tag rules (alphanumeric, _, @)
         Respects mutually exclusive tag groups
  body - Body content under the headline (string, optional)
         Cannot contain headlines at same or higher level as new item
         Must have balanced #+BEGIN/#+END blocks
  parent_uri - Parent location (string, required)
               For top-level: org-headline://{absolute-path}
               For child: org-headline://{path}#{parent-path}
                         or org-id://{parent-uuid}
  after_uri - Sibling to insert after (string, optional)
              Must be org-id://{uuid} format
              If omitted, appends as last child of parent

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the new headline
  file - Filename (not full path) where item was added
  title - The headline title that was created

Error cases:
  - Invalid title: Empty, whitespace-only, or contains newlines
  - Invalid TODO state: Not in org-todo-keywords
  - Invalid tags: Not in org-tag-alist, invalid characters, or \
violates mutex groups
  - Invalid body: Contains conflicting headlines or unbalanced blocks
  - File access denied: Referenced file not in org-mcp-allowed-files
  - Parent not found: parent_uri doesn't resolve to a headline
  - Sibling not found: after_uri doesn't match a child of parent
  - Unsaved changes: File has unsaved modifications in a buffer

Positioning behavior:
  - With parent_uri only: Appends as last child of parent
  - With parent_uri + after_uri: Inserts immediately after specified \
sibling
  - Top-level (parent_uri with no fragment): Adds at end of file

Security: Only files in org-mcp-allowed-files can be modified."
   :read-only nil
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-rename-headline
   :id "org-rename-headline"
   :description
   "Rename an Org headline's title while preserving its TODO state, \
tags, properties, and body content.  Creates an Org ID property for the \
headline if one doesn't exist.

Parameters:
  uri - URI of the headline to rename (string, required)
        Formats: org-headline://{absolute-path}#{url-encoded-path}
                 org-id://{uuid}
  current_title - Expected current title without TODO/tags (string, \
required)
                  Must match actual title or tool will error
                  Used to prevent race conditions
  new_title - New title without TODO state or tags (string, required)
              Cannot be empty or whitespace-only
              Cannot contain newlines

Returns JSON object:
  success - Always true on success (boolean)
  previous_title - The previous headline title (string)
  new_title - The new title that was set (string)
  uri - ID-based URI (org-id://{uuid}) for the renamed headline

Error cases:
  - Title mismatch: current_title doesn't match actual headline title
  - Invalid new title: Empty, whitespace-only, or contains newlines
  - File access denied: Referenced file not in org-mcp-allowed-files
  - Headline not found: URI path doesn't resolve to a headline
  - Unsaved changes: File has unsaved modifications in a buffer

Preservation guarantees:
  - TODO state remains unchanged
  - All tags remain unchanged
  - All properties (including custom IDs) remain unchanged
  - Body content and child headlines remain unchanged

Security: Only files in org-mcp-allowed-files can be modified.  The \
tool validates the current title to prevent race conditions."
   :read-only nil
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-edit-body
   :id "org-edit-body"
   :description
   "Edit the body content of an Org headline using partial string \
replacement.  Finds and replaces a substring within the headline's body \
text.  Creates an Org ID property for the headline if one doesn't exist.

Parameters:
  resource_uri - URI of the headline to edit (string, required)
                 Formats: org-headline://{absolute-path}#{url-encoded-path}
                          org-id://{uuid}
  old_body - Substring to find and replace (string, required)
             Must appear exactly once unless replace_all is true
             Use empty string \"\" only for adding to empty nodes
  new_body - Replacement text (string, required)
             Cannot introduce headlines at same or higher level
             Must maintain balanced #+BEGIN/#+END blocks
  replace_all - Replace all occurrences (boolean, optional, default \
false)
                When false, old_body must be unique in the body

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the edited headline

Special behavior - Empty old_body:
  When old_body is \"\", the tool adds content to empty nodes:
  - Only works if node body is empty or whitespace-only
  - Error if node already has content
  - Useful for adding initial content to newly created headlines

Error cases:
  - Text not found: old_body doesn't appear in the body
  - Multiple occurrences: old_body appears multiple times without \
replace_all
  - Empty body conflict: old_body is \"\" but node has content
  - Invalid new content: new_body contains headlines at conflicting levels
  - Unbalanced blocks: new_body has unclosed #+BEGIN or orphaned #+END
  - File access denied: Referenced file not in org-mcp-allowed-files
  - Headline not found: resource_uri doesn't resolve to a headline
  - Unsaved changes: File has unsaved modifications in a buffer

Content validation:
  - Prevents introducing headlines that would break document structure
  - Ensures all #+BEGIN_* blocks have matching #+END_* markers
  - Block validation handles nested literal content correctly

Security: Only files in org-mcp-allowed-files can be modified."
   :read-only nil
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-refile-headline
   :id "org-refile-headline"
   :description
   "Move a headline and its entire subtree to become a child of a \
different parent headline.  Uses Org's built-in org-refile function.  \
Creates an Org ID property for the source headline if one doesn't exist.

Parameters:
  source_uri - URI of the headline to move (string, required)
               Formats: org-headline://{absolute-path}#{url-encoded-path}
                        org-id://{uuid}
  target_parent_uri - URI of the new parent headline (string, required)
                      Formats: org-headline://{absolute-path}#{url-encoded-path}
                               org-id://{uuid}

Returns JSON object:
  success - Always true on success (boolean)
  uri - ID-based URI (org-id://{uuid}) for the refiled headline

Behavior:
  - Moves source headline to become the last child of target parent
  - Automatically adjusts heading level to parent level + 1
  - Preserves all TODO state, tags, properties, and body content
  - Works across different files
  - Source headline is removed from its original location
  - Both source and target files are saved after the operation

Security: Only files in org-mcp-allowed-files can be modified."
   :read-only nil
   :server-id org-mcp--server-id)
  ;; Workaround tools for resource templates (until Claude Code supports templates)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-file
   :id "org-read-file"
   :description
   "Read complete raw content of an Org file. Returns entire file as \
plain text with all formatting, properties, and structure preserved. \
File must be in org-mcp-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)

Returns: Plain text content of the entire Org file"
   :read-only t
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-outline
   :id "org-read-outline"
   :description
   "Get hierarchical structure of Org file as JSON outline. Returns all \
headline titles and nesting relationships at full depth. File must be \
in org-mcp-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)

Returns: JSON object with hierarchical outline structure"
   :read-only t
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-headline
   :id "org-read-headline"
   :description
   "Read specific Org headline by hierarchical path. Returns headline with \
TODO state, tags, properties, body text, and all nested subheadings. File \
must be in org-mcp-allowed-files.

Parameters:
  file - Absolute path to Org file (string, required)
  headline_path - Non-empty slash-separated path to headline (string, required)
                  Only slashes (/) in headline titles must be encoded as %2F
                  Example: \"Project/Planning\" for nested headlines
                  Example: \"A%2FB Testing\" for headline titled \"A/B Testing\"
                  To read entire files, use org-read-file instead

Returns: Plain text content of the headline and its subtree"
   :read-only t
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-read-by-id
   :id "org-read-by-id"
   :description
   "Read Org headline by its unique ID property. More stable than path-based \
access since IDs don't change when headlines are renamed or moved. File \
containing the ID must be in org-mcp-allowed-files.

Parameters:
  uuid - UUID from headline's ID property (string, required)

Returns: Plain text content of the headline and its subtree"
   :read-only t
   :server-id org-mcp--server-id)
  ;; Register template resources for org files
  (mcp-server-lib-register-resource
   "org://{filename}" #'org-mcp--handle-file-resource
   :name "Org file"
   :description
   "Access the complete raw content of an Org file.  Returns the entire \
file as plain text, preserving all formatting, properties, and structure.

URI format: org://{filename}
  filename - Absolute path to the Org file (required)

Security and access:
  - File must be in org-mcp-allowed-files list
  - Only absolute paths are accepted
  - Path validation includes symlink resolution

Returns: Plain text content of the entire Org file

Example URIs:
  org:///home/user/notes/tasks.org
  org:///Users/name/Documents/projects.org

Use this resource to:
  - Read complete Org file contents
  - Access files for processing or analysis
  - Get raw Org markup including all metadata

Error cases:
  - File access denied: File not in org-mcp-allowed-files
  - Invalid path: Relative paths not accepted
  - File not found: Path doesn't exist"
   :mime-type "text/plain"
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-resource
   "org-outline://{filename}" #'org-mcp--handle-outline-resource
   :name "Org file outline"
   :description
   "Get the hierarchical structure of an Org file as a JSON outline.  \
Extracts headline titles and their nesting relationships up to 2 levels \
deep.

URI format: org-outline://{filename}
  filename - Absolute path to the Org file (required)

Security and access:
  - File must be in org-mcp-allowed-files list
  - Only absolute paths are accepted

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
  - Build navigation interfaces
  - Understand file organization without reading full content

Error cases:
  - File access denied: File not in org-mcp-allowed-files
  - Invalid path: Relative paths not accepted
  - File not found: Path doesn't exist"
   :mime-type "application/json"
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-resource
   (concat org-mcp--org-headline-prefix "{filename}")
   #'org-mcp--handle-headline-resource
   :name "Org headline content"
   :description
   "Access content of a specific Org headline by its path in the file \
hierarchy.  Returns the headline and all its subheadings as plain text.

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

Security and access:
  - File must be in org-mcp-allowed-files list
  - Only absolute file paths are accepted

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
  - Get complete subtree including all children

Error cases:
  - File access denied: File not in org-mcp-allowed-files
  - Invalid path: Relative file paths not accepted
  - Headline not found: Path doesn't match any headline hierarchy
  - File not found: Path doesn't exist"
   :mime-type "text/plain"
   :server-id org-mcp--server-id)
  (mcp-server-lib-register-resource
   (concat org-mcp--org-id-prefix "{uuid}")
   #'org-mcp--handle-id-resource
   :name "Org node by ID"
   :description
   "Access content of an Org headline by its unique ID property.  More \
stable than path-based access since IDs don't change when headlines are \
renamed or moved.

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
  - Build cross-references between Org nodes

Advantages over path-based access:
  - Survives headline renames
  - Survives headline moves within file
  - Survives file moves (if org-id database is updated)

Error cases:
  - ID not found: No headline with that ID exists in allowed files
  - File access denied: File with ID is not in org-mcp-allowed-files
  - File not found: org-id database points to non-existent file"
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
  (mcp-server-lib-unregister-tool
   "org-refile-headline" org-mcp--server-id)
  ;; Unregister workaround tools
  (mcp-server-lib-unregister-tool "org-read-file" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-read-outline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-read-headline" org-mcp--server-id)
  (mcp-server-lib-unregister-tool "org-read-by-id" org-mcp--server-id)
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource
   "org://{filename}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   "org-outline://{filename}" org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   (concat
    org-mcp--org-headline-prefix "{filename}")
   org-mcp--server-id)
  (mcp-server-lib-unregister-resource
   (concat org-mcp--org-id-prefix "{uuid}") org-mcp--server-id))

(provide 'org-mcp)
;;; org-mcp.el ends here
