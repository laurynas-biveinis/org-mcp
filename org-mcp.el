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
           (mapconcat 'identity headline-path "/"))))

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

(defun org-mcp--tool-file-access-error (file-path)
  "Throw file access error for tool operations.
FILE-PATH is the file that cannot be accessed."
  (mcp-server-lib-tool-throw
   (format "File not in allowed list: %s" file-path)))

(defun org-mcp--resource-file-access-error (file-path)
  "Signal file access error for resource operations.
FILE-PATH is the file that cannot be accessed."
  (mcp-server-lib-resource-signal-error
   mcp-server-lib-jsonrpc-error-invalid-params
   (format "File not in allowed list: %s" file-path)))

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
              (push `((state . ,kw)
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
     (org-tags-sort-function
      . ,(prin1-to-string org-tags-sort-function))
     (org-tag-alist . ,(prin1-to-string org-tag-alist))
     (org-tag-persistent-alist
      . ,(prin1-to-string org-tag-persistent-alist)))))

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
        (org-mcp--tool-file-access-error id-file))
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
             'url-unhex-string
             (split-string headline-path-str "/")))))
    (if headline-path
        (let ((content
               (org-mcp--get-headline-content
                (expand-file-name allowed-file) headline-path)))
          (unless content
            (org-mcp--resource-not-found-error
             "headline" (mapconcat 'identity headline-path "/")))
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
        (org-mcp--resource-file-access-error file-path))
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

(defun org-mcp--tool-update-todo-state (uri currentState newState)
  "Update the TODO state of a headline.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the updated headline.
URI is the URI of the headline to update.
CURRENTSTATE is the current TODO state (empty string for no state).
NEWSTATE is the new TODO state to set.

MCP Parameters:
  uri - URI of the headline (org-headline:// or org-id://)
  currentState - Current TODO state (empty string for no state)
  newState - New TODO state (must be in `org-todo-keywords')"
  ;; Parse the resource URI
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Validate new state is in org-todo-keywords
    (let ((valid-states
           (apply 'append (mapcar 'cdr org-todo-keywords))))
      (unless (member newState valid-states)
        (org-mcp--tool-validation-error
         "Invalid TODO state: '%s'.  Valid states: %s"
         newState (mapconcat 'identity valid-states ", "))))

    ;; Check for unsaved changes
    (org-mcp--check-buffer-modifications file-path "update")

    ;; Update the TODO state in the file
    (org-mcp--with-org-file-buffer file-path
        `((previousState . ,(or currentState ""))
          (newState . ,newState))
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--org-id-prefix uri))

      ;; Check current state matches
      (beginning-of-line)
      (let ((actual-state (org-get-todo-state)))
        (unless (string= actual-state currentState)
          (org-mcp--state-mismatch-error
           (or currentState "(no state)")
           (or actual-state "(no state)") "State")))

      ;; Update the state
      (org-todo newState))))

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
            (string-match-p "^[[:space:]]*$" title))
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
      ;; After scanning everything, check if we're still in a block
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
    (title todoState tags body parentUri afterUri)
  "Add a new TODO item to an Org file.
Creates an Org ID for the new headline and returns its ID-based URI.
TITLE is the headline text.
TODOSTATE is the TODO state from `org-todo-keywords'.
TAGS is a single tag string or list of tag strings.
BODY is optional body text.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after.

MCP Parameters:
  title - The headline text
  todoState - TODO state from `org-todo-keywords'
  tags - Tags to add (single string or array of strings)
  body - Optional body text content
  parentUri - Parent item URI (required)
  afterUri - Sibling to insert after (optional)"
  (org-mcp--validate-headline-title title)

  ;; Normalize tags and get valid TODO states
  (let ((tag-list (org-mcp--normalize-tags-to-list tags))
        (valid-states
         (apply 'append (mapcar 'cdr org-todo-keywords))))

    ;; Validate TODO state
    (unless (member todoState valid-states)
      (org-mcp--tool-validation-error
       "Invalid TODO state: '%s'.  Valid states: %s"
       todoState (mapconcat 'identity valid-states ", ")))

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
          parentUri
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
              (parent-id nil))
          ;; Parse parent URI (org-headline:// or org-id://)
          (org-mcp--with-uri-prefix-dispatch
              parentUri
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
              (org-mcp--goto-headline-from-uri
               (or (and parent-id (list parent-id))
                   parent-path)
               parent-id)
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
            ;; Handle afterUri positioning
            (if afterUri
                (progn
                  ;; Parse afterUri to get the ID
                  (let ((after-id
                         (if (string-match
                              "^org-id://\\(.+\\)$" afterUri)
                             (match-string 1 afterUri)
                           (org-mcp--tool-validation-error
                            "AfterUri must be org-id://, got: %s"
                            afterUri))))
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
              ;; No afterUri - insert at end of parent's subtree
              (org-end-of-subtree t t)))

          ;; Insert the new heading using Org functions
          (if (or parent-path parent-id)
              ;; We're inside a parent
              (progn
                ;; Ensure we have a newline before inserting
                (unless (or (bobp) (looking-back "\n" 1))
                  (insert "\n"))
                (if afterUri
                    ;; With afterUri, positioned after sibling
                    ;; Use org-insert-heading to insert right here
                    (progn
                      (org-insert-heading)
                      (insert title))
                  ;; No afterUri - at parent's end
                  ;; Need to create a child heading
                  (progn
                    ;; Ensure blank line before child
                    (unless (or (bobp) (looking-back "\n\n" 2))
                      (insert "\n"))
                    ;; Create child with subheading
                    (org-insert-subheading nil)
                    (insert title))))
            ;; Top-level heading
            (progn
              ;; Ensure proper spacing before inserting
              (unless (or (bobp) (looking-back "\n" 1))
                (insert "\n"))
              ;; Use org-insert-heading for top-level
              (org-insert-heading nil nil t)
              (insert title)))

          ;; Set the TODO state using Org functions
          (org-todo todoState)

          ;; Set tags using Org functions
          (when tag-list
            (org-set-tags tag-list))

          ;; Add body if provided
          (when body
            (org-mcp--validate-body-no-headlines
             body (org-current-level))
            (org-mcp--validate-body-no-unbalanced-blocks body)
            (end-of-line)
            (insert "\n" body)
            (unless (string-suffix-p "\n" body)
              (insert "\n"))))))))

(defun org-mcp--tool-rename-headline (uri currentTitle newTitle)
  "Rename headline title, preserve TODO state and tags.
Creates an Org ID for the headline if one doesn't exist.
Returns the ID-based URI for the renamed headline.
URI is the URI of the headline to rename.
CURRENTTITLE is the current title (without TODO/tags) for validation.
NEWTITLE is the new title to set (without TODO/tags).

MCP Parameters:
  uri - URI of the headline (org-headline:// or org-id://)
  currentTitle - Current title without TODO state or tags
  newTitle - New title without TODO state or tags"
  (org-mcp--validate-headline-title newTitle)

  ;; Parse the resource URI
  (let* ((parsed (org-mcp--parse-resource-uri uri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Check for unsaved changes
    (org-mcp--check-buffer-modifications file-path "rename")

    ;; Rename the headline in the file
    (org-mcp--with-org-file-buffer file-path
        `((previousTitle . ,currentTitle) (newTitle . ,newTitle))
      ;; Navigate to the headline
      (org-mcp--goto-headline-from-uri
       headline-path (string-prefix-p org-mcp--org-id-prefix uri))

      ;; Verify current title matches
      (beginning-of-line)
      (let ((actual-title (org-get-heading t t t t)))
        (unless (string= actual-title currentTitle)
          (org-mcp--state-mismatch-error
           currentTitle actual-title "Title")))

      (org-edit-headline newTitle))))

(defun org-mcp--tool-edit-body
    (resourceUri oldBody newBody replaceAll)
  "Edit body content of an Org node using partial string replacement.
RESOURCEURI is the URI of the node to edit.
OLDBODY is the substring to search for within the node's body.
         Use empty string \"\" to add content to an empty node.
NEWBODY is the replacement text.
REPLACEALL if non-nil, replace all occurrences.

MCP Parameters:
  resourceUri - URI of the node (org-headline:// or org-id://)
  oldBody - Substring to replace within the body (must be unique
            unless replaceAll).  Use \"\" to add to empty nodes
  newBody - Replacement text
  replaceAll - Replace all occurrences (optional, default false)

Special behavior:
  When oldBody is an empty string (\"\"), the tool will only work if
  the node has no body content, allowing you to add initial content
  to empty nodes."
  ;; Check for unbalanced blocks in newBody
  (org-mcp--validate-body-no-unbalanced-blocks newBody)

  ;; Parse the resource URI
  (let* ((parsed (org-mcp--parse-resource-uri resourceUri))
         (file-path (car parsed))
         (headline-path (cdr parsed)))

    ;; Check for unsaved changes
    (org-mcp--check-buffer-modifications file-path "edit body")

    ;; Process the file
    (org-mcp--with-org-file-buffer file-path nil
      ;; Navigate to the headline
      (org-mcp--goto-headline-from-uri
       headline-path
       (string-prefix-p org-mcp--org-id-prefix resourceUri))

      ;; Validate headlines in newBody based on current level
      (org-mcp--validate-body-no-headlines
       newBody (org-current-level))

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
          (if (string= oldBody "")
              (setq occurrence-count 1) ; Treat as single replacement
            (org-mcp--tool-validation-error
             "Node has no body content")))

        ;; Count occurrences (unless already handled above)
        (unless (= occurrence-count 1) ; Skip if already set above
          ;; Empty oldBody with non-empty body is an error
          (if (and (string= oldBody "")
                   (not
                    (string-match-p
                     "\\`[[:space:]]*\\'" body-content)))
              (org-mcp--tool-validation-error
               "Cannot use empty oldBody with non-empty body content")
            ;; Normal occurrence counting
            (let ((case-fold-search nil)
                  (search-pos 0))
              (while (string-match (regexp-quote oldBody) body-content
                                   search-pos)
                (setq occurrence-count (1+ occurrence-count))
                (setq search-pos (match-end 0))))))

        ;; Validate occurrences
        (cond
         ((= occurrence-count 0)
          (org-mcp--tool-validation-error "Body text not found: %s"
                                          oldBody))
         ((and (> occurrence-count 1) (not replaceAll))
          (org-mcp--tool-validation-error
           (concat "Text appears %d times (use replaceAll)")
           occurrence-count)))

        ;; Perform replacement
        (let ((new-body-content
               (cond
                ;; Special case: empty oldBody with empty body
                ((and (string= oldBody "")
                      (string-match-p
                       "\\`[[:space:]]*\\'" body-content))
                 newBody)
                ;; Normal replacement with replaceAll
                (replaceAll
                 (replace-regexp-in-string
                  (regexp-quote oldBody) newBody body-content
                  t t))
                ;; Normal single replacement
                (t
                 (let ((pos
                        (string-match
                         (regexp-quote oldBody) body-content)))
                   (if pos
                       (concat
                        (substring body-content 0 pos) newBody
                        (substring body-content
                                   (+ pos (length oldBody))))
                     body-content))))))

          ;; Replace the body content
          (if (< body-begin body-end)
              (delete-region body-begin body-end)
            ;; Empty body - ensure we're at the right position
            (goto-char body-begin))
          (insert new-body-content))))))

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-todo-config
   :id "org-get-todo-config"
   :description "Get TODO keyword configuration for task states"
   :read-only t)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-tag-config
   :id "org-get-tag-config"
   :description "Get tag configuration for tags and properties"
   :read-only t)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-update-todo-state
   :id "org-update-todo-state"
   :description "Update the TODO state of a headline"
   :read-only nil)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-add-todo
   :id "org-add-todo"
   :description "Add a new TODO item to an Org file"
   :read-only nil)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-rename-headline
   :id "org-rename-headline"
   :description "Rename the title of a headline"
   :read-only nil)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-edit-body
   :id "org-edit-body"
   :description "Edit body content using partial string replacement"
   :read-only nil)
  ;; Register template resources for org files
  (mcp-server-lib-register-resource
   "org://{filename}"
   #'org-mcp--handle-file-resource
   :name "Org file"
   :description "Raw Org file content"
   :mime-type "text/plain")
  (mcp-server-lib-register-resource
   "org-outline://{filename}"
   #'org-mcp--handle-outline-resource
   :name "Org file outline"
   :description "Hierarchical structure of an Org file"
   :mime-type "application/json")
  (mcp-server-lib-register-resource
   (concat org-mcp--org-headline-prefix "{filename}")
   #'org-mcp--handle-headline-resource
   :name "Org headline content"
   :description "Content of a specific Org headline by path"
   :mime-type "text/plain")
  (mcp-server-lib-register-resource
   (concat org-mcp--org-id-prefix "{uuid}")
   #'org-mcp--handle-id-resource
   :name "Org node by ID"
   :description "Content of an Org node by its ID"
   :mime-type "text/plain"))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-tool "org-get-todo-config")
  (mcp-server-lib-unregister-tool "org-get-tag-config")
  (mcp-server-lib-unregister-tool "org-update-todo-state")
  (mcp-server-lib-unregister-tool "org-add-todo")
  (mcp-server-lib-unregister-tool "org-rename-headline")
  (mcp-server-lib-unregister-tool "org-edit-body")
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource "org://{filename}")
  (mcp-server-lib-unregister-resource "org-outline://{filename}")
  (mcp-server-lib-unregister-resource
   (concat org-mcp--org-headline-prefix "{filename}"))
  (mcp-server-lib-unregister-resource
   (concat org-mcp--org-id-prefix "{uuid}")))

(provide 'org-mcp)
;;; org-mcp.el ends here
