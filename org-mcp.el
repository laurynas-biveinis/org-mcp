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
  "List of Org files that can be accessed via MCP.
Each element should be a file path (absolute or relative).
Relative paths are expanded relative to `default-directory'.
For security, only files in this list can be accessed by MCP clients."
  :type '(repeat file)
  :group 'org-mcp)

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

(defun org-mcp--file-matches-p (filename file)
  "Check if FILENAME matches FILE by basename or full path."
  (let ((basename (file-name-nondirectory file)))
    (or (string= filename file) (string= filename basename))))

(defun org-mcp--find-allowed-file (filename)
  "Find FILENAME in `org-mcp-allowed-files' and return the full path.
Returns nil if the file is not in the allowed list."
  (let ((allowed-file nil))
    (dolist (file org-mcp-allowed-files)
      (when (org-mcp--file-matches-p filename file)
        (setq allowed-file file)))
    allowed-file))

(defun org-mcp--validate-file-access (filename)
  "Validate that FILENAME is in the allowed list.
Returns the full path if allowed, signals an error otherwise."
  (let ((allowed-file (org-mcp--find-allowed-file filename)))
    (unless allowed-file
      (mcp-server-lib-resource-signal-error
       mcp-server-lib-jsonrpc-error-invalid-params
       (format "File not in allowed list: %s" filename)))
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

(defun org-mcp--handle-headline-resource (params)
  "Handler for org-headline://{filename} template.
PARAMS is an alist containing the filename parameter.
The filename parameter includes both file and headline path."
  (let* ((full-path (alist-get "filename" params nil nil #'string=)))
    ;; Split filename and headline path
    (if (string-match "^\\([^/]+\\)/\\(.+\\)$" full-path)
        (let* ((filename (match-string 1 full-path))
               (headline-path-str (match-string 2 full-path))
               (allowed-file (org-mcp--validate-file-access filename))
               ;; Parse the path (URL-encoded headline path)
               (decoded-path (url-unhex-string headline-path-str))
               (headline-path (split-string decoded-path "/"))
               (content
                (org-mcp--get-headline-content
                 (expand-file-name allowed-file) headline-path)))
          (unless content
            (mcp-server-lib-resource-signal-error
             mcp-server-lib-jsonrpc-error-invalid-params
             (format "Headline not found: %s"
                     (car (last headline-path)))))
          content)
      (mcp-server-lib-resource-signal-error
       mcp-server-lib-jsonrpc-error-invalid-params
       "Invalid headline resource format"))))

(defun org-mcp--get-headline-content (file-path headline-path)
  "Get content for headline at HEADLINE-PATH in FILE-PATH.
HEADLINE-PATH is a list of headline titles to traverse.
Returns the content string or nil if not found."
  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))
    ;; Find the headline by traversing the path
    (catch 'not-found
      (dolist (target-title headline-path)
        (let ((found nil))
          ;; Search for the headline at the current level
          (while (and (not found) (re-search-forward "^\\*+ " nil t))
            (let ((title (org-get-heading t t t t)))
              (when (string= title target-title)
                (setq found t))))
          (unless found
            ;; Headline not found, return nil
            (throw 'not-found nil))))
      ;; All parts of path found, extract content
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
      (mcp-server-lib-resource-signal-error
       mcp-server-lib-jsonrpc-error-invalid-params
       (format "ID not found: %s" id)))
    ;; Validate that the file is in allowed list
    (let ((allowed-file
           (org-mcp--find-allowed-file
            (file-name-nondirectory file-path))))
      (unless allowed-file
        (mcp-server-lib-resource-signal-error
         mcp-server-lib-jsonrpc-error-invalid-params
         (format "File not in allowed list: %s"
                 (file-name-nondirectory file-path))))
      ;; Get the content
      (org-mcp--get-content-by-id
       (expand-file-name allowed-file) id))))

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

(defun org-mcp--tool-update-todo-state
    (resourceUri currentState newState)
  "Update the TODO state of a headline.
RESOURCEURI is the URI of the headline to update.
CURRENTSTATE is the current TODO state (empty string for no state).
NEWSTATE is the new TODO state to set.

MCP Parameters:
  resourceUri - URI of the headline (org-headline:// or org-id://)
  currentState - Current TODO state (empty string for no state)
  newState - New TODO state (must be in `org-todo-keywords')"
  (let (file-path
        headline-path)
    ;; Parse the resource URI
    (cond
     ;; Handle org-headline:// URIs
     ((string-match
       "^org-headline://\\([^/]+\\)/\\(.+\\)$" resourceUri)
      (let* ((filename (match-string 1 resourceUri))
             (headline-path-str (match-string 2 resourceUri))
             (allowed-file (org-mcp--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))
        (setq headline-path
              (split-string (url-unhex-string headline-path-str)
                            "/"))))
     ;; Handle org-id:// URIs
     ((string-match "^org-id://\\(.+\\)$" resourceUri)
      (let* ((id (match-string 1 resourceUri))
             (id-file (org-id-find-id-file id)))
        (unless id-file
          (mcp-server-lib-tool-throw (format "ID not found: %s" id)))
        (let ((allowed-file
               (org-mcp--find-allowed-file
                (file-name-nondirectory id-file))))
          (unless allowed-file
            (mcp-server-lib-tool-throw
             (format "File not in allowed list: %s"
                     (file-name-nondirectory id-file))))
          (setq file-path (expand-file-name allowed-file))
          ;; For ID-based, we'll find the headline by ID
          (setq headline-path (list id)))))
     (t
      (mcp-server-lib-tool-throw
       (format "Invalid resource URI format: %s" resourceUri))))

    ;; Validate new state is in org-todo-keywords
    (unless (member
             newState (apply 'append (mapcar 'cdr org-todo-keywords)))
      (mcp-server-lib-tool-throw
       (format "Invalid TODO state: %s" newState)))

    ;; Check if any buffer visiting this file has unsaved changes
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (string= (buffer-file-name) file-path)
                   (buffer-modified-p))
          (mcp-server-lib-tool-throw
           "Cannot update: file has unsaved changes in buffer"))))

    ;; Update the TODO state in the file
    (with-temp-buffer
      (insert-file-contents file-path)
      (org-mode)
      (goto-char (point-min))

      ;; Find the headline
      (let ((found nil))
        (if (and (= (length headline-path) 1)
                 (string-match "^[a-f0-9-]+$" (car headline-path)))
            ;; ID-based search
            (let ((pos (org-find-property "ID" (car headline-path))))
              (when pos
                (goto-char pos)
                (setq found t)))
          ;; Path-based search
          (catch 'not-found
            (dolist (target-title headline-path)
              (setq found nil)
              (while (and (not found)
                          (re-search-forward "^\\*+ " nil t))
                (let ((title (org-get-heading t t t t)))
                  (when (string= title target-title)
                    (setq found t))))
              (unless found
                (throw 'not-found nil)))
            (setq found t)))

        (unless found
          (mcp-server-lib-tool-throw "Headline not found"))

        ;; Check current state matches
        (beginning-of-line)
        (let ((actual-state (org-get-todo-state)))
          (unless (string= actual-state currentState)
            (mcp-server-lib-tool-throw
             (format "State mismatch: expected %s, found %s"
                     (or currentState "(no state)")
                     (or actual-state "(no state)"))))

          ;; Update the state
          (org-todo newState)

          ;; Write the updated content back
          (write-region (point-min) (point-max) file-path)

          ;; Update any buffers visiting this file (all unmodified)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (and (buffer-file-name)
                         (string= (buffer-file-name) file-path))
                ;; Safe to revert - already checked for modifications
                (revert-buffer t t t))))

          ;; Return success
          (json-encode
           `((success . t)
             (previousState . ,(or currentState ""))
             (newState . ,newState))))))))

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
Throws an error if multiple tags from the same mutex group are present."
  (let ((mutex-groups (org-mcp--parse-mutex-tag-groups tag-alist)))
    (dolist (group mutex-groups)
      (let ((tags-in-group
             (cl-intersection tags group :test #'string=)))
        (when (> (length tags-in-group) 1)
          (mcp-server-lib-tool-throw
           (format
            "Tags %s are mutually exclusive (cannot use together)"
            (mapconcat (lambda (tag)
                         (format "'%s'" tag))
                       tags-in-group
                       ", "))))))))

(defun org-mcp--tool-add-todo
    (title todoState tags body parentUri afterUri)
  "Add a new TODO item to an Org file.
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

  ;; Validate TODO state
  (unless (member
           todoState (apply 'append (mapcar 'cdr org-todo-keywords)))
    (mcp-server-lib-tool-throw
     (format "Invalid TODO state: %s" todoState)))

  ;; Validate tags
  (let ((tag-list
         (cond
          ((vectorp tags)
           (append tags nil)) ; Convert JSON array (vector) to list
          ((stringp tags)
           (list tags)) ; Single tag string
          (t
           (mcp-server-lib-tool-throw
            (format "Invalid tags format: %s" tags))))))
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
            (mcp-server-lib-tool-throw
             (format "Tag not in configured tag alist: %s" tag)))))
      ;; Always validate tag names follow Org's rules
      (dolist (tag tag-list)
        (unless (string-match "^[[:alnum:]_@]+$" tag)
          (mcp-server-lib-tool-throw
           (format
            "Invalid tag name (must be alphanumeric, _, or @): %s"
            tag))))
      ;; Validate mutual exclusivity if tag-alist is configured
      (when org-tag-alist
        (org-mcp--validate-mutex-tag-groups tag-list org-tag-alist))
      (when org-tag-persistent-alist
        (org-mcp--validate-mutex-tag-groups
         tag-list org-tag-persistent-alist))))

  ;; Parse parent URI to get file path
  (let (file-path)
    (cond
     ;; Handle org-headline:// URIs
     ((string-match "^org-headline://\\([^/]+\\)/" parentUri)
      (let* ((filename (match-string 1 parentUri))
             (allowed-file (org-mcp--validate-file-access filename)))
        (setq file-path (expand-file-name allowed-file))))
     ;; Handle org-id:// URIs
     ((string-match "^org-id://\\(.+\\)$" parentUri)
      ;; For org-id, we need to find which file contains this ID
      ;; We'll search through allowed files
      (let ((parent-id (match-string 1 parentUri))
            (found nil))
        (dolist (allowed-file org-mcp-allowed-files)
          (unless found
            (when (file-exists-p allowed-file)
              (with-temp-buffer
                (insert-file-contents allowed-file)
                (org-mode)
                (goto-char (point-min))
                (while (and (not found)
                            (re-search-forward "^\\*+ " nil t))
                  (when (string= (org-entry-get nil "ID") parent-id)
                    (setq found t)
                    (setq file-path
                          (expand-file-name allowed-file))))))))
        (unless found
          (mcp-server-lib-tool-throw
           (format "Parent with ID %s not found in allowed files"
                   parent-id)))))
     (t
      (mcp-server-lib-tool-throw
       (format "Invalid parent URI format: %s" parentUri))))

    ;; Check for unsaved changes
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (string= (buffer-file-name) file-path)
                   (buffer-modified-p))
          (mcp-server-lib-tool-throw
           "Cannot add TODO: file has unsaved changes in buffer"))))

    ;; Add the TODO item
    (with-temp-buffer
      (set-visited-file-name file-path t)
      (insert-file-contents file-path)
      (org-mode)

      ;; Navigate to insertion point based on parentUri
      (let ((parent-path nil)
            (parent-id nil))
        ;; Parse the parent URI - either org-headline:// or org-id://
        (cond
         ;; org-headline:// format
         ((string-match "^org-headline://[^/]+/\\(.*\\)$" parentUri)
          (let ((path-str (match-string 1 parentUri)))
            (when (> (length path-str) 0)
              (setq parent-path
                    (split-string (url-unhex-string path-str) "/")))))
         ;; org-id:// format
         ((string-match "^org-id://\\(.+\\)$" parentUri)
          (setq parent-id (match-string 1 parentUri))))

        ;; Navigate to parent if specified
        (cond
         ;; Navigate by path
         (parent-path
          (goto-char (point-min))
          (dolist (title parent-path)
            (unless (re-search-forward (format "^\\*+ %s"
                                               (regexp-quote title))
                                       nil t)
              (mcp-server-lib-tool-throw
               (format "Parent headline not found: %s" title))))
          ;; Parent found, point is at the parent heading
          )
         ;; Navigate by ID
         (parent-id
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found)
                        (re-search-forward "^\\*+ " nil t))
              (when (string= (org-entry-get nil "ID") parent-id)
                (setq found t)))
            (unless found
              (mcp-server-lib-tool-throw
               (format "Parent with ID not found: %s" parent-id))))
          ;; Parent found, point is at the parent heading
          )
         ;; No parent specified - top level
         (t
          (goto-char (point-min))
          ;; Skip past any header comments (#+TITLE, #+AUTHOR, etc.)
          (while (and (not (eobp)) (looking-at "^#\\+"))
            (forward-line))
          ;; Position at the right place: if there's a blank line after headers,
          ;; move past it; if there's a headline immediately after, stay there
          (when (and (not (eobp)) (looking-at "^[ \t]*$"))
            ;; We're on a blank line after headers, skip to next non-blank
            (while (and (not (eobp)) (looking-at "^[ \t]*$"))
              (forward-line)))))

        ;; Handle positioning after navigation to parent
        (when (or parent-path parent-id)
          ;; Handle afterUri positioning
          (if afterUri
              (progn
                ;; Parse afterUri to get the ID
                (let
                    ((after-id
                      (if (string-match
                           "^org-id://\\(.+\\)$" afterUri)
                          (match-string 1 afterUri)
                        (mcp-server-lib-tool-throw
                         (format
                          "afterUri must be org-id:// format, got: %s"
                          afterUri)))))
                  ;; Find the sibling with the specified ID
                  (org-back-to-heading t) ;; Ensure we're at the parent heading
                  (let ((found nil)
                        (parent-end
                         (save-excursion
                           (org-end-of-subtree t t)
                           (point))))
                    ;; Search for the sibling within parent's subtree
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
                                ;; Move to end of this sibling's subtree
                                (org-end-of-subtree t t)))
                            (unless found
                              ;; Move to next sibling
                              (unless (org-get-next-sibling)
                                ;; No more siblings
                                (goto-char parent-end)))))
                      ;; No children
                      (goto-char parent-end))
                    (unless found
                      (mcp-server-lib-tool-throw
                       (format
                        "Sibling with ID %s not found under parent"
                        after-id))))))
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
                  ;; When afterUri is specified, we've positioned after a specific sibling
                  ;; Use org-insert-heading to insert right here
                  (progn
                    (org-insert-heading)
                    (insert title))
                ;; No afterUri - we're at the end of the parent's subtree
                ;; Need to create a child heading
                (progn
                  ;; Ensure blank line before child heading if there's content
                  (unless (or (bobp) (looking-back "\n\n" 2))
                    (insert "\n"))
                  ;; Use org-insert-subheading to create a proper child
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
        (when tags
          (let ((tag-list
                 (cond
                  ((vectorp tags)
                   (append tags nil)) ; Convert vector to list
                  ((listp tags)
                   tags)
                  (t
                   (list tags)))))
            (org-set-tags tag-list)))

        ;; Add ID property using Org functions
        (org-id-get-create)
        (let ((id (org-id-get)))

          ;; Add body if provided
          (when body
            ;; org-id-get-create leaves point at the heading
            ;; Move past the properties drawer if it exists
            (org-end-of-meta-data t)
            ;; Insert body
            (insert body)
            (unless (string-suffix-p "\n" body)
              (insert "\n")))

          ;; Write the file
          (write-region (point-min) (point-max) file-path)

          ;; Update buffers
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (and (buffer-file-name)
                         (string= (buffer-file-name) file-path))
                (revert-buffer t t t))))

          ;; Return result
          (json-encode
           `((success . t)
             (uri . ,(format "org-id://%s" id))
             (file . ,(file-name-nondirectory file-path))
             (title . ,title))))))))

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
   "org-headline://{filename}"
   #'org-mcp--handle-headline-resource
   :name "Org headline content"
   :description "Content of a specific Org headline by path"
   :mime-type "text/plain")
  (mcp-server-lib-register-resource
   "org-id://{uuid}"
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
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource "org://{filename}")
  (mcp-server-lib-unregister-resource "org-outline://{filename}")
  (mcp-server-lib-unregister-resource "org-headline://{filename}")
  (mcp-server-lib-unregister-resource "org-id://{uuid}"))

(provide 'org-mcp)
;;; org-mcp.el ends here
