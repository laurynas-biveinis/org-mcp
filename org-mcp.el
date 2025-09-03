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

(require 'mcp-server-lib)
(require 'org)

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
    `((sequences . ,(vconcat (nreverse seq-list)))
      (semantics . ,(vconcat (nreverse sem-list))))))

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

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-todo-config
   :id "org-get-todo-config"
   :description "Get TODO keyword configuration for task states"
   :read-only t)
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
   :mime-type "application/json"))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-tool "org-get-todo-config")
  ;; Unregister template resources
  (mcp-server-lib-unregister-resource "org://{filename}")
  (mcp-server-lib-unregister-resource "org-outline://{filename}"))

(provide 'org-mcp)
;;; org-mcp.el ends here
