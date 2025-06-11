;;; org-mcp.el --- MCP server for Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 org-mcp contributors

;; Author: org-mcp contributors
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
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
  "List of Org files that the MCP server is allowed to access.
File paths can be absolute or relative, and may use ~ for home directory.
When accessed via MCP tools, paths will be expanded to absolute paths."
  :type '(repeat string)
  :group 'org-mcp)

(defun org-mcp--tool-list-allowed-files ()
  "Return the list of allowed Org files with expanded paths."
  (unless (listp org-mcp-allowed-files)
    (mcp-server-lib-tool-throw
     "org-mcp-allowed-files must be a list"))
  (let ((result []))
    (dolist (file org-mcp-allowed-files)
      (unless (stringp file)
        (mcp-server-lib-tool-throw
         (format "Invalid element in org-mcp-allowed-files: %S"
                 file)))
      (when (string-empty-p file)
        (mcp-server-lib-tool-throw
         "Empty file path in org-mcp-allowed-files"))
      (condition-case err
          (setq result
                (vconcat result (vector (expand-file-name file))))
        (error
         (mcp-server-lib-tool-throw
          (format "Invalid file path: %s (%s)"
                  file (error-message-string err))))))
    result))

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

(defun org-mcp-enable ()
  "Enable the org-mcp server."
  (mcp-server-lib-register-tool
   #'org-mcp--tool-get-todo-config
   :id "org-get-todo-config"
   :description "Get TODO keyword configuration for task states"
   :read-only t)
  (mcp-server-lib-register-tool
   #'org-mcp--tool-list-allowed-files
   :id "org-list-allowed-files"
   :description
   "Discover available Org files for MCP operations.
Essential for clients to know which files they can query/modify.
These paths can be used in 'files' parameters of other tools."
   :read-only t))

(defun org-mcp-disable ()
  "Disable the org-mcp server."
  (mcp-server-lib-unregister-tool "org-get-todo-config")
  (mcp-server-lib-unregister-tool "org-list-allowed-files"))

(provide 'org-mcp)
;;; org-mcp.el ends here
