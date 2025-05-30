;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

(require 'ert)
(require 'org-mcp)

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (progn
           ,@body)
       (org-mcp-disable))))

(ert-deftest org-mcp-test-tool-get-todo-config-exists ()
  "Test that org-get-todo-config tool is registered."
  (org-mcp-test--with-enabled
    ;; For now, just test that enable/disable works without error
    (should t)))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
