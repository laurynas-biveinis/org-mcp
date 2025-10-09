;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((fill-column . 70)
                     (indent-tabs-mode . nil)
                     (elisp-lint-indent-specs . ((org-mcp--with-org-file-buffer . 2)
                                                  (org-mcp--with-uri-prefix-dispatch . 1)
                                                  (org-mcp-test--with-enabled . defun)
                                                  (org-mcp-test--with-config . 1)
                                                  (org-mcp-test--with-temp-org-file . 2)
                                                  (org-mcp-test--with-add-todo-setup . 2)
                                                  (org-mcp-test--with-id-setup . 1)
                                                  (org-mcp-test--refile-and-verify . 2)
                                                  (mcp-server-lib-ert-with-server . defun))))))