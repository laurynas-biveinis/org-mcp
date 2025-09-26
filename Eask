;; -*- mode: eask; lexical-binding: t -*-

(package "org-mcp"
         "0.1.0"
         "MCP server for Org-mode")

(website-url "https://github.com/laurynas-biveinis/org-mcp")
(keywords "tools" "convenience")

(source "melpa")

(depends-on "emacs" "27.1")
(depends-on "mcp-server-lib" "0.2.0")

(setq network-security-level 'low)

(package-file "org-mcp.el")

(files "org-mcp.el")

(script "test" "eask test ert org-mcp-test.el")

(script "org-lint" 
        (concat "emacs -Q --batch "
                "--eval \"(require 'org)\" "
                "--eval \"(require 'org-lint)\" "
                "--eval \"(let ((all-checks-passed t) "
                        "(files '(\\\"README.org\\\"))) "
                    "(dolist (file files all-checks-passed) "
                      "(with-temp-buffer "
                        "(insert-file-contents file) "
                        "(org-mode) "
                        "(let ((results (org-lint))) "
                          "(when results "
                            "(message \\\"Found issues in %s: %S\\\" file results) "
                            "(setq all-checks-passed nil))))) "
                    "(unless all-checks-passed (kill-emacs 1)))\""))
