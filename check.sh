#!/bin/bash
# check.sh - Run all quality checks for org-mcp project
#
# Continue on errors but track them

#
# Linters / tests / autoformatters and their dependencies:
#
# - Elisp:
#   - Start with the syntax check (via byte compilation)
#     - Catches major issues like unbalanced parentheses
#     - LLMs tend to generate syntax errors, so check these first
#   - Run elisp-autofmt
#     - Skip if there are syntax errors to prevent incorrect runaway
#       re-indentation
#   - Run elisp-lint
#     - Skip if there are either syntax errors or autoformatting failure
#     - It may still produce formatting issues even after a successful
#       elisp-autofmt call, but much fewer of them, and more suitable for fixing
#       by the LLM.
#   - Run ERT tests
#     - Skip if there were syntax errors
# - Org (elisp-adjacent): call Emacs org-lint on Org files
# - Shell:
#   - Check syntax with bash -n
#   - Do shellcheck static analysis
#     - Only if the syntax check passed
#   - Format all the shell scripts with shfmt
#     - Only if the syntax check passed to avoid runaway re-indentation
# - Markdown:
#   - Lint with mdl
#   - Check formatting with prettier
#   - Check terminology with textlint

set -eu -o pipefail

readonly ELISP_FILES='"org-mcp.el" "org-mcp-test.el"'
readonly ORG_FILES='"README.org"'
readonly SHELL_FILES=(check.sh)
# Explicitly list markdown files to lint, excluding uncommitted LLM scratch/memory files
readonly MARKDOWN_FILES=(CLAUDE.md)

readonly EMACS="emacs -Q --batch"

# Elisp packages in ELPA
MCP_SERVER_LIB=$(find ~/.emacs.d/elpa/ -maxdepth 1 -name "mcp-server-lib-*" -type d 2>/dev/null | head -1 | xargs basename)
readonly MCP_SERVER_LIB
ELISP_AUTOFMT=$(find ~/.emacs.d/elpa/ -maxdepth 1 -name "elisp-autofmt-*" -type d 2>/dev/null | head -1 | xargs basename)
readonly ELISP_AUTOFMT
readonly ELISP_LINT="elisp-lint-20220419.252"
readonly PACKAGE_LINT="package-lint-0.26"
readonly DASH="dash-20250312.1307"

ERRORS=0
ELISP_SYNTAX_FAILED=0
SHELL_SYNTAX_FAILED=0

# Elisp

echo -n "Checking Elisp syntax... "
if eask recompile; then
	echo "OK!"
else
	echo "Elisp syntax check failed!"
	ERRORS=$((ERRORS + 1))
	ELISP_SYNTAX_FAILED=1
fi

# Only run indentation if there are no syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running elisp-autofmt... "
	if $EMACS --eval "(add-to-list 'load-path (locate-user-emacs-file \"elpa/$ELISP_AUTOFMT\"))" \
		--eval "(add-to-list 'load-path (expand-file-name \".\"))" \
		--eval "(require 'elisp-autofmt)" \
		--eval "(dolist (file '($ELISP_FILES))
                   (princ (format \"%s \" file))
               (find-file file)
               (elisp-autofmt-buffer-to-file))"; then
		echo "OK!"
	else
		echo "elisp-autofmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping indentation due to syntax errors"
fi

# Remove byte-compiled files before linter to avoid conflicts
rm -f ./*.elc

# Only run elisp-lint if there are no errors so far
if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-lint... "
	if $EMACS --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/$MCP_SERVER_LIB\")
                                          (locate-user-emacs-file \"elpa/$ELISP_LINT\")
                                          (locate-user-emacs-file \"elpa/$PACKAGE_LINT\")
                                          (locate-user-emacs-file \"elpa/$DASH\")
                                          (expand-file-name \".\"))))
                         (dolist (dir pkg-dirs)
                           (add-to-list 'load-path dir))
                         (require 'elisp-lint)
                         (let ((has-errors nil))
                           (dolist (file (list $ELISP_FILES))
                               (princ (format \"%s \" file))
                               ;; Skip package-lint for test files
                               (let ((elisp-lint-ignored-validators
                                      (if (string-match-p \"test\" file)
                                          '(\"package-lint\")
                                        nil)))
                                 (unless (elisp-lint-file file)
                                   (setq has-errors t))))
                           (when has-errors
                             (kill-emacs 1))))"; then
		echo "OK!"
	else
		echo "elisp-lint failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping elisp-lint due to previous errors"
fi

# Remove byte-compiled files after elisp-lint
rm -f ./*.elc

# Only run ERT tests if there are no Elisp syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running all tests... "
	if $EMACS --eval \
		"(add-to-list 'load-path (locate-user-emacs-file \"elpa/$MCP_SERVER_LIB\"))" \
		--eval "(add-to-list 'load-path (expand-file-name \".\"))" \
		-l org-mcp-test.el --eval '(let ((ert-quiet t))
          (ert-run-tests-batch-and-exit))'; then
		echo "OK!"
	else
		echo "ERT tests failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping ERT tests due to Elisp syntax errors"
fi

# Org

echo -n "Checking org files... $(echo "$ORG_FILES" | tr -d '"') "
if $EMACS --eval "(require 'org)" --eval "(require 'org-lint)" \
	--eval "(let ((all-checks-passed t))
             (dolist (file '($ORG_FILES) all-checks-passed)
               (with-temp-buffer
                 (insert-file-contents file)
                 (org-mode)
                 (let ((results (org-lint)))
                   (when results
                     (message \"Found issues in %s: %S\" file results)
                     (setq all-checks-passed nil)))))
             (unless all-checks-passed (kill-emacs 1)))"; then
	echo "OK!"
else
	echo "org files check failed"
	ERRORS=$((ERRORS + 1))
fi

# Shell

echo -n "Checking shell syntax... ${SHELL_FILES[*]} "
if bash -n "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
	SHELL_SYNTAX_FAILED=1
fi

if [ $SHELL_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${SHELL_FILES[*]} "
	if shellcheck "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format all shell scripts... ${SHELL_FILES[*]} "
	if shfmt -w "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping shellcheck and shfmt due to previous errors"
fi

# Markdown

echo -n "Checking Markdown files... "
if [ ${#MARKDOWN_FILES[@]} -gt 0 ]; then
	echo "${MARKDOWN_FILES[*]} "
	if mdl --no-verbose "${MARKDOWN_FILES[@]}"; then
		echo "OK!"
	else
		echo "mdl check failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No markdown files configured"
fi

echo -n "Checking Markdown formatting... "
if [ ${#MARKDOWN_FILES[@]} -gt 0 ]; then
	if prettier --log-level warn --check "${MARKDOWN_FILES[@]}"; then
		echo "OK!"
	else
		echo "prettier check for Markdown failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No markdown files to check"
fi

echo -n "Checking YAML formatting... $(echo .github/workflows/*.yml) "
if prettier --log-level warn --check .github/workflows/*.yml; then
	echo "OK!"
else
	echo "prettier check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking terminology... "
if [ ${#MARKDOWN_FILES[@]} -gt 0 ]; then
	if textlint --rule terminology "${MARKDOWN_FILES[@]}"; then
		echo "OK!"
	else
		echo "textlint check failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No markdown files to check"
fi

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
