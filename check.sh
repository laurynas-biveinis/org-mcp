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

readonly SHELL_FILES=(check.sh)
# Explicitly list markdown files to lint, excluding uncommitted LLM scratch/memory files
readonly MARKDOWN_FILES=(CLAUDE.md)

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
	if eask format elisp-autofmt; then
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
	if eask lint elisp-lint; then
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
	if eask run script test; then
		echo "OK!"
	else
		echo "ERT tests failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping ERT tests due to Elisp syntax errors"
fi

# Org

echo -n "Checking org files... "
if eask run script org-lint; then
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
