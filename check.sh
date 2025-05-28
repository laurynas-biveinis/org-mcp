#!/bin/bash
# check.sh - Run all quality checks for org-mcp project
#
# Continue on errors but track them

#
# Linters / tests / autoformatters and their dependencies:
#
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

ERRORS=0
SHELL_SYNTAX_FAILED=0

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
if ls ./*.md >/dev/null 2>&1; then
	echo "$(echo ./*.md) "
	if mdl --no-verbose ./*.md; then
		echo "OK!"
	else
		echo "mdl check failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No markdown files found yet"
fi

echo -n "Checking Markdown formatting... "
if ls ./*.md >/dev/null 2>&1; then
	if prettier --log-level warn --check ./*.md; then
		echo "OK!"
	else
		echo "prettier check for Markdown failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "No markdown files to check"
fi

echo -n "Checking terminology... "
if ls ./*.md >/dev/null 2>&1; then
	if textlint --rule terminology ./*.md; then
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
