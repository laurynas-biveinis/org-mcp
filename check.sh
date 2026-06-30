#!/bin/bash
# check.sh - Run all quality checks, with focus on providing guardrails for LLM
# coding agents.
#
# Continue on errors but track them.
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
#   - Run eask lint keywords and eask lint regexps
#     - Skip if there were syntax errors
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
# - GitHub Actions / YAML:
#   - Check workflows with actionlint
#   - Check workflow security with zizmor
#   - Check workflow security with checkov
#   - Check YAML formatting with prettier
# - JavaScript/TypeScript/JSON (Biome):
#   - Check formatting with Biome
#   - Lint with Biome

set -eu -o pipefail

readonly SHELL_FILES=(check.sh)
# Explicitly list markdown files to lint, excluding uncommitted LLM
# scratch/memory files.
readonly MARKDOWN_FILES=(CLAUDE.md)
# Test files are excluded from the Eask package file set, so the no-argument
# eask format/lint commands below do not see them; pass them explicitly.
readonly ELISP_TEST_FILES=(org-mcp-test.el)

ERRORS=0
ELISP_SYNTAX_FAILED=0
SHELL_SYNTAX_FAILED=0

# Install dependencies first (required for syntax check and tests)
echo -n "Installing dependencies... "
if eask install-deps; then
	echo "OK!"
else
	echo "Failed to install dependencies!"
	ERRORS=$((ERRORS + 1))
fi

# Elisp

echo -n "Checking Elisp syntax... "
# recompile covers the package files (Eask `files'); the test suite is not a
# package file, so byte-compile it explicitly too.
if eask recompile && eask compile "${ELISP_TEST_FILES[@]}"; then
	echo "OK!"
else
	echo "Elisp syntax check failed!"
	ERRORS=$((ERRORS + 1))
	ELISP_SYNTAX_FAILED=1
fi

# Only run indentation if there are no syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running elisp-autofmt... "
	if eask format elisp-autofmt &&
		eask format elisp-autofmt "${ELISP_TEST_FILES[@]}"; then
		echo "OK!"
	else
		echo "elisp-autofmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping indentation due to syntax errors"
fi

# Remove byte-compiled files before linter to avoid conflicts
eask clean elc

# Only run elisp-lint if there are no errors so far
if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-lint... "
	if eask lint elisp-lint &&
		eask lint elisp-lint "${ELISP_TEST_FILES[@]}"; then
		echo "OK!"
	else
		echo "elisp-lint failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping elisp-lint due to previous errors"
fi

# Remove byte-compiled files after elisp-lint
eask clean elc

# These linters read source forms, not byte-compiled output, so gate them on
# syntax errors like the ERT block rather than the broader ERRORS.
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running eask lint keywords... "
	if eask lint keywords; then
		echo "OK!"
	else
		echo "eask lint keywords failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running eask lint regexps... "
	if eask lint regexps &&
		eask lint regexps "${ELISP_TEST_FILES[@]}"; then
		echo "OK!"
	else
		echo "eask lint regexps failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping eask lint keywords and regexps due to syntax errors"
fi

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

echo -n "Checking Markdown files... ${MARKDOWN_FILES[*]} "
if mdl --no-verbose "${MARKDOWN_FILES[@]}"; then
	echo "OK!"
else
	echo "mdl check failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown formatting... ${MARKDOWN_FILES[*]} "
if prettier --log-level warn --check "${MARKDOWN_FILES[@]}"; then
	echo "OK!"
else
	echo "prettier check for Markdown failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking terminology... ${MARKDOWN_FILES[*]} "
if textlint --rule terminology "${MARKDOWN_FILES[@]}"; then
	echo "OK!"
else
	echo "textlint check failed"
	ERRORS=$((ERRORS + 1))
fi

# GitHub Actions / YAML

echo -n "Checking GitHub workflows... $(echo .github/workflows/*.yml) "
if actionlint .github/workflows/*.yml; then
	echo "OK!"
else
	echo "actionlint check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking GitHub Actions security with zizmor... "
if zizmor --offline .github/workflows/*.yml; then
	echo "OK!"
else
	echo "zizmor check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo "Checking GitHub Actions security with checkov..."
if ! checkov --framework github_actions --directory .github/workflows --compact --quiet; then
	echo "checkov check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Checking YAML formatting... $(echo .github/workflows/*.yml) "
if prettier --log-level warn --check .github/workflows/*.yml; then
	echo "OK!"
else
	echo "prettier check failed!"
	ERRORS=$((ERRORS + 1))
fi

# Biome

echo -n "Running Biome format check... "
if npx @biomejs/biome format .; then
	echo "OK!"
else
	echo "Biome format check failed!"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running Biome lint... "
if npx @biomejs/biome lint .; then
	echo "OK!"
else
	echo "Biome lint failed!"
	ERRORS=$((ERRORS + 1))
fi

# jscpd copy/paste detection, matching the super-linter CI check
# (jscpd 5.x, threshold 0 -- any duplicate fails).
echo -n "Running jscpd... "
if npx --yes jscpd@5.0.10 --threshold 0 .; then
	echo "OK!"
else
	echo "jscpd found duplicate code"
	ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
	echo "All checks passed successfully!"
else
	echo "$ERRORS check(s) failed!"
	exit 1
fi
