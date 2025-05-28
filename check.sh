#!/bin/bash
# check.sh - Run all quality checks for org-mcp project
#
# Continue on errors but track them

set -eu -o pipefail

ERRORS=0

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