# CLAUDE.md

This file provides guidance to you, Claude Code, when working with code in this
repository. These guidelines build on the common user's guidelines at
~/.claude/CLAUDE.md locally or
<https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md>
online.

## Project Overview

This repository is for org-mcp, which is an integration between Emacs Org-mode
and the Model Context Protocol (MCP).

User-facing documentation is in README.org.

In the ERT tests, always use defconst constants for before and after Org file
images.

To verify the changed Org content, use a single regular expression, matching
the complete Org file.

Test-fixture `defconst` docstrings describe the data, not the tests that use
it. Keep them brief and focused on the fixture's shape or role; put
test-specific rationale ("why this test exists", "what regression this locks
in", "what bug this catches") in the `ert-deftest` docstring instead.
Fixtures are reusable; coupling them to one test's narrative rots when other
tests reuse the same fixture.
