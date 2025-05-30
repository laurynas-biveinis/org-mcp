# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

This repository is for org-mcp, which appears to be an integration between
Emacs Org-mode and the Model Context Protocol (MCP).

## Development Methodology

**IMPORTANT**: This project strictly follows Test-Driven Development (TDD)
with the smallest possible iteration steps:

1. Write user-facing documentation for the feature in README.org
1. Write ONE failing test for the smallest piece of functionality
1. Run `./check.sh` to verify the test fails
1. Write the minimal code to make the test pass
1. Run `./check.sh` to verify the test passes
1. Refactor if needed
1. Run `./check.sh` to ensure tests still pass
1. Repeat with the next small increment

Each iteration should be as small as possible - test and implement one tiny
behavior at a time. NEVER write multiple tests at once - always implement
one test completely before writing the next test.

**IMPORTANT**: Always fix linting errors properly. Never suppress linter
warnings or errors with configuration files unless explicitly instructed by
the user. Fix the actual issues in the code/documentation.

## Development Setup

Since this is a new project, the following setup steps will likely be needed:

### Emacs Lisp Development

- Files should follow Emacs Lisp conventions (`.el` extension)
- Use standard Emacs package headers and commentary sections
- Follow Emacs Lisp coding conventions

## Architecture Notes

As this project develops, key architectural decisions should be documented
here, such as:

- How Org-mode buffers interact with MCP
- The protocol for communication between Emacs and MCP servers
- Data structures used for representing Org elements in MCP format

## MCP Integration

When implementing MCP functionality:

- Follow the MCP specification for server/client communication
- Consider using JSON-RPC for protocol implementation
- Ensure proper error handling for network communication
