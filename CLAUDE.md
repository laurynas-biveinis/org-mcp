# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository. These guidelines build on the common user's
guidelines at ~/.claude/CLAUDE.md locally or
<https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md>
online.

## Project Overview

This repository is for org-mcp, which is an integration between Emacs Org-mode
and the Model Context Protocol (MCP).

This is an Elisp project, and should follow user's Elisp guidelines at
@~/.claude/CLAUDE-elisp.md.

User-facing documentation is in README.org.

## Development Methodology

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

## Testing Guidelines

- Test MAY NOT call any internal APIs or access internal variables of this
  package, or those of any dependencies
