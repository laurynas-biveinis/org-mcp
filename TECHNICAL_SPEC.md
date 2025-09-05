# Technical Specifications and Design Requirements for an Emacs Org-mode MCP Server

## Two-Layer Architecture

### Layer 1: MCP Protocol (mcp-server-lib.el)

Required extensions for full MCP support:

- Resource registration and handling
- Subscription/notification support
- Prompt template support

### Layer 2: Org-mode Server (org-mcp.el)

A single, well-organized package containing all org-specific functionality:

- Security and file access validation
- Resource and tool implementations
- URI scheme and cross-referencing
- TODO keyword state semantics

## Resources vs Tools Design Pattern

### Resources for Data Access

Resources represent the document structure and content:

- Natural hierarchical URIs: `org://file.org/outline/Section`
- Enable subscriptions (future enhancement)
- Direct addressability for specific content
- RESTful semantics for read operations

Resources expose the document "as is" - the raw file, its structure, and
specific sections. Filtered views (like TODO items by state) are
intentionally not resources.

### Tools for Operations

Tools handle queries, filters, and modifications:

- Search across files with complex filters
- Get TODO items with multiple filter criteria
- Update TODO keyword states
- Complex transformations
- Cross-file operations

Tools provide the "queries" - dynamic views of the data based on search criteria, filters, and other parameters.

### Cross-Referencing Pattern

**Critical design principle**: Tools that return org content must include resource URIs to enable consistent data access:

```json
{
  "heading": "Implement feature",
  "todo": { "state": "TODO", "type": "todo", "isFinal": false },
  "resourceUri": "org://project.org/headline/Implement%20feature",
  "orgId": "550e8400-e29b-41d4-a716-446655440000"
}
```

When ID properties exist, tools return both the path-based `resourceUri` and the `orgId`. The ID enables persistent references through `org://id/{orgId}` URIs that survive renames, moves, and refiling operations.

## Key Implementation Components

### URI Scheme and Resource Templates

The implementation leverages mcp-server-lib's built-in resource template support (RFC 6570 URI templates) for dynamic resource serving.

#### URI Template Patterns

```
org://{filename}                 → Raw org file content
org://{filename}/outline         → Document structure/hierarchy
org://{filename}/headline/{path} → Path-based headline reference
org://id/{uuid}                  → ID-based headline reference (persistent)
```

#### Template Implementation

Using `mcp-server-lib-register-resource` with URI templates:

```elisp
;; Template resource with single parameter
(mcp-server-lib-register-resource
 "org://{filename}"
 (lambda (params)
   (org-mcp--read-file-resource 
     (alist-get "filename" params nil nil #'string=)))
 :name "Org file"
 :description "Raw org file content")

;; Template with multiple parameters
(mcp-server-lib-register-resource
 "org://{filename}/headline/{+path}"  ; {+path} allows slashes
 (lambda (params)
   (org-mcp--get-headline-content
     (alist-get "filename" params nil nil #'string=)
     (alist-get "path" params nil nil #'string=)))
 :name "Org headline"
 :description "Specific headline content")
```

The library automatically:
- Detects templates by presence of `{}` patterns
- Parses and validates URI templates
- Matches incoming URIs against templates
- Extracts parameters and passes them to handlers

Examples:

- `org://projects.org` - Complete org file
- `org://projects.org/outline` - Full document outline
- `org://projects.org/headline/Development/Tasks` - Path-based section reference
- `org://id/550e8400-e29b-41d4-a716-446655440000` - ID-based section reference

**ID-Based Addressing**: Org nodes with ID properties can be referenced using persistent ID-based URIs that survive renames, moves, and refiling. Tools return both URI types when an ID exists, allowing clients to choose based on their needs.

### TODO Keyword State Semantics

Semantic information derived from org's built-in configuration:

```elisp
{
  "state": "WAIT",
  "isFinal": false,        ; true for done states
  "sequenceType": "sequence" ; or "type"
}
```

This provides MCP clients with workflow understanding:
- `isFinal`: Whether this is a final state (after "|") or active state (before "|")
- `sequenceType`: Whether keywords represent workflow stages ("sequence") or task categories ("type")

The full sequence information is provided separately in the tool response to avoid redundancy.

### Security Model

- Whitelist allowed org files via `org-mcp-allowed-files`
- Validate all file access operations
- Prevent path traversal attacks
- Restrict dangerous elisp operations

### ID Creation Policy

**Mandatory ID creation**: When MCP clients create new Org nodes (headlines), they MUST generate and assign a unique ID property. This ensures:

- All MCP-created content has persistent, stable references from the start
- Clients can immediately use ID-based URIs for subsequent operations
- No ambiguity about which nodes have IDs

The server will automatically generate IDs using Org's built-in `org-id-new` function when creating headlines through MCP tools.

## Core Tool and Resource Definitions

### Essential Resources

```elisp
;; File and structure
org://{file}                      ; Raw org file content
org://{file}/outline              ; Document structure/hierarchy
org://{file}/headline/{path}      ; Path-based section reference
org://id/{uuid}                   ; ID-based section reference

;; Configuration
org://config/todo-states          ; TODO keyword configuration
```

### Essential Tools

#### org-search-headlines

Search for headlines across org files with text and tag filters.

**Parameters:**

- `query` (string): Text to search for in headline titles
- `files` (array of strings, optional): Files to search in (defaults to allowed files)
- `tags` (array of strings, optional): Required tags
- `excludeTags` (array of strings, optional): Tags to exclude
- `caseSensitive` (boolean, optional): Case-sensitive search (default: false)

**Returns:** Array of headline objects with resourceUri references (both path-based and ID-based when available)

#### org-get-todos

Retrieve TODO items with multiple filter criteria.

**Parameters:**

- `files` (array of strings, optional): Files to search (defaults to allowed files)
- `states` (array of strings, optional): TODO states to include (e.g., ["TODO", "NEXT"])
- `tags` (array of strings, optional): Required tags
- `scheduled` (object, optional): Date range filter {"from": "2024-01-01", "to": "2024-12-31"}
- `deadline` (object, optional): Date range filter

**Returns:** Array of TODO items with full metadata and resourceUri (both path-based and ID-based when available)

#### org-search-content

Full-text search within org file content (not just headlines).

**Parameters:**

- `query` (string): Text to search for
- `files` (array of strings, optional): Files to search
- `includeArchived` (boolean, optional): Include archived sections (default: false)
- `maxResults` (number, optional): Limit number of results

**Returns:** Array of matches with context and resourceUri to containing headline (both path-based and ID-based when available)

#### org-update-todo-state

Change the TODO state of a specific headline.

**Parameters:**

- `resourceUri` (string): URI of the headline to update (supports org-headline:// or org-id://)
- `currentState` (string): Current TODO state (empty string "" for no state) - must match actual state for update to proceed
- `newState` (string): New TODO state (must be valid in org-todo-keywords, empty string "" to remove state)

**Returns:** 
- Success: `{"success": true, "previousState": "TODO", "newState": "DONE"}`
- State mismatch error: `{"error": "State mismatch", "message": "...", "actualState": "...", "expectedState": "..."}`

#### org-schedule-task

Set or update scheduling/deadline for a headline.

**Parameters:**

- `resourceUri` (string): URI of the headline
- `scheduled` (string, optional): Scheduled date in org format (e.g., "2024-12-25")
- `deadline` (string, optional): Deadline date in org format
- `removeScheduled` (boolean, optional): Remove existing scheduled date
- `removeDeadline` (boolean, optional): Remove existing deadline

**Returns:** Success status and updated scheduling info

#### org-get-properties

Extract property drawer contents for a headline.

**Parameters:**

- `resourceUri` (string): URI of the headline
- `properties` (array of strings, optional): Specific properties to retrieve

**Returns:** Object with property key-value pairs

#### org-get-todo-config

Get the TODO keyword configuration for understanding task states.

**Parameters:** None

**Returns:** Object containing:

- `sequences`: Array of sequence objects, each with:
  - `type`: "sequence" or "type" indicating interpretation
  - `keywords`: Array of keywords including "|" separator
- `semantics`: Array of state objects for all keywords with:
  - `state`: The keyword string
  - `isFinal`: Boolean indicating if this is a final state
  - `sequenceType`: "sequence" or "type" matching the parent sequence

#### org-get-tag-config

Get the tag configuration as literal Elisp variable values.

**Parameters:** None

**Returns:** Object containing literal Elisp string representations of:

- `org-use-tag-inheritance`: Literal string representation of the variable value (e.g., `"t"`, `"nil"`, `"(\"tag1\" \"tag2\")"`, or `"\"^regex\""`)
- `org-tags-exclude-from-inheritance`: Literal string of the exclusion list (e.g., `"(\"tag1\" \"tag2\")"`)
- `org-tags-sort-function`: Literal string of the sort function value
- `org-tag-alist`: Literal string of the complete tag alist configuration
- `org-tag-persistent-alist`: Literal string of the persistent tag alist

## Implementation Approach

### File Structure

```
org-mcp.el              ; Single file containing all org-specific code
├── Configuration       ; defcustom for allowed files
├── Security           ; File access validation
├── URI Handling       ; Parse and generate resource URIs
├── Tool Implementations
├── Resource Implementations
└── Server Lifecycle   ; Start/stop functions
```

### MCP Protocol Library Features

The `mcp-server-lib.el` already provides:

```elisp
;; Resource registration with template support
(defun mcp-server-lib-register-resource (uri handler &rest properties)
  "Register resource with automatic template detection.
   URI can be static (org://file.org) or template (org://{filename})."
  ...)

;; Future extensions needed:
;; - Subscription support
(defun mcp-server-lib-notify-resource-changed (uri &optional data))
```

Resource templates follow RFC 6570 URI template syntax:
- `{variable}` - Simple expansion
- `{+variable}` - Reserved character expansion (allows slashes)

### Resource URI Hierarchy

```
org://projects.org                   → Raw org file content
org://projects.org/outline           → Structural view of document
org://projects.org/headline/Alpha    → Specific section with content
org://config/todo-states            → TODO configuration (global)
```

## Implementation Roadmap

1. **Phase 1: Basic Setup with Tools Only**

   - Create org-mcp.el with basic structure
   - Implement security validation for allowed files
   - Implement `org-get-todo-config` tool (no file access needed)
   - Implement `org-search-headlines` tool using existing mcp-server-lib
   - Test basic tool functionality manually

2. **Phase 2: Resource Support**

   - Use mcp-server-lib's built-in resource template support
   - Implement template-based resources for files, outlines, and headlines
   - Create handler functions for each resource type
   - Update tools to include resource URIs in responses

3. **Phase 3: Complete Read Operations**

   - Implement remaining search tools (TODOs, content)
   - Add property extraction
   - Ensure consistent TODO state semantics
   - Full cross-referencing between tools and resources

4. **Phase 4: Write Operations**

   - TODO state updates
   - Task scheduling/deadline management
   - Property modifications
   - Change notifications

5. **Phase 5: Future Development**
   - See "Future Potential Development" section for advanced features

## Key Design Principles

1. **Simplicity**: Two-layer architecture keeps concerns separated
2. **Security**: All file access is validated against file whitelist
3. **Consistency**: Every piece of org data has a resource URI
4. **Zero Configuration**: TODO semantics work out-of-the-box
5. **Extensibility**: Easy to add new tools and resources

## Testing Strategy

- Test manually for protocol compliance
- Test with representative org files
- Verify security boundaries
- Ensure cross-referencing works correctly

## Future Potential Development

### Additional Tools

#### org-export-section

Export a section to different formats.

**Parameters:**

- `resourceUri` (string): URI of the section to export
- `format` (string): Target format ("html", "markdown", "latex", "plain")
- `includeSubheadings` (boolean, optional): Include child headlines (default: true)
- `bodyOnly` (boolean, optional): Export body without headers (default: false)

**Returns:** Exported content as string

#### org-get-agenda-items

Get items that would appear in org agenda.

**Parameters:**

- `files` (array of strings, optional): Files to check
- `days` (number, optional): Number of days to look ahead (default: 7)
- `includeScheduled` (boolean, optional): Include scheduled items (default: true)
- `includeDeadlines` (boolean, optional): Include deadlines (default: true)

**Returns:** Array of agenda items sorted by date with resourceUri

### Advanced Features

- Resource subscriptions and file watching
- Org-roam integration for linked notes
- Capture template execution
- Refiling capabilities
- Archive operations
- Performance optimizations (caching, streaming)
- Bulk operations for TODO updates
- Complex query language for advanced filtering

