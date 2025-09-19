# Technical Specifications and Design Requirements for an Emacs Org-mode MCP Server

## Two-Layer Architecture

### Layer 1: MCP Protocol (mcp-server-lib.el)

The existing mcp-server-lib.el provides all needed MCP protocol support.

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

**Critical design principles**:

1. **Read-only tools**: Return org content with resource URIs to enable consistent data access. When ID properties exist, return both the path-based `uri` and the `orgId`.

2. **Write operations**: Any non-read-only tool that operates on an org node:
   - MUST return the node's ID regardless of how it was addressed (by path or ID)
   - MUST create an ID if the node doesn't have one
   - This ensures all modified content has persistent, stable references

```json
{
  "heading": "Implement feature",
  "todo": { "state": "TODO", "type": "todo", "isFinal": false },
  "uri": "org://project.org/headline/Implement%20feature",
  "orgId": "550e8400-e29b-41d4-a716-446655440000"
}
```

The ID enables persistent references through `org://id/{orgId}` URIs that survive renames, moves, and refiling operations.

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


Examples:

- `org://projects.org` - Complete org file
- `org://projects.org/outline` - Full document outline
- `org://projects.org/headline/Development/Tasks` - Path-based section reference
- `org://id/550e8400-e29b-41d4-a716-446655440000` - ID-based section reference

**ID-Based Addressing**: Org nodes with ID properties can be referenced using persistent ID-based URIs that survive renames, moves, and refiling. Tools return both URI types when an ID exists, allowing clients to choose based on their needs.


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


### Essential Tools

#### org-search-headlines

Search for headlines across org files with text and tag filters.

**Parameters:**

- `query` (string): Text to search for in headline titles
- `files` (array of strings, optional): Files to search in (defaults to allowed files)
- `tags` (array of strings, optional): Required tags
- `excludeTags` (array of strings, optional): Tags to exclude
- `caseSensitive` (boolean, optional): Case-sensitive search (default: false)

**Returns:** Array of headline objects with uri references (both path-based and ID-based when available)

#### org-get-todos

Retrieve TODO items with multiple filter criteria.

**Parameters:**

- `files` (array of strings, optional): Files to search (defaults to allowed files)
- `states` (array of strings, optional): TODO states to include (e.g., ["TODO", "NEXT"])
- `tags` (array of strings, optional): Required tags
- `scheduled` (object, optional): Date range filter {"from": "2024-01-01", "to": "2024-12-31"}
- `deadline` (object, optional): Date range filter

**Returns:** Array of TODO items with full metadata and uri (both path-based and ID-based when available)

#### org-search-content

Full-text search within org file content (not just headlines).

**Parameters:**

- `query` (string): Text to search for
- `files` (array of strings, optional): Files to search
- `includeArchived` (boolean, optional): Include archived sections (default: false)
- `maxResults` (number, optional): Limit number of results

**Returns:** Array of matches with context and uri to containing headline (both path-based and ID-based when available)


#### org-schedule-task

Set or update scheduling/deadline for a headline.

**Parameters:**

- `uri` (string): URI of the headline
- `scheduled` (string, optional): Scheduled date in org format (e.g., "2024-12-25")
- `deadline` (string, optional): Deadline date in org format
- `removeScheduled` (boolean, optional): Remove existing scheduled date
- `removeDeadline` (boolean, optional): Remove existing deadline

**Returns:** Success status and updated scheduling info

#### org-get-properties

Extract property drawer contents for a headline.

**Parameters:**

- `uri` (string): URI of the headline
- `properties` (array of strings, optional): Specific properties to retrieve

**Returns:** Object with property key-value pairs



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


### Resource URI Hierarchy

```
org://projects.org                   → Raw org file content
org://projects.org/outline           → Structural view of document
org://projects.org/headline/Alpha    → Specific section with content
```

## Implementation Roadmap

### Completed
- ✅ Basic org-mcp.el structure with security validation
- ✅ `org-get-todo-config` tool
- ✅ `org-get-tag-config` tool
- ✅ `org-update-todo-state` tool
- ✅ `org-add-todo` tool with ID generation
- ✅ Basic resources (file, outline, headline, ID-based)
- ✅ `org-rename-headline` tool (bonus feature)

### Remaining Work

1. **Phase 3: Complete Read Operations**
   - Implement `org-search-headlines` tool
   - Implement `org-get-todos` tool
   - Implement `org-search-content` tool
   - Implement `org-get-properties` tool
   - Add cross-referencing between tools and resources

2. **Phase 4: Additional Write Operations**
   - Implement `org-schedule-task` tool
   - Property modifications

3. **Phase 5: Future Development**
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

- `uri` (string): URI of the section to export
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

**Returns:** Array of agenda items sorted by date with uri


### Advanced Features

- Org-roam integration for linked notes
- Capture template execution
- Refiling capabilities
- Archive operations
- Performance optimizations (caching, streaming)
- Bulk operations for TODO updates
- Complex query language for advanced filtering

