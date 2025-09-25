# Technical Specifications and Design Requirements for an Emacs Org-mode MCP Server

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
