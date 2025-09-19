# org-edit-body Tool Implementation Plan (TDD Process)

## Overview
Implement an MCP tool to edit Org node body contents using partial string replacement, similar to the Edit tool's behavior.

## Function Signature
```elisp
(defun org-mcp--tool-edit-body
    (resourceUri oldBody newBody &optional replaceAll)
  "Edit body content of an Org node using partial string replacement.
RESOURCEURI is the URI of the node to edit.
OLDBODY is the substring to search for within the node's body.
NEWBODY is the replacement text.
REPLACEALL if true, replace all occurrences (default: nil).

MCP Parameters:
  resourceUri - URI of the node (org-headline:// or org-id://)
  oldBody - Substring to replace within the body (must be unique unless replaceAll)
  newBody - Replacement text (must differ from oldBody)
  replaceAll - Replace all occurrences (optional, default false)")
```

## Implementation Steps Following TDD

### Step 1: Initial Setup
- Run `./check.sh` to ensure clean baseline

### Step 2: Documentation
- Update README.org with org-edit-body tool description
- Document parameters and usage examples
- Run `./check.sh` to format/lint documentation

### Step 3: Test - Basic Single-Line Body Edit
- Write test: `org-mcp-test-edit-body-single-line`
- Test replacing a unique substring in body content
- Verify replacement is made correctly
- Ensure properties and headlines are preserved
- Run `./check.sh` to observe test failure

### Step 4: Minimal Implementation
- Add `org-mcp--tool-edit-body` function
- Extract body content between metadata and next headline
- Perform simple string replacement
- Save file and return result
- Run `./check.sh` to confirm test passes

### Step 5: Test - Multi-line Body Replacement
- Write test: `org-mcp-test-edit-body-multiline`
- Test replacing multi-line strings
- Verify exact whitespace preservation
- Run `./check.sh`, implement, verify

### Step 6: Test - Multiple Occurrences Error
- Write test: `org-mcp-test-edit-body-multiple-without-replaceall`
- Test error when oldBody appears multiple times without replaceAll
- Verify appropriate error message
- Run `./check.sh`, implement occurrence counting and validation

### Step 7: Test - ReplaceAll Functionality
- Write test: `org-mcp-test-edit-body-replace-all`
- Test replacing all occurrences when replaceAll is true
- Verify all instances are replaced
- Run `./check.sh`, implement replaceAll logic

### Step 8: Test - Text Not Found Error
- Write test: `org-mcp-test-edit-body-not-found`
- Test error when oldBody doesn't exist in body
- Verify appropriate error message
- Run `./check.sh`, implement not-found validation

### Step 9: Test - Empty Body Handling
- Write test: `org-mcp-test-edit-body-empty`
- Test nodes with no body content
- Verify appropriate error handling
- Run `./check.sh`, implement empty body handling

### Step 10: Test - Nested Headlines Preservation
- Write test: `org-mcp-test-edit-body-nested-headlines`
- Test that child headlines are not affected
- Body edit stops before first child
- Run `./check.sh`, implement proper boundary detection

### Step 11: Test - Properties Drawer Preservation
- Write test: `org-mcp-test-edit-body-properties-preservation`
- Ensure properties are skipped correctly
- Body starts after metadata
- Run `./check.sh`, verify metadata skipping works

### Step 12: Test - Reject newBody with Headlines
- Write test: `org-mcp-test-edit-body-reject-headline-markers`
- Test that newBody containing `\n*` or `\n**` etc. is rejected
- Verify error message about preserving structure
- Run `./check.sh`, implement validation to prevent outline corruption

### Step 13: Refactoring
- Extract common body extraction logic into `org-mcp--extract-node-body`
- Create `org-mcp--replace-node-body` helper
- Apply DRY principle for shared validation
- Reuse existing helper functions
- Run `./check.sh` after each refactoring

### Step 13: Tool Registration
- Add tool registration in `org-mcp-enable`
- Add tool unregistration in `org-mcp-disable`
- Run `./check.sh`

### Step 14: Final Verification
- Run final `./check.sh` to ensure all tests pass
- Review code for any missed edge cases

### Step 15: Commit
- Stage files individually (not with git add -A)
- Create commit with descriptive message

## Test Structure Template
```elisp
(ert-deftest org-mcp-test-edit-body-[scenario] ()
  "Test org-edit-body tool [specific scenario]."
  (org-mcp-test--with-temp-org-file test-file "* Test Heading\n:PROPERTIES:\n:ID: test-id\n:END:\nOriginal body content"
    (let ((org-mcp-allowed-files (list test-file)))
      ;; Call the tool
      (let ((result (org-mcp-test--call-edit-body
                     "org-id://test-id"
                     "Original body"
                     "New body"
                     nil)))
        ;; Assert results
        (should (equal (alist-get 'success result) t))
        ;; Verify file content
        (org-mcp-test--verify-file-content
         test-file
         "New body content")))))
```

## Example Usage
For an Org node like:
```org
* TODO Implement new feature :work:
:PROPERTIES:
:ID: abc-123
:END:
This is a placeholder.
Need to document the approach.
```

To edit the body:
```json
{
  "tool": "org-edit-body",
  "arguments": {
    "resourceUri": "org-id://abc-123",
    "oldBody": "This is a placeholder.",
    "newBody": "Implementation started - using Strategy pattern."
  }
}
```

## Error Messages to Implement
- "Body text not found: [substring]" - oldBody not in node body
- "Body text appears N times (use replaceAll for multiple)" - non-unique without replaceAll
- "Old and new body text must be different" - validation error
- "Node has no body content" - if body is empty
- "New body cannot contain headline markers (*, **, etc.)" - prevent structure corruption

## Key Implementation Details

### Body Extraction Logic
1. Navigate to headline
2. Call `org-end-of-meta-data t` to skip properties/metadata
3. Mark start position
4. Find end: look for next headline at same/higher level or use `org-end-of-subtree`
5. Extract content between positions

### Validation Steps
1. Check oldBody != newBody
2. Count occurrences of oldBody
3. Validate based on replaceAll flag
4. Preserve exact whitespace

### Helper Functions
- `org-mcp--extract-node-body`: Get body content of a node
- `org-mcp--replace-node-body`: Replace body content
- Reuse: `org-mcp--parse-resource-uri`, `org-mcp--find-headline-by-path`, `org-mcp--check-buffer-modifications`, `org-mcp--refresh-file-buffers`

## Success Criteria
- All tests pass
- Body edits work with partial string matching
- Properties and structure are preserved
- Error messages are clear and helpful
- Code follows DRY principle
- Documentation is complete