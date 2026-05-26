;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org-mcp)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-ert)
(require 'json)

(setq mcp-server-lib-ert-server-id "org-mcp")

;;; Test Data Constants

;; Initial content strings for various test scenarios

(defconst org-mcp-test--content-empty ""
  "Empty org file content.")

(defconst org-mcp-test--content-with-id-id
  "550e8400-e29b-41d4-a716-446655440000"
  "ID value for org-mcp-test--content-with-id.")

(defconst org-mcp-test--content-with-id-uri
  (format "org-id://%s" org-mcp-test--content-with-id-id)
  "URI for org-mcp-test--content-with-id.")

(defconst org-mcp-test--content-nested-siblings-parent-id
  "nested-siblings-parent-id-002"
  "ID for Parent Task in org-mcp-test--content-nested-siblings.")

(defconst org-mcp-test--content-nested-siblings
  (format
   "#+TITLE: My Org Document

* Parent Task
:PROPERTIES:
:ID:       %s
:END:
Some parent content.
** First Child 50%% Complete
First child content.
It spans multiple lines.
** Second Child
:PROPERTIES:
:ID:       %s
:END:
Second child content.
** Third Child #3"
   org-mcp-test--content-nested-siblings-parent-id
   org-mcp-test--content-with-id-id)
  "Parent with multiple child tasks and doc file header.")

(defconst org-mcp-test--level2-parent-level3-sibling-id
  "level2-parent-level3-sibling-id-001"
  "ID for Review org-mcp.el in level2-parent-level3-children.")

(defconst org-mcp-test--content-level2-parent-level3-children
  (format
   "* Top Level
** Review the package
*** Review org-mcp.el
:PROPERTIES:
:ID:       %s
:END:
Main package file"
   org-mcp-test--level2-parent-level3-sibling-id)
  "Level 2 parent with level 3 children - matches emacs.org structure.")

(defconst org-mcp-test--content-simple-todo
  "* TODO Original Task
First line of body.
Second line of body.
Third line of body."
  "Simple TODO task with three-line body.")

(defconst org-mcp-test--content-with-id-todo
  (format
   "* TODO Task with ID
:PROPERTIES:
:ID:       %s
:END:
First line of content.
Second line of content.
Third line of content."
   org-mcp-test--content-with-id-id)
  "Task with an Org ID property, TODO state, and multiline content.")


(defconst org-mcp-test--timestamp-id "20240101T120000"
  "Timestamp-format ID value.")

(defconst org-mcp-test--content-timestamp-id
  (format
   "* TODO Task with timestamp ID
:PROPERTIES:
:ID:       %s
:END:
Task content."
   org-mcp-test--timestamp-id)
  "Task with a timestamp-format ID property.")

(defconst org-mcp-test--content-with-id-no-body
  (format
   "* TODO Task with ID but no body
:PROPERTIES:
:ID:       %s
:END:"
   org-mcp-test--timestamp-id)
  "Task with an ID property but no body content.")

(defconst org-mcp-test--body-text-multiline
  (concat
   "This is the body text.\n"
   "It has multiple lines.\n"
   "With some content.")
  "Multi-line body text for testing TODO items with content.")

(defconst org-mcp-test--content-wrong-levels
  "* First Parent
Some content in first parent.
* Second Parent
** Other Child
*** Target Headline
This should NOT be found via First Parent/Target Headline path.
* Third Parent
** Target Headline
This is actually a child of Third Parent, not First Parent!"
  "Test content with same headline names at different levels.")

(defconst org-mcp-test--content-todo-with-tags
  "* TODO Task with Tags :work:urgent:\nTask description."
  "TODO task with tags and body.")

(defconst org-mcp-test--content-slash-not-nested-before
  "* Parent
** Real Child
Content here.
* Parent/Child
This is a single headline with a slash, not nested under Parent."
  "Content with Parent having a child and separate Parent/Child headline.")

(defconst org-mcp-test--content-with-id-repeated-text
  "* Test Heading
:PROPERTIES:
:ID: test-id
:END:
First occurrence of pattern.
Some other text.
Second occurrence of pattern.
More text.
Third occurrence of pattern."
  "Heading with ID and repeated text patterns.")

(defconst org-mcp-test--content-malformed-header-with-todo-and-body
  "#+TITLE: Test Document
:PROPERTIES:
:CATEGORY: work
* TODO Existing Heading
Some body content."
  "File with an unterminated `:PROPERTIES:' drawer plus a TODO heading
with body content.")

(defconst org-mcp-test--content-malformed-header-logbook-unterminated
  "#+TITLE: Test Document
:LOGBOOK:
CLOCK: [2026-01-01 Mon 09:00]--[2026-01-01 Mon 10:00] =>  1:00
* Existing Heading
Some content."
  "File with an unterminated `:LOGBOOK:' drawer in its header.  Used
to verify that any drawer keyword (not just `:PROPERTIES:') is
recognised by the file-header walker.")

(defconst org-mcp-test--content-malformed-header-heading-in-drawer
  "#+TITLE: Test Document
:PROPERTIES:
:CATEGORY: work
* Heading Inside Drawer
* Real Heading"
  "File with a heading line trapped inside an unterminated
`:PROPERTIES:' drawer in the header block.")

(defconst org-mcp-test--content-parent-child-then-other-top
  "* Parent Task
Parent body.
** Existing Child
* Other Top"
  "Parent with one child followed by another top-level heading.")

(defconst org-mcp-test--content-with-id-case-mixed-body
  (format
   "* TODO Task with mixed-case body
:PROPERTIES:
:ID:       %s
:END:
hello world
Hello world"
   org-mcp-test--content-with-id-id)
  "Task with body containing both `hello world' (lowercase, first)
and `Hello world' (capitalized, second).")

(defconst org-mcp-test--content-duplicate-headlines-before
  "* Team Updates
** Project Review
First review content.
* Development Tasks
** Project Review
Second review content.
* Planning
** Project Review
Third review content."
  "Content with duplicate 'Project Review' headlines under different parents.")

(defconst org-mcp-test--content-hierarchy-before
  "* First Section
** Target
Some content.
* Second Section
** Other Item
More content.
** Target
This Target is under Second Section, not First Section."
  "Content with duplicate 'Target' headlines under different parents.")

(defconst org-mcp-test--content-todo-keywords-before
  "* Project Management
** TODO Review Documents
This task needs to be renamed
** DONE Review Code
This is already done"
  "Parent with TODO and DONE children for testing keyword handling.")

;; Expected patterns and validation regexes
;;
;; Note on property drawer patterns: The patterns use ` *` (zero or more
;; spaces) before :PROPERTIES:, :ID:, and :END: lines to maintain compatibility
;; across Emacs versions. Emacs 27.2 indents property drawers with 3 spaces,
;; while Emacs 28+ does not add indentation.

(defconst org-mcp-test--expected-parent-task-from-nested-siblings
  (format
   "* Parent Task
:PROPERTIES:
:ID:       nested-siblings-parent-id-002
:END:
Some parent content.
** First Child 50%% Complete
First child content.
It spans multiple lines.
** Second Child
:PROPERTIES:
:ID:       %s
:END:
Second child content.
** Third Child #3"
   org-mcp-test--content-with-id-id)
  "Expected content when extracting Parent Task from nested-siblings.")

(defconst org-mcp-test--regex-after-sibling-level3
  (concat "\\`\\* Top Level\n"
          "\\*\\* Review the package\n"
          "\\*\\*\\* Review org-mcp\\.el\n"
          " *:PROPERTIES:\n"
          " *:ID: +" org-mcp-test--level2-parent-level3-sibling-id "\n"
          " *:END:\n"
          "Main package file\n"
          "\\*\\*\\* TODO Review org-mcp-test\\.el +.*:internet:.*\n"
          " *:PROPERTIES:\n"
          " *:ID: +[a-fA-F0-9-]+\n"
          " *:END:\n\\'")
  "Expected pattern after adding TODO after level 3 sibling.")

(defconst org-mcp-test--regex-after-sibling-level3-with-body
  (concat "\\`\\* Top Level\n"
          "\\*\\* Review the package\n"
          "\\*\\*\\* Review org-mcp\\.el\n"
          " *:PROPERTIES:\n"
          " *:ID: +" org-mcp-test--level2-parent-level3-sibling-id "\n"
          " *:END:\n"
          "Main package file\n"
          "\\*\\*\\* TODO Review org-mcp-test\\.el +.*:internet:.*\n"
          " *:PROPERTIES:\n"
          " *:ID: +[a-fA-F0-9-]+\n"
          " *:END:\n"
          (regexp-quote org-mcp-test--body-text-multiline)
          "\n\\'")
  "Whole-file pattern after a body-bearing child insert at EOF when the
fixture has no trailing newline.")

(defconst org-mcp-test--expected-regex-renamed-second-child
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    ":PROPERTIES:\n"
    ":ID: +nested-siblings-parent-id-002\n"
    ":END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Renamed Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\\'")
   org-mcp-test--content-with-id-id)
  "Regex matching complete buffer after renaming Second Child.")

(defconst org-mcp-test--expected-regex-todo-to-in-progress-with-id
  (format
   (concat
    "\\`"
    "\\* IN-PROGRESS Task with ID\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "First line of content\\.\n"
    "Second line of content\\.\n"
    "Third line of content\\."
    "\\'")
   org-mcp-test--content-with-id-id)
  "Expected regex for TODO to IN-PROGRESS state change with ID.")

(defconst org-mcp-test--expected-timestamp-id-done-regex
  (concat
   "\\`\\* DONE Task with timestamp ID"
   "\n:PROPERTIES:\n:ID:[ \t]+[A-Za-z0-9-]+\n:END:"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer after updating timestamp ID task to DONE.")

(defconst org-mcp-test--expected-task-with-id-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task with ID"
   "\n:PROPERTIES:\n:ID:[ \t]+[A-Fa-f0-9-]+\n:END:"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task with ID in IN-PROGRESS state.")

(defconst org-mcp-test--regex-id-drawer
  " *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n"
  "Regex matching an `org-id-get-create'-style ID property drawer.")

(defconst org-mcp-test--expected-regex-top-level-with-header
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "\n"
   "\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-nested-siblings-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Regex matching complete buffer after adding top-level TODO with headers.
Locks the documented \"end of file\" placement for top-level inserts
into a file that already contains headings.")

(defconst org-mcp-test--regex-child-under-parent
  (format
   (concat
    "^\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n"
    "\\*\\* TODO Child Task +.*:work:.*\n"
    org-mcp-test--regex-id-drawer)
   org-mcp-test--content-with-id-id)
  "Pattern for child TODO (level 2) added under parent (level 1) with existing child (level 2).")

(defconst org-mcp-test--regex-child-end-no-blank-before-other-top
  (concat
   "\\`\\* Parent Task\n"
   "Parent body\\.\n"
   "\\*\\* Existing Child\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\* Other Top\\'")
  "Pattern asserting that a child appended at end of parent appears
between `** Existing Child' and `* Other Top' with no blank line
before `* Other Top'.")

(defconst org-mcp-test--regex-second-child-same-level
  (concat
   "\\`\\* Top Level\n"
   "\\*\\* Review the package\n"
   "\\*\\*\\* Review org-mcp\\.el\n"
   org-mcp-test--regex-id-drawer
   "Main package file\n"
   "\\*\\*\\* TODO Second Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern for second child (level 3) added at same level as first child (level 3) under parent (level 2).")

(defconst org-mcp-test--regex-todo-with-body
  (concat
   "\\`\\* TODO Task with Body +:[^\n]*\n"
   org-mcp-test--regex-id-drawer
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n\\'")
  "Pattern for TODO with body text.")

(defconst org-mcp-test--regex-todo-with-literal-block-end
  (concat
   "\\`\\* TODO Task with literal END_SRC +:work:\n"
   org-mcp-test--regex-id-drawer
   "Example of source block:\n"
   "#\\+BEGIN_EXAMPLE\n"
   "#\\+END_SRC\n"
   "#\\+END_EXAMPLE\n"
   "Text after\\.\n\\'")
  "Pattern for TODO with body containing literal END_SRC inside EXAMPLE block.")

(defconst org-mcp-test--regex-todo-after-sibling
  (concat
   "^#\\+TITLE: My Org Document\n\n"
   "\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-nested-siblings-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   ":PROPERTIES:\n"
   ":ID: +[^\n]+\n"
   ":END:\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n\n?"
   "\\*\\* TODO New Task After First +:[^\n]*\n"
   org-mcp-test--regex-id-drawer
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\\'")
  "Pattern for TODO added after specific sibling.")

(defconst org-mcp-test--regex-todo-after-second-child
  (concat
   "\\`#\\+TITLE: My Org Document\n\n"
   "\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-nested-siblings-parent-id "\n"
   ":END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   ":PROPERTIES:\n"
   ":ID: +" org-mcp-test--content-with-id-id "\n"
   ":END:\n"
   "Second child content\\.\n\n?"
   "\\*\\* TODO New Task After Second +:[^\n]*\n"
   org-mcp-test--regex-id-drawer
   "\\*\\* Third Child #3\\'")
  "Pattern for TODO added after Second Child sibling.")

(defconst org-mcp-test--regex-todo-without-tags
  (concat
   "\\`\\* TODO Task Without Tags\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern for TODO item without any tags.")

(defconst org-mcp-test--regex-top-level-todo
  (concat
   "\\`\\* TODO New Task +:.*work.*urgent.*:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern for top-level TODO item with work and urgent tags.")

(defconst org-mcp-test--regex-todo-tag-accept-valid
  (concat
   "\\`\\* TODO ValidTask +:work:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern for TODO with a single `work' tag drawn from `org-tag-alist'.")

(defconst org-mcp-test--regex-todo-tag-validation-without-alist
  (concat
   "\\`\\* TODO Task1 +:"
   ".*validtag.*tag123.*my_tag.*@home.*:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern for TODO with multiple valid tags accepted without `org-tag-alist'.")

(defconst org-mcp-test--pattern-add-todo-parent-id-uri
  (concat
   "\\`#\\+TITLE: My Org Document\n\n"
   "\\* Parent Task\n"
   org-mcp-test--regex-id-drawer
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   org-mcp-test--regex-id-drawer
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n"
   "\\*\\* TODO Child via ID +:work:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern for TODO added via parent ID URI.")

(defconst org-mcp-test--pattern-renamed-simple-todo
  (concat
   "\\`\\* TODO Updated Task\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "First line of body\\.\n"
   "Second line of body\\.\n"
   "Third line of body\\.\\'")
  "Pattern for renamed simple TODO with generated ID.")

(defconst org-mcp-test--pattern-renamed-todo-with-tags
  (concat
   "\\`\\* TODO Renamed Task[ \t]+:work:urgent:\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "Task description\\.\\'")
  "Pattern for renamed TODO task preserving tags.")

(defconst org-mcp-test--pattern-renamed-headline-no-todo
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Some parent content\\.\n"
    "\\*\\* Updated Child\n"
    " *:PROPERTIES:\n"
    " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
    " *:END:\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for renamed headline without TODO state.")

(defconst org-mcp-test--pattern-renamed-headline-with-id
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Renamed Child\n"
    " *:PROPERTIES:\n"
    " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
    " *:END:\n?\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for headline renamed with ID creation.")

(defconst org-mcp-test--pattern-renamed-slash-headline
  (concat
   "\\`\\* Parent\n"
   "\\*\\* Real Child\n"
   "Content here\\.\n"
   "\\* Parent/Child Renamed\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This is a single headline with a slash, not nested under Parent\\.\\'")
  "Pattern for renamed headline containing slash character.")

(defconst org-mcp-test--regex-slash-not-nested-after
  (concat
   "\\`\\* Parent\n"
   "\\*\\* Real Child\n"
   "Content here\\.\n"
   "\\* Parent-Child Renamed\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This is a single headline with a slash, not nested under Parent\\.\\'")
  "Regex for slash-not-nested test after renaming Parent/Child.")

(defconst org-mcp-test--regex-percent-after
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 75%% Complete\n"
    " *:PROPERTIES:\n"
    " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
    " *:END:\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\\'")
   org-mcp-test--content-nested-siblings-parent-id
   org-mcp-test--content-with-id-id)
  "Expected pattern after renaming headline with percent sign.")

(defconst org-mcp-test--regex-duplicate-first-renamed
  (concat
   "\\`\\* Team Updates\n"
   "\\*\\* Q1 Review\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "First review content\\.\n"
   "\\* Development Tasks\n"
   "\\*\\* Project Review\n"
   "Second review content\\.\n"
   "\\* Planning\n"
   "\\*\\* Project Review\n"
   "Third review content\\.\\'")
  "Regex for duplicate headlines after renaming first occurrence.")

(defconst org-mcp-test--regex-hierarchy-second-target-renamed
  (concat
   "\\`\\* First Section\n"
   "\\*\\* Target\n"
   "Some content\\.\n"
   "\\* Second Section\n"
   "\\*\\* Other Item\n"
   "More content\\.\n"
   "\\*\\* Renamed Target\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This Target is under Second Section, not First Section\\.\\'")
  "Regex for hierarchy test after renaming second Target.")

(defconst org-mcp-test--regex-add-todo-with-mutex-tags
  (concat
   "\\`#\\+TITLE: Test Org File\n"
   "\n"
   "\\* TODO Test Task[ \t]+\\(:[^:\n]+\\)+:\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n?\\'")
  "Regex for add-todo test accepting any tag order.")

(defconst org-mcp-test--regex-todo-keywords-after
  (concat
   "\\`\\* Project Management\n"
   "\\*\\* TODO Q1 Planning Review\n"
   " *:PROPERTIES:\n"
   " *:ID:[ \t]+[A-Fa-f0-9-]+\n"
   " *:END:\n"
   "This task needs to be renamed\n"
   "\\*\\* DONE Review Code\n"
   "This is already done\\'")
  "Regex for todo-keywords test after renaming TODO headline.")

(defconst org-mcp-test--pattern-edit-body-single-line
  (format (concat
           "\\`#\\+TITLE: My Org Document\n"
           "\n"
           "\\* Parent Task\n"
           ":PROPERTIES:\n"
           ":ID: +nested-siblings-parent-id-002\n"
           ":END:\n"
           "Some parent content\\.\n"
           "\\*\\* First Child 50%% Complete\n"
           "First child content\\.\n"
           "It spans multiple lines\\.\n"
           "\\*\\* Second Child\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "Updated second child content\\.\n"
           "\\*\\* Third Child #3\n"
           "?\\'")
          org-mcp-test--content-with-id-id)
  "Pattern for single-line edit-body test result.")

(defconst org-mcp-test--pattern-edit-body-multiline
  (format (concat
           "\\`\\* TODO Task with ID\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "First line of content\\.\n"
           "This has been replaced\n"
           "with new multiline\n"
           "content here\\.\n"
           "Third line of content\\.\n"
           "?\\'")
          org-mcp-test--content-with-id-id)
  "Pattern for multiline edit-body test result.")

(defconst org-mcp-test--pattern-edit-body-case-sensitive
  (format (concat
           "\\`\\* TODO Task with mixed-case body\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "hello world\n"
           "REPLACED\n?\\'")
          org-mcp-test--content-with-id-id)
  "Pattern for case-sensitive edit-body: replaces `Hello world' (capitalized)
while leaving `hello world' (lowercase) untouched.")

(defconst org-mcp-test--pattern-edit-body-replace-all
  (concat
   "\\`\\* Test Heading\n"
   ":PROPERTIES:\n"
   ":ID: +test-id\n"
   ":END:\n"
   "First REPLACED\\.\n"
   "Some other text\\.\n"
   "Second REPLACED\\.\n"
   "More text\\.\n"
   "Third REPLACED\\.\n"
   "?\\'")
  "Pattern for replace-all edit-body test result.")

(defconst org-mcp-test--pattern-edit-body-nested-headlines
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Updated parent content\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n?"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for nested headlines edit-body test result.  The first
child gains no `:ID:' drawer; `org-id-get-create' resolves to the
edit target (`* Parent Task', which already has one).")

(defconst org-mcp-test--pattern-edit-body-empty
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Some parent content\\.\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\n"
    " *:PROPERTIES:\n *:ID:[ \t]+[A-Fa-f0-9-]+\n *:END:\n"
    "New content added\\.\n"
    "\\'")
   org-mcp-test--content-with-id-id)
  "Whole-file regex for `edit-body-empty': new body added to
`** Third Child #3' as the file's last subtree.  An auto-generated
`:ID:' drawer attaches to that heading; the body is on its own line
after `:END:'; the file ends with a trailing newline.")

(defconst org-mcp-test--pattern-edit-body-empty-with-props
  (format (concat
           "\\`\\* TODO Task with ID but no body\n"
           " *:PROPERTIES:\n"
           " *:ID: +%s\n"
           " *:END:\n"
           "Content added after properties\\.\n"
           "\\'")
          org-mcp-test--timestamp-id)
  "Whole-file regex for `edit-body-empty-with-properties': new body
added under a heading whose only metadata is its `:ID:' drawer.
The drawer is preserved verbatim (no orphaned-ID cascade); the
body is on its own line after `:END:'; the file ends with a
trailing newline.")

(defconst org-mcp-test--pattern-edit-body-empty-with-deeper-heading
  (format (concat
           "\\`\\* TODO Task with ID but no body\n"
           " *:PROPERTIES:\n"
           " *:ID: +%s\n"
           " *:END:\n"
           "intro text\n"
           "\\*\\* Sub heading\n?\\'")
          org-mcp-test--timestamp-id)
  "Whole-file regex for empty-body edit with new content containing
a deeper heading.  The deeper `** Sub heading' carries no `:ID:'
drawer; the edit target's existing `:ID:' is preserved and returned
as the URI.")

(defconst org-mcp-test--pattern-edit-body-accept-lower-level
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "\n"
   "\\* Parent Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +nested-siblings-parent-id-002\n"
   " *:END:\n"
   "Some parent content\\.\n"
   "\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   " *:PROPERTIES:\n"
   " *:ID: +"
   org-mcp-test--content-with-id-id
   "\n"
   " *:END:\n"
   "some text\n"
   "\\*\\*\\* Subheading content\n"
   "\\*\\* Third Child #3\n?\\'")
  "Whole-file regex for edit-body accepting lower-level headlines.
The body-internal `*** Subheading content' heading carries no
`:ID:' drawer; `org-id-get-create' attaches `:ID:' to the edit
target (`** Second Child', which already has one), not to the
deeper body-internal heading.")

(defconst org-mcp-test--pattern-tool-read-headline-single
  (concat
   "\\`\\* Parent/Child\n"
   "This is a single headline with a slash, not nested under Parent\\.\n"
   "?\\'")
  "Pattern for org-read-headline tool single-level path result.")

(defconst org-mcp-test--pattern-tool-read-headline-nested
  (concat
   "\\`\\*\\* First Child 50% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "?\\'")
  "Pattern for org-read-headline tool nested path result.")

(defconst org-mcp-test--pattern-tool-read-by-id
  (format
   (concat
    "\\`\\*\\* Second Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Second child content\\.\n"
    "?\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for org-read-by-id tool result.")

(defconst org-mcp-test--content-id-resource-id
  "12345678-abcd-efgh-ijkl-1234567890ab"
  "ID value for org-mcp-test--content-id-resource.")

(defconst org-mcp-test--content-id-resource
  (format
   "* Section with ID
:PROPERTIES:
:ID: %s
:END:
Content of section with ID."
   org-mcp-test--content-id-resource-id)
  "Content for ID resource tests.")

(defconst org-mcp-test--content-headline-resource
  "* First Section
Some content in first section.
** Subsection 1.1
Content of subsection 1.1.
** Subsection 1.2
Content of subsection 1.2.
* Second Section
Content of second section.
*** Deep subsection
Very deep content."
  "Test content with hierarchical headlines for resource read tests.")

(defconst org-mcp-test--expected-first-section
  (concat
   "* First Section\n"
   "Some content in first section.\n"
   "** Subsection 1.1\n"
   "Content of subsection 1.1.\n"
   "** Subsection 1.2\n"
   "Content of subsection 1.2.")
  "Expected content when reading 'First Section' top-level headline.")

(defconst org-mcp-test--expected-subsection-1-1
  (concat
   "** Subsection 1.1\n"
   "Content of subsection 1.1.")
  "Expected content when reading 'First Section/Subsection 1.1' nested headline.")

;; Test helpers

(defun org-mcp-test--read-file (file)
  "Read and return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-mcp-test--verify-file-matches (test-file expected-pattern)
  "Verify TEST-FILE content matches EXPECTED-PATTERN regexp."
  (should (string-match-p expected-pattern (org-mcp-test--read-file test-file))))

(defmacro org-mcp-test--assert-error-and-file (test-file error-form)
  "Assert that ERROR-FORM throws an error and TEST-FILE remains unchanged."
  (declare (indent 1) (debug t))
  `(let ((original-content (org-mcp-test--read-file ,test-file)))
     (should-error ,error-form :type 'mcp-server-lib-tool-error)
     (should (string= (org-mcp-test--read-file ,test-file) original-content))))

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (mcp-server-lib-ert-with-server :tools t :resources t ,@body)
       (org-mcp-disable))))

(defmacro org-mcp-test--with-temp-org-files (file-specs &rest body)
  "Create temporary Org files, execute BODY, and ensure cleanup.
FILE-SPECS is a list of file specifications.
Each spec is (VAR CONTENT [FILENAME-PREFIX]).
VAR is the variable to bind the temp file path to.
CONTENT is the initial content to write to the file.
FILENAME-PREFIX is optional, defaults to \"org-mcp-test\".
All created files are automatically added to `org-mcp-allowed-files'.
BODY is executed with org-mcp enabled."
  (declare (indent 1))
  (let* ((vars (mapcar #'car file-specs))
         (temp-vars (mapcar (lambda (v) (gensym (symbol-name v)))
                            vars))
         (bindings (cl-mapcar
                    (lambda (var temp-var)
                      `(,var ,temp-var))
                    vars temp-vars))
         (inits (cl-mapcar
                 (lambda (temp-var spec)
                   (let ((content (nth 1 spec))
                         (filename (or (nth 2 spec) "org-mcp-test")))
                     `(setq ,temp-var
                            (make-temp-file ,filename nil ".org" ,content))))
                 temp-vars file-specs))
         (cleanups (mapcar
                    (lambda (temp-var)
                      `(when ,temp-var
                         (delete-file ,temp-var)))
                    temp-vars)))
    `(let (,@temp-vars)
       (unwind-protect
           (progn
             ,@inits
             (let (,@bindings
                   (org-mcp-allowed-files (list ,@temp-vars)))
               (org-mcp-test--with-enabled
                 ,@body)))
         ,@cleanups))))

(defmacro org-mcp-test--with-id-tracking
    (allowed-files id-locations &rest body)
  "Set up org-id tracking with ID-LOCATIONS and run BODY.
ALLOWED-FILES is the list of files to bind to `org-mcp-allowed-files'.
ID-LOCATIONS is a list of (ID . FILE) cons cells to register.
Sets up `org-id-track-globally' and `org-id-locations-file',
then registers each ID location."
  (declare (indent 2) (debug t))
  `(let ((org-id-track-globally t)
         (org-id-locations-file nil) ; Prevent saving to disk
         (org-id-locations nil)
         (org-mcp-allowed-files ,allowed-files))
     (dolist (id-loc ,id-locations)
       (org-id-add-location (car id-loc) (cdr id-loc)))
     ,@body))

(defmacro org-mcp-test--with-id-setup (file-var initial-content ids &rest body)
  "Create temp file, set up org-id tracking with IDS, run BODY.
FILE-VAR is the variable to bind the temp file path to.
INITIAL-CONTENT is the initial content to write to the file.
IDS is a list of ID strings to register.
Sets up `org-id-track-globally' and `org-id-locations-file',
then registers each ID location and enables MCP for BODY.
The created temp file is automatically added to `org-mcp-allowed-files'."
  (declare (indent 2) (debug t))
  `(org-mcp-test--with-temp-org-files
    ((,file-var ,initial-content))
    (org-mcp-test--with-id-tracking
     (list ,file-var)
     (mapcar (lambda (id) (cons id ,file-var)) ,ids)
     ,@body)))

(defmacro org-mcp-test--with-file-buffer (buffer file &rest body)
  "Open FILE in BUFFER and execute BODY, ensuring buffer is killed.
BUFFER is the variable name to bind the buffer to.
FILE is the file path to open.
BODY is the code to execute with the buffer."
  (declare (indent 2) (debug t))
  `(let ((,buffer (find-file-noselect ,file)))
     (unwind-protect
         (progn ,@body)
       (kill-buffer ,buffer))))

;; Helpers for testing org-get-todo-config MCP tool

(defun org-mcp-test--check-todo-config-sequence
    (seq expected-type expected-keywords)
  "Check sequence SEQ has EXPECTED-TYPE and EXPECTED-KEYWORDS."
  (should (= (length seq) 2))
  (should (equal (alist-get 'type seq) expected-type))
  (should (equal (alist-get 'keywords seq) expected-keywords)))

(defun org-mcp-test--check-todo-config-semantic
    (sem expected-state expected-final expected-type)
  "Check semantic SEM properties.
EXPECTED-STATE is the TODO keyword.
EXPECTED-FINAL is whether it's a final state.
EXPECTED-TYPE is the sequence type."
  (should (= (length sem) 3))
  (should (equal (alist-get 'state sem) expected-state))
  (should (equal (alist-get 'isFinal sem) expected-final))
  (should (equal (alist-get 'sequenceType sem) expected-type)))

(defmacro org-mcp-test--with-get-todo-config-result (keywords &rest body)
  "Call get-todo-config tool with KEYWORDS and run BODY with result bindings.
Sets `org-todo-keywords' to KEYWORDS, calls the get-todo-config MCP tool,
and binds `sequences' and `semantics' from the result for use in BODY."
  (declare (indent 1) (debug t))
  `(let ((org-todo-keywords ,keywords))
     (org-mcp-test--with-enabled
      (let ((result (json-read-from-string
                     (mcp-server-lib-ert-call-tool "org-get-todo-config" nil))))
        (should (= (length result) 2))
        (let ((sequences (cdr (assoc 'sequences result)))
              (semantics (cdr (assoc 'semantics result))))
          ,@body)))))

;; Helpers for testing org-get-tag-config MCP tool

(defmacro org-mcp-test--get-tag-config-and-check
    (expected-alist expected-persistent expected-inheritance expected-exclude)
  "Call org-get-tag-config tool and check result against expected values.
EXPECTED-ALIST is the expected value for org-tag-alist (string).
EXPECTED-PERSISTENT is the expected value for org-tag-persistent-alist (string).
EXPECTED-INHERITANCE is the expected value for org-use-tag-inheritance (string).
EXPECTED-EXCLUDE is the expected value for
org-tags-exclude-from-inheritance (string)."
  (declare (indent defun) (debug t))
  `(org-mcp-test--with-enabled
    (let ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool "org-get-tag-config" nil))))
      (should (= (length result) 4))
      (should (equal (alist-get 'org-tag-alist result) ,expected-alist))
      (should (equal (alist-get 'org-tag-persistent-alist result)
                     ,expected-persistent))
      (should (equal (alist-get 'org-use-tag-inheritance result)
                     ,expected-inheritance))
      (should (equal (alist-get 'org-tags-exclude-from-inheritance result)
                     ,expected-exclude)))))

;; Helpers for testing org-get-allowed-files MCP tool

(defun org-mcp-test--get-allowed-files-and-check (allowed-files expected-files)
  "Call org-get-allowed-files tool and verify the result.
ALLOWED-FILES is the value to bind to org-mcp-allowed-files.
EXPECTED-FILES is a list of expected file paths."
  (let ((org-mcp-allowed-files allowed-files))
    (org-mcp-test--with-enabled
     (let* ((result-text
             (mcp-server-lib-ert-call-tool "org-get-allowed-files" nil))
            (result (json-read-from-string result-text)))
       (should (= (length result) 1))
       (let ((files (cdr (assoc 'files result))))
         (should (vectorp files))
         (should (= (length files) (length expected-files)))
         (dotimes (i (length expected-files))
           (should (string= (aref files i) (nth i expected-files)))))))))

;; Helper functions for testing org-add-todo MCP tool

(defmacro org-mcp-test--with-add-todo-setup
    (file-var initial-content todo-keywords tag-alist ids &rest body)
  "Helper for org-add-todo test.
Sets up FILE-VAR with INITIAL-CONTENT and org configuration.
TODO-KEYWORDS is the org-todo-keywords config (nil for default).
TAG-ALIST is the org-tag-alist config (nil for default).
IDS is optional list of ID strings to register (nil for no ID tracking).
Executes BODY with org-mcp enabled and standard variables set."
  (declare (indent 2))
  (let ((todo-kw (or todo-keywords ''((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (tag-al (or tag-alist ''("work" "personal" "urgent"))))
    `(org-mcp-test--with-temp-org-files
      ((,file-var ,initial-content))
      (let ((org-todo-keywords ,todo-kw)
            (org-tag-alist ,tag-al)
            (org-id-locations-file nil))
        ,(if ids
             `(org-mcp-test--with-id-tracking
               (list ,file-var)
               (mapcar (lambda (id) (cons id ,file-var)) ,ids)
               ,@body)
           `(progn ,@body))))))

(defun org-mcp-test--build-add-todo-params
    (title todo-state tags body parent-uri &optional after-uri)
  "Build params alist for the `org-add-todo' MCP tool.
TITLE, TODO-STATE, TAGS, BODY, and PARENT-URI populate the
correspondingly-named wire-protocol keys.  AFTER-URI populates
`after_uri'."
  (list (cons 'title title)
        (cons 'todo_state todo-state)
        (cons 'tags tags)
        (cons 'body body)
        (cons 'parent_uri parent-uri)
        (cons 'after_uri after-uri)))

(cl-defmacro org-mcp-test--call-add-todo-expecting-error
    (initial-content title todo-state tags body parent-uri
                     &key todo-keywords tag-alist after-uri)
  "Call org-add-todo MCP tool expecting an error and verify file unchanged.
INITIAL-CONTENT is the initial Org file content.
TITLE is the headline text.
TODO-STATE is the TODO state.
TAGS is a list of tag strings or nil.
BODY is the body text or nil.
PARENT-URI is the URI of the parent item.

Keyword arguments:
:TODO-KEYWORDS is the org-todo-keywords config (nil for default).
:TAG-ALIST is the org-tag-alist config (nil for default).
:AFTER-URI is the URI of a sibling to insert after."
  `(org-mcp-test--with-add-todo-setup
    test-file ,initial-content ,todo-keywords
    ,tag-alist nil
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((params
             (org-mcp-test--build-add-todo-params
              ,title ,todo-state ,tags ,body ,parent-uri ,after-uri))
            (request
              (mcp-server-lib-create-tools-call-request
               "org-add-todo" nil params))
            (response (mcp-server-lib-process-jsonrpc-parsed request
                                                             mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result)))))

(defun org-mcp-test--assert-add-todo-rejects-body-headline
    (initial-content parent-headline body-with-headline)
  "Test that adding TODO with BODY-WITH-HEADLINE is rejected.
INITIAL-CONTENT is the initial file content.
PARENT-HEADLINE is the parent headline path (empty string for top-level).
BODY-WITH-HEADLINE is the body containing invalid headline."
  (org-mcp-test--call-add-todo-expecting-error
   initial-content
   "Test Task" "TODO" '("work") body-with-headline
   (format "org-headline://%s#%s" test-file parent-headline)))

(defun org-mcp-test--assert-add-todo-invalid-title (invalid-title)
  "Assert that adding TODO with INVALID-TITLE throws an error.
Tests that the given title is rejected when creating a TODO."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   invalid-title "TODO" nil nil
   (format "org-headline://%s#" test-file)))

(defun org-mcp-test--assert-add-todo-invalid-tag (invalid-tag)
  "Assert that adding TODO with INVALID-TAG (a single tag string) is rejected.
Binds `org-tag-alist' and `org-tag-persistent-alist' to nil so the
alist-membership path is inactive and the tag-format check fires."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil))
    (org-mcp-test--call-add-todo-expecting-error
     org-mcp-test--content-empty
     "Task" "TODO" (list invalid-tag) nil
     (format "org-headline://%s#" test-file))))

(defun org-mcp-test--assert-add-todo-result-shape (result title basename)
  "Assert RESULT has the standard 4-field `org-add-todo' response shape.
TITLE is the expected `title' field; BASENAME the expected `file'."
  (should (= (length result) 4))
  (should (equal (alist-get 'success result) t))
  (should (string-match-p "\\`org-id://.+" (alist-get 'uri result)))
  (should (equal (alist-get 'file result) basename))
  (should (equal (alist-get 'title result) title)))

(cl-defmacro org-mcp-test--add-todo-and-check
    (initial-content
     title todo-state tags body parent-uri
     basename expected-pattern
     &key todo-keywords tag-alist ids after-uri override-bindings)
  "Add TODO item with setup and verify the result.
INITIAL-CONTENT is the initial Org file content.
TITLE is the headline text.
TODO-STATE is the TODO state.
TAGS is a list of tag strings or nil.
BODY is the body text or nil.
PARENT-URI is the URI of the parent item.
BASENAME is the expected file basename.
EXPECTED-PATTERN is a regexp that the file content should match.

Keyword arguments:
:TODO-KEYWORDS is the org-todo-keywords config (nil for default).
:TAG-ALIST is the org-tag-alist config (nil for default).
:IDS is a list of ID strings to register (nil for no ID tracking).
:AFTER-URI is the URI of a sibling to insert after.
:OVERRIDE-BINDINGS is a list of let-style bindings to override
variables after setup, e.g., ((org-tag-alist nil))."
  (let ((checking-logic
         `(let* ((params
                  (org-mcp-test--build-add-todo-params
                   ,title ,todo-state ,tags ,body ,parent-uri ,after-uri))
                 (result-text (mcp-server-lib-ert-call-tool "org-add-todo" params))
                 (result (json-read-from-string result-text)))
            (org-mcp-test--assert-add-todo-result-shape
             result ,title ,basename)
            (org-mcp-test--verify-file-matches test-file ,expected-pattern))))
    `(org-mcp-test--with-add-todo-setup
      test-file
      ,initial-content ,todo-keywords ,tag-alist ,ids
      ,(if override-bindings
           `(let ,override-bindings
              ,checking-logic)
         checking-logic))))

;; Helper functions for testing org-update-todo-state MCP tool

(defun org-mcp-test--call-update-todo-state-expecting-error
    (test-file resource-uri current-state new-state)
  "Call org-update-todo-state tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI to update.
CURRENT-STATE is the current TODO state.
NEW-STATE is the new TODO state to set."
  (let ((org-todo-keywords
         '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((request
              (mcp-server-lib-create-tools-call-request
               "org-update-todo-state" 1
               `((uri . ,resource-uri)
                 (current_state . ,current-state)
                 (new_state . ,new-state))))
            (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result)))))

(defun org-mcp-test--update-todo-state-and-check
    (resource-uri old-state new-state test-file expected-content-regex)
  "Update TODO state and verify the result via MCP JSON-RPC.
RESOURCE-URI is the URI to update.
OLD-STATE is the current TODO state to update from.
NEW-STATE is the new TODO state to update to.
TEST-FILE is the file to verify content after update.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer."
  (let* ((params
          `((uri . ,resource-uri)
            (current_state . ,old-state)
            (new_state . ,new-state)))
         (result-text
          (mcp-server-lib-ert-call-tool "org-update-todo-state" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 4))
    (should (equal (alist-get 'success result) t))
    (should (equal (alist-get 'previous_state result) old-state))
    (should (equal (alist-get 'new_state result) new-state))
    (should (stringp (alist-get 'uri result)))
    (should (string-prefix-p "org-id://" (alist-get 'uri result)))
    ;; For ID-based URIs, verify the returned URI matches the input
    (when (string-prefix-p "org-id://" resource-uri)
      (should (equal (alist-get 'uri result) resource-uri)))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)))

;; Helper functions for testing org-read-headline MCP tool

(defun org-mcp--tool-read-headline-and-check (initial-content headline-path expected-pattern-regex)
  "Call org-read-headline tool via JSON-RPC and verify the result.
INITIAL-CONTENT is the content to write to the temp file.
HEADLINE-PATH is the slash-separated path to the headline.
EXPECTED-PATTERN-REGEX is an anchored regex that matches the expected result."
  (org-mcp-test--with-temp-org-files
      ((test-file initial-content))
    (let* ((params `((file . ,test-file)
                     (headline_path . ,headline-path)))
           (result-text (mcp-server-lib-ert-call-tool "org-read-headline" params)))
      (should
       (string-match-p expected-pattern-regex result-text)))))

(defmacro org-mcp-test--call-read-headline-expecting-error (content headline-path)
  "Call org-read-headline tool via JSON-RPC expecting an error.
CONTENT is the Org file content to use.
HEADLINE-PATH is the headline path string."
  (declare (indent 0))
  `(org-mcp-test--with-temp-org-files
       ((test-file ,content))
     (let* ((request
              (mcp-server-lib-create-tools-call-request
               "org-read-headline" 1
               (list (cons 'file test-file)
                     (cons 'headline_path ,headline-path))))
            (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result))))

;; Helper functions for testing org-rename-headline MCP tool

(defun org-mcp-test--call-rename-headline-and-check
    (initial-content headline-path-or-uri current-title new-title
                     expected-content-regex
                     &optional ids-to-register)
  "Call org-rename-headline tool via JSON-RPC and verify the result.
INITIAL-CONTENT is the initial Org file content.
HEADLINE-PATH-OR-URI is either a headline path fragment or full URI.
CURRENT-TITLE is the expected current title.
NEW-TITLE is the new title to set.
EXPECTED-CONTENT-REGEX is an anchored regex that matches the complete buffer.
IDS-TO-REGISTER is optional list of IDs to register for the temp file."
  (org-mcp-test--with-temp-org-files
   ((test-file initial-content))
   (when ids-to-register
     (let ((org-id-track-globally t)
           (org-id-locations-file nil)
           (org-id-locations nil))
       (dolist (id ids-to-register)
         (org-id-add-location id test-file))))
   (let* ((uri (if (string-prefix-p "org-" headline-path-or-uri)
                   headline-path-or-uri
                 (format "org-headline://%s#%s" test-file headline-path-or-uri)))
          (params
           `((uri . ,uri)
             (current_title . ,current-title)
             (new_title . ,new-title)))
          (result-text
           (mcp-server-lib-ert-call-tool "org-rename-headline" params))
          (result (json-read-from-string result-text))
          (result-uri (alist-get 'uri result)))
     (should (= (length result) 4))
     (should (equal (alist-get 'success result) t))
     (should (equal (alist-get 'previous_title result) current-title))
     (should (equal (alist-get 'new_title result) new-title))
     (should (stringp result-uri))
     (should (string-prefix-p "org-id://" result-uri))
     ;; If input URI was ID-based, result URI should remain ID-based
     (when (string-prefix-p "org-id://" uri)
       (should (equal result-uri uri)))
     (org-mcp-test--verify-file-matches test-file expected-content-regex))))

(defun org-mcp-test--assert-rename-headline-rejected
    (initial-content headline-title new-title)
  "Assert renaming headline to NEW-TITLE is rejected.
INITIAL-CONTENT is the Org content to test with.
HEADLINE-TITLE is the current headline to rename.
NEW-TITLE is the invalid new title that should be rejected."
  (org-mcp-test--call-rename-headline-expecting-error
   initial-content
   (url-hexify-string headline-title)
   headline-title
   new-title))

(defun org-mcp-test--call-rename-headline-expecting-error
    (initial-content headline-path-or-uri current-title new-title)
  "Call org-rename-headline tool expecting an error and verify file unchanged.
INITIAL-CONTENT is the initial Org file content.
HEADLINE-PATH-OR-URI is either a headline path fragment or full URI.
CURRENT-TITLE is the current title for validation.
NEW-TITLE is the new title to set."
  (org-mcp-test--with-temp-org-files
   ((test-file initial-content))
   (let ((uri (if (string-prefix-p "org-" headline-path-or-uri)
                  headline-path-or-uri
                (format "org-headline://%s#%s" test-file headline-path-or-uri))))
     (org-mcp-test--assert-error-and-file
      test-file
      (let* ((params
              `((uri . ,uri)
                (current_title . ,current-title)
                (new_title . ,new-title)))
             (request
               (mcp-server-lib-create-tools-call-request
                "org-rename-headline" 1 params))
             (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
             (result (mcp-server-lib-ert-process-tool-response response)))
        ;; If we get here, the tool succeeded when we expected failure
        (error "Expected error but got success: %s" result))))))

;; Helper functions for testing org-edit-body MCP tool

(defun org-mcp-test--call-edit-body-and-check
    (test-file resource-uri old-body new-body expected-pattern
               &optional replace-all expected-id)
  "Call org-edit-body tool and check result structure and file content.
TEST-FILE is the path to the file to check.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
EXPECTED-PATTERN is a regexp that the file content should match.
REPLACE-ALL if true, replace all occurrences (default: nil).  The
`replace_all' key is always emitted, so passing nil exercises the
explicit-`null' wire form; tests needing absent-key build the
params alist directly.
EXPECTED-ID if provided, check the returned URI has this exact ID."
  (let* ((params
          `((resource_uri . ,resource-uri)
            (old_body . ,old-body)
            (new_body . ,new-body)
            (replace_all . ,replace-all)))
         (result-text (mcp-server-lib-ert-call-tool "org-edit-body" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 2))
    (should (equal (alist-get 'success result) t))
    (let ((uri (alist-get 'uri result)))
      (if expected-id
          (should (equal uri (concat "org-id://" expected-id)))
        (should (string-prefix-p "org-id://" uri))))
    (org-mcp-test--verify-file-matches test-file expected-pattern)))

(defun org-mcp-test--call-edit-body-expecting-error
    (test-file resource-uri old-body new-body &optional replace-all)
  "Call org-edit-body tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
REPLACE-ALL if true, replace all occurrences (default: nil).  The
`replace_all' key is always emitted, so passing nil exercises the
explicit-`null' wire form; tests needing absent-key build the
params alist directly."
  (org-mcp-test--assert-error-and-file
   test-file
   (let* ((params
           `((resource_uri . ,resource-uri)
             (old_body . ,old-body)
             (new_body . ,new-body)
             (replace_all . ,replace-all)))
          (request
            (mcp-server-lib-create-tools-call-request
             "org-edit-body" 1 params))
          (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
          (result (mcp-server-lib-ert-process-tool-response response)))
     ;; If we get here, the tool succeeded when we expected failure
     (error "Expected error but got success: %s" result))))

;; Helper functions for testing org-read-file MCP tool

(defun org-mcp-test--call-read-file (file)
  "Call org-read-file tool via JSON-RPC and return the result.
FILE is the file path to read."
  (let ((params `((file . ,file))))
    (mcp-server-lib-ert-call-tool "org-read-file" params)))

;; Helper functions for testing org-read-outline MCP tool

(defun org-mcp-test--call-read-outline (file)
  "Call org-read-outline tool via JSON-RPC and return the result.
FILE is the file path to read the outline from."
  (let* ((params `((file . ,file)))
         (result-json
          (mcp-server-lib-ert-call-tool "org-read-outline" params)))
    (json-parse-string result-json :object-type 'alist)))

;; Helper functions for testing org-read-by-id MCP tool

(defun org-mcp-test--call-read-by-id-and-check (uuid expected-pattern)
  "Call org-read-by-id tool via JSON-RPC and verify the result.
UUID is the ID property of the headline to read.
EXPECTED-PATTERN is a regex pattern the result should match."
  (let* ((params `((uuid . ,uuid)))
         (result-text (mcp-server-lib-ert-call-tool "org-read-by-id" params)))
    (should (string-match-p expected-pattern result-text))))

;; Helper functions for testing MCP resources

(defun org-mcp-test--verify-resource-read (uri text)
  "Verify MCP resource at URI being TEXT."
  (mcp-server-lib-ert-verify-resource-read
   uri `((uri . ,uri)
         (text . ,text)
         (mimeType . "text/plain"))))

(defun org-mcp-test--read-resource-expecting-error
    (uri expected-error-message)
  "Read resource at URI expecting an error with EXPECTED-ERROR-MESSAGE."
  (let* ((request (mcp-server-lib-create-resources-read-request uri))
         (response-json (mcp-server-lib-process-jsonrpc request mcp-server-lib-ert-server-id))
         (response
          (json-parse-string response-json :object-type 'alist)))
    (unless (assoc 'error response)
      (error "Expected error but got success for URI: %s" uri))
    (mcp-server-lib-ert-check-error-object
     response
     mcp-server-lib-jsonrpc-error-invalid-params
     expected-error-message)))

(defun org-mcp-test--test-headline-resource-with-extension (extension)
  "Test headline resource with file having EXTENSION.
EXTENSION can be a string like \".txt\" or nil for no extension."
  (let ((test-file
         (make-temp-file
          "org-mcp-test" nil extension org-mcp-test--content-nested-siblings)))
    (unwind-protect
        (let ((org-mcp-allowed-files (list test-file))
              (uri
               (format "org-headline://%s#Parent%%20Task"
                       test-file)))
          (org-mcp-test--with-enabled
           (org-mcp-test--verify-resource-read
            uri
            org-mcp-test--expected-parent-task-from-nested-siblings)))
      (delete-file test-file))))

;;; Tests

;; org-get-todo-config tests

(ert-deftest org-mcp-test-tool-get-todo-config-empty ()
  "Test org-get-todo-config with empty `org-todo-keywords'."
  (org-mcp-test--with-get-todo-config-result
   nil
   (should (assoc 'sequences result))
   (should (assoc 'semantics result))
   (should (equal sequences []))
   (should (equal semantics []))))

(ert-deftest org-mcp-test-tool-get-todo-config-default ()
  "Test org-get-todo-config with default `org-todo-keywords'."
  (org-mcp-test--with-get-todo-config-result '((sequence "TODO(t!)" "DONE(d!)"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["TODO(t!)" "|" "DONE(d!)"])
    (should (= (length semantics) 2))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-single-keyword ()
  "Test org-get-todo-config with single keyword."
  (org-mcp-test--with-get-todo-config-result '((sequence "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["|" "DONE"])
    (should (= (length semantics) 1))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-explicit-bar ()
  "Test org-get-todo-config with explicit | and multiple states."
  (org-mcp-test--with-get-todo-config-result '((sequence
                                "TODO" "NEXT" "|" "DONE" "CANCELLED"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0)
     "sequence"
     ["TODO" "NEXT" "|" "DONE" "CANCELLED"])
    (should (= (length semantics) 4))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "NEXT" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "DONE" t "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "CANCELLED" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type ()
  "Test org-get-todo-config with type keywords."
  (org-mcp-test--with-get-todo-config-result '((type "Fred" "Sara" "Lucy" "|" "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "type" ["Fred" "Sara" "Lucy" "|" "DONE"])
    (should (= (length semantics) 4))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "Fred" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "Sara" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 2) "Lucy" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "DONE" t "type")))

(ert-deftest org-mcp-test-tool-get-todo-config-multiple-sequences ()
  "Test org-get-todo-config with multiple sequences."
  (org-mcp-test--with-get-todo-config-result '((sequence "TODO" "|" "DONE")
                               (type "BUG" "FEATURE" "|" "FIXED"))
    (should (= (length sequences) 2))
    ;; First sequence
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["TODO" "|" "DONE"])
    ;; Second sequence
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 1) "type" ["BUG" "FEATURE" "|" "FIXED"])
    (should (= (length semantics) 5))
    ;; Semantics from first sequence
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "DONE" t "sequence")
    ;; Semantics from second sequence
    (org-mcp-test--check-todo-config-semantic (aref semantics 2) "BUG" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 3) "FEATURE" nil "type")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 4) "FIXED" t "type")))

(ert-deftest org-mcp-test-tool-get-todo-config-no-done-states ()
  "Test org-get-todo-config with no done states."
  (org-mcp-test--with-get-todo-config-result '((sequence "TODO" "NEXT" "|"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-todo-config-sequence
     (aref sequences 0) "sequence" ["TODO" "NEXT" "|"])
    (should (= (length semantics) 2))
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-todo-config-semantic
     (aref semantics 1) "NEXT" nil "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type-no-separator ()
  "Test org-get-todo-config with type keywords and no separator."
  (org-mcp-test--with-get-todo-config-result
   '((type "BUG" "FEATURE" "ENHANCEMENT"))
   (should (= (length sequences) 1))
   (org-mcp-test--check-todo-config-sequence
    (aref sequences 0) "type" ["BUG" "FEATURE" "|" "ENHANCEMENT"])
   (should (= (length semantics) 3))
   (org-mcp-test--check-todo-config-semantic (aref semantics 0) "BUG" nil "type")
   (org-mcp-test--check-todo-config-semantic
    (aref semantics 1) "FEATURE" nil "type")
   (org-mcp-test--check-todo-config-semantic
    (aref semantics 2) "ENHANCEMENT" t "type")))

;; org-get-tag-config tests

(ert-deftest org-mcp-test-tool-get-tag-config-empty ()
  "Test org-get-tag-config with empty `org-tag-alist'."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check "nil" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-simple ()
  "Test org-get-tag-config with simple tags."
  (let ((org-tag-alist '("work" "personal" "urgent"))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\" \"urgent\")" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-with-keys ()
  "Test org-get-tag-config with fast selection keys."
  (let ((org-tag-alist
         '(("work" . ?w) ("personal" . ?p) "urgent" ("@home" . ?h)))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check
     "((\"work\" . 119) (\"personal\" . 112) \"urgent\" (\"@home\" . 104))"
     "nil"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-with-groups ()
  "Test org-get-tag-config with tag groups."
  (let ((org-tag-alist
         '((:startgroup)
           ("@office" . ?o)
           ("@home" . ?h)
           ("@errand" . ?e)
           (:endgroup)
           "laptop"
           (:startgrouptag)
           ("project")
           (:grouptags)
           ("proj_a")
           ("proj_b")
           (:endgrouptag)))
        (org-tag-persistent-alist nil))
    (org-mcp-test--get-tag-config-and-check
     "((:startgroup) (\"@office\" . 111) (\"@home\" . 104) (\"@errand\" . 101) (:endgroup) \"laptop\" (:startgrouptag) (\"project\") (:grouptags) (\"proj_a\") (\"proj_b\") (:endgrouptag))"
     "nil"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-persistent ()
  "Test org-get-tag-config with persistent tags."
  (let ((org-tag-alist '(("work" . ?w)))
        (org-tag-persistent-alist '(("important" . ?i) "recurring"))
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "((\"work\" . 119))" "((\"important\" . 105) \"recurring\")"
     "t"
     "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-enabled ()
  "Test org-get-tag-config with inheritance enabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "t" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-disabled ()
  "Test org-get-tag-config with inheritance disabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance nil))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "nil" "nil")))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-selective ()
  "Test org-get-tag-config with selective inheritance (list)."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance '("work")))
    (org-mcp-test--get-tag-config-and-check
     "(\"work\" \"personal\")" "nil" "(\"work\")"
     "nil")))

;; org-get-allowed-files tests

(ert-deftest org-mcp-test-tool-get-allowed-files-empty ()
  "Test org-get-allowed-files with empty configuration."
  (org-mcp-test--get-allowed-files-and-check nil nil))

(ert-deftest org-mcp-test-tool-get-allowed-files-single ()
  "Test org-get-allowed-files with single file."
  (org-mcp-test--get-allowed-files-and-check
   '("/home/user/tasks.org")
   '("/home/user/tasks.org")))

(ert-deftest org-mcp-test-tool-get-allowed-files-multiple ()
  "Test org-get-allowed-files with multiple files."
  (org-mcp-test--get-allowed-files-and-check
   '("/home/user/tasks.org"
     "/home/user/projects.org"
     "/home/user/notes.org")
   '("/home/user/tasks.org"
     "/home/user/projects.org"
     "/home/user/notes.org")))

(ert-deftest org-mcp-test-file-not-in-allowed-list-returns-error ()
  "Test that reading a file not in allowed list returns an error."
  (org-mcp-test--with-temp-org-files
   ((allowed-file "Allowed content")
    (forbidden-file "Forbidden content"))
   (let ((org-mcp-allowed-files (list allowed-file)))
     ;; Try to read the forbidden file
     (let ((uri (format "org://%s" forbidden-file)))
       (org-mcp-test--read-resource-expecting-error
        uri
        (format "'%s': the referenced file not in allowed list" forbidden-file))))))

;;; org-update-todo-state tests

(ert-deftest org-mcp-test-update-todo-state-success ()
  "Test successful TODO state update."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((org-todo-keywords
            '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
       ;; Update TODO to IN-PROGRESS
       (let ((resource-uri
              (format "org-headline://%s#Task%%20with%%20ID" test-file)))
         (org-mcp-test--update-todo-state-and-check
          resource-uri "TODO" "IN-PROGRESS"
          test-file org-mcp-test--expected-task-with-id-in-progress-regex))))))

(ert-deftest org-mcp-test-update-todo-state-mismatch ()
  "Test TODO state update fails on state mismatch."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Try to update with wrong current state
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "IN-PROGRESS" "DONE")))))

(ert-deftest org-mcp-test-update-todo-with-timestamp-id ()
  "Test updating TODO state using timestamp-format ID (not UUID)."
  (let ((test-content org-mcp-test--content-timestamp-id))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `("20240101T120000")
        (let ((uri "org-id://20240101T120000"))
          (org-mcp-test--update-todo-state-and-check
           uri "TODO" "DONE"
           test-file
           org-mcp-test--expected-timestamp-id-done-regex))))))

(ert-deftest org-mcp-test-update-todo-state-empty-newstate-invalid ()
  "Test that empty string for new_state is rejected."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Try to set empty state
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" "")))))

(ert-deftest org-mcp-test-update-todo-state-invalid ()
  "Test TODO state update fails for invalid new state."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Try to update to invalid state
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" "INVALID-STATE")))))

(ert-deftest org-mcp-test-update-todo-state-with-open-buffer ()
  "Test TODO state update works when file is open in another buffer."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Open the file in a buffer
        (org-mcp-test--with-file-buffer buffer test-file
          ;; Update TODO state while buffer is open
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20with%%20ID"
                         test-file)))
            (org-mcp-test--update-todo-state-and-check
             resource-uri "TODO" "IN-PROGRESS"
             test-file org-mcp-test--expected-task-with-id-in-progress-regex)
            ;; Verify the buffer was also updated
            (with-current-buffer buffer
              (goto-char (point-min))
              (should
               (re-search-forward "^\\* IN-PROGRESS Task with ID"
                                  nil t)))))))))

(ert-deftest org-mcp-test-update-todo-state-with-modified-buffer ()
  "Test TODO state update fails when buffer has unsaved changes."
  (let ((test-content org-mcp-test--content-simple-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      ;; Open the file in a buffer and modify it elsewhere
      (org-mcp-test--with-file-buffer buffer test-file
        ;; Make a modification at an unrelated location
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert "\n* TODO Another Task\nAdded in buffer.")
          ;; Buffer is now modified but not saved
          (should (buffer-modified-p)))

        ;; Try to update while buffer has unsaved changes
        (let ((resource-uri
               (format "org-headline://%s#Original%%20Task"
                       test-file)))
          (org-mcp-test--call-update-todo-state-expecting-error
           test-file resource-uri "TODO" "IN-PROGRESS")
          ;; Verify buffer still has unsaved changes
          (with-current-buffer buffer
            (should (buffer-modified-p))))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-id ()
  "Test TODO state update fails for non-existent UUID."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-id-setup test-file test-content '()
      ;; Try to update a non-existent ID
      (let ((resource-uri "org-id://nonexistent-uuid-12345"))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" "IN-PROGRESS")))))

(ert-deftest org-mcp-test-update-todo-state-by-id ()
  "Test updating TODO state using org-id:// URI."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (let ((org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (org-mcp-test--with-id-setup test-file test-content
          `(,org-mcp-test--content-with-id-id)
        (org-mcp-test--update-todo-state-and-check
         org-mcp-test--content-with-id-uri "TODO" "IN-PROGRESS"
         test-file
         org-mcp-test--expected-task-with-id-in-progress-regex)))))

(ert-deftest org-mcp-test-id-fallback-scan-caches-resolution ()
  "Test that DB-miss ID resolution writes to `org-id-locations'.
Locks the cache-write side effect of
`org-mcp--find-allowed-file-with-id': when the org-id DB has no
record of the ID but a fallback scan of `org-mcp-allowed-files'
finds it, the (id, file) pair must be registered into
`org-id-locations' so the next lookup hits the DB at O(1) instead
of re-scanning every allowed file.  Without this test, a
regression that silently dropped the `org-id-add-location' call
would still pass every existing ID-using test because they all
pre-register IDs via `with-id-setup'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-with-id-todo))
    (let ((org-id-track-globally t)
          (org-id-locations-file nil)
          (org-id-locations nil)
          (org-mcp-allowed-files (list test-file))
          (org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (should-not
       (org-id-find-id-file org-mcp-test--content-with-id-id))
      (mcp-server-lib-ert-call-tool
       "org-update-todo-state"
       `((uri . ,org-mcp-test--content-with-id-uri)
         (current_state . "TODO")
         (new_state . "DONE")))
      (should
       (equal
        (file-truename
         (org-id-find-id-file
          org-mcp-test--content-with-id-id))
        (file-truename test-file))))))

(ert-deftest org-mcp-test-id-fallback-scan-skips-cache-when-tracking-off ()
  "Test that DB-miss ID resolution skips the cache write when
`org-id-track-globally' is nil.  Locks the gating contract: users
who have opted out of global ID tracking must not get implicit
`org-id-locations' mutations from MCP tool calls; the fallback
scan still resolves the ID and the tool succeeds, but the DB
stays untouched."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-with-id-todo))
    (let ((org-id-track-globally nil)
          (org-id-locations-file nil)
          (org-id-locations nil)
          (org-mcp-allowed-files (list test-file))
          (org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (mcp-server-lib-ert-call-tool
       "org-update-todo-state"
       `((uri . ,org-mcp-test--content-with-id-uri)
         (current_state . "TODO")
         (new_state . "DONE")))
      (should-not
       (org-id-find-id-file
        org-mcp-test--content-with-id-id)))))

(ert-deftest org-mcp-test-id-fallback-scan-tolerates-cache-write-failure
    ()
  "Test that a signal from `org-id-add-location' does not poison
ID resolution.  When the fallback scan finds the ID, the cache
write into `org-id-locations' is best-effort: a failing
`org-id-add-location' (locked file, write error, broken DB) must
not surface as a tool error, because the resolution semantically
already succeeded by the time the cache write is attempted."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-with-id-todo))
    (let ((org-id-track-globally t)
          (org-id-locations-file nil)
          (org-id-locations nil)
          (org-mcp-allowed-files (list test-file))
          (org-todo-keywords
           '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
      (should-not
       (org-id-find-id-file org-mcp-test--content-with-id-id))
      (cl-letf (((symbol-function 'org-id-add-location)
                 (lambda (&rest _) (error "cache write boom"))))
        (mcp-server-lib-ert-call-tool
         "org-update-todo-state"
         `((uri . ,org-mcp-test--content-with-id-uri)
           (current_state . "TODO")
           (new_state . "DONE")))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-headline ()
  "Test TODO state update fails for non-existent headline path."
  (let ((test-content org-mcp-test--content-simple-todo))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     ;; Try to update a non-existent headline
     (let ((resource-uri
            (format "org-headline://%s#Nonexistent%%20Task"
                    test-file)))
       (org-mcp-test--call-update-todo-state-expecting-error
        test-file resource-uri "TODO" "IN-PROGRESS")))))

;; org-add-todo tests

(ert-deftest org-mcp-test-add-todo-top-level ()
  "Test adding a top-level TODO item."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo))

(ert-deftest org-mcp-test-add-todo-top-level-with-header ()
  "Test adding top-level TODO after header comments."
  (let ((initial-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--add-todo-and-check
     initial-content
     "New Top Task"
     "TODO"
     '("urgent")
     nil ; no body
     (format "org-headline://%s#" test-file)
     (file-name-nondirectory test-file)
     org-mcp-test--expected-regex-top-level-with-header)))

(ert-deftest org-mcp-test-add-todo-invalid-state ()
  "Test that adding TODO with invalid state throws error."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task"
   "INVALID-STATE" ; Not in org-todo-keywords
   '("work")
   nil
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-empty-title ()
  "Test that adding TODO with empty title throws error."
  (org-mcp-test--assert-add-todo-invalid-title ""))

(ert-deftest org-mcp-test-add-todo-spaces-only-title ()
  "Test that adding TODO with spaces-only title throws error."
  (org-mcp-test--assert-add-todo-invalid-title "   "))

(ert-deftest org-mcp-test-add-todo-mixed-whitespace-title ()
  "Test that adding TODO with mixed whitespace title throws error."
  (org-mcp-test--assert-add-todo-invalid-title "	  	"))

(ert-deftest org-mcp-test-add-todo-unicode-nbsp-title ()
  "Test that adding TODO with Unicode non-breaking space throws error."
  ;; U+00A0 is the non-breaking space character
  (org-mcp-test--assert-add-todo-invalid-title "\u00A0"))

(ert-deftest org-mcp-test-add-todo-embedded-newline-title ()
  "Test that adding TODO with embedded newline in title throws error."
  (org-mcp-test--assert-add-todo-invalid-title
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-add-todo-tag-reject-invalid-with-alist ()
  "Test that tags not in `org-tag-alist' are rejected."
  ;; Should reject tags not in org-tag-alist
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "Task" "TODO" '("invalid") nil
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-tag-accept-valid-with-alist ()
  "Test that tags in `org-tag-alist' are accepted."
  ;; Should accept tags in org-tag-alist (work, personal, urgent)
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "ValidTask"
   "TODO"
   '("work")
   nil
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-tag-accept-valid))

(ert-deftest org-mcp-test-add-todo-tag-validation-without-alist ()
  "Test valid tag names are accepted when `org-tag-alist' is empty."
  ;; Should accept valid tag names (alphanumeric, _, @)
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task1"
   "TODO"
   '("validtag" "tag123" "my_tag" "@home")
   nil
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-tag-validation-without-alist
   :override-bindings ((org-tag-alist nil)
                       (org-tag-persistent-alist nil))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-exclamation ()
  "Test that tags with exclamation mark are rejected."
  (org-mcp-test--assert-add-todo-invalid-tag "invalid-tag!"))

(ert-deftest org-mcp-test-add-todo-tag-invalid-dash ()
  "Test that tags with dash character are rejected."
  (org-mcp-test--assert-add-todo-invalid-tag "tag-with-dash"))

(ert-deftest org-mcp-test-add-todo-tag-invalid-hash ()
  "Test that tags with hash character are rejected."
  (org-mcp-test--assert-add-todo-invalid-tag "tag#hash"))

(ert-deftest org-mcp-test-add-todo-child-under-parent ()
  "Test adding a child TODO under an existing parent."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-under-parent))

(ert-deftest org-mcp-test-add-todo-child-end-parent-followed-by-other ()
  "Test child insert avoids a spurious blank line before a following heading.
When `org-end-of-subtree' is called from a parent that has content
following it, point lands at the `*' of the next heading; the
insertion pipeline must produce a single `\\n' between the new child
and that following heading.  Locks in the fix to
`org-mcp--position-for-new-child'."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-child-then-other-top
   "New Child"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-end-no-blank-before-other-top))

(ert-deftest org-mcp-test-add-todo-empty-after-uri-rejected ()
  "Test that adding a child TODO with empty `after_uri' is rejected.
Empty string is distinct from absent/nil: absent appends as last
child, but `\"\"' must error so a client cannot silently mistake
an unset field for an explicit no-op."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-nested-siblings
   "Child Task" "TODO" '("work") nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   :after-uri ""))

(ert-deftest org-mcp-test-add-todo-whitespace-after-uri-rejected ()
  "Test that adding a child TODO with whitespace-only `after_uri' is rejected.
A whitespace-only string carries the same no-meaningful-content
intent as `\"\"' (which is already rejected by
`org-mcp-test-add-todo-empty-after-uri-rejected') but slipped
through the `string-empty-p' guard and surfaced as the prefix-shape
`Field after_uri is not org-id://: ...' error.  Lock the
no-meaningful-content rejection at the validation boundary so
whitespace and the empty string take the same error path."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-nested-siblings
   "Child Task" "TODO" '("work") nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   :after-uri "   "))

(ert-deftest org-mcp-test-add-todo-after-uri-nil-wire-accepted ()
  "Test that an explicit JSON `null' on the wire for `after_uri' succeeds.
The empty/whitespace pair is rejected at the validation boundary;
the positive half of that contract is that an explicit JSON `null'
must be accepted as equivalent to omitting the key, landing on the
same last-child append path.  This test bypasses
`org-mcp-test--build-add-todo-params' and builds the params alist
directly with `(cons 'after_uri nil)' so the wire-null contract is
pinned independent of helper shape -- a future helper that omits
nil keys would otherwise silently strip the only coverage of the
JSON-null wire form."
  (org-mcp-test--with-add-todo-setup
      test-file org-mcp-test--content-nested-siblings nil nil nil
    (let* ((params
            (list (cons 'title "Child Task")
                  (cons 'todo_state "TODO")
                  (cons 'tags '("work"))
                  (cons 'body nil)
                  (cons 'parent_uri
                        (format "org-headline://%s#Parent%%20Task"
                                test-file))
                  (cons 'after_uri nil)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-add-todo" params))
           (result (json-read-from-string result-text)))
      (org-mcp-test--assert-add-todo-result-shape
       result "Child Task" (file-name-nondirectory test-file))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-add-todo-second-child-same-level ()
  "Test that adding a second child creates it at the same level as first child.
This tests the bug where the second child was created at level 4 instead of level 3."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-level2-parent-level3-children
   "Second Child"
   "TODO"
   '("work")
   nil  ; no body
   (format "org-headline://%s#Top%%20Level/Review%%20the%%20package"
           test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-second-child-same-level))

(ert-deftest org-mcp-test-add-todo-with-after-uri ()
  "Test adding TODO after a sibling using after_uri.
Tests that adding after a level 3 sibling correctly creates level 3 (not level 1).
Reproduces the emacs.org scenario: level 2 parent (via path), level 3 sibling (via ID)."
  ;; BUG: org-insert-heading creates level 1 (*) instead of level 3 (***)
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-level2-parent-level3-children
   "Review org-mcp-test.el"
   "TODO"
   '("internet")
   nil
   (format "org-headline://%s#Top%%20Level/Review%%20the%%20package"
           test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-after-sibling-level3
   :todo-keywords '((sequence "TODO" "|" "DONE"))
   :tag-alist '("internet")
   :ids `(,org-mcp-test--level2-parent-level3-sibling-id)
   :after-uri (format "org-id://%s"
                      org-mcp-test--level2-parent-level3-sibling-id)))

(ert-deftest org-mcp-test-add-todo-after-uri-eof-no-trailing-newline-with-body ()
  "Test body insert after_uri at parent's last child at EOF with no `\\n'.
The fixture's parent (`Review the package') is the file's last
subtree and ends mid-line at `point-max' (no trailing newline).
`org-mcp--insert-heading-line' bakes a `\\n' into the new heading
line, and the body-insertion block previously left that `\\n' in
place after the body's own terminator, producing a trailing blank
line at EOF.  Locks the fix that drops the leftover `\\n' when it
is the buffer's last char."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-level2-parent-level3-children
   "Review org-mcp-test.el"
   "TODO"
   '("internet")
   org-mcp-test--body-text-multiline
   (format "org-headline://%s#Top%%20Level/Review%%20the%%20package"
           test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-after-sibling-level3-with-body
   :todo-keywords '((sequence "TODO" "|" "DONE"))
   :tag-alist '("internet")
   :ids `(,org-mcp-test--level2-parent-level3-sibling-id)
   :after-uri (format "org-id://%s"
                      org-mcp-test--level2-parent-level3-sibling-id)))

(ert-deftest org-mcp-test-add-todo-with-body ()
  "Test adding TODO with body text."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task with Body"
   "TODO"
   '("work")
   org-mcp-test--body-text-multiline
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-with-body))

(ert-deftest org-mcp-test-add-todo-empty-body-no-spurious-blank-lines ()
  "Test that `body=\"\"' produces the same file as `body=nil'.
Empty string is a valid `body' value (the validator accepts both
nil and \"\" because `allow-nil=t'), but the body-insertion block
would emit `(insert \"\\n\" \"\")' (one newline) plus a trailing
guard `(unless (string-suffix-p \"\\n\" \"\") (insert \"\\n\"))'
that also fires -- yielding two unwanted blank lines after the
new heading.  Locks the normalization fix that collapses an empty
`body' to nil before the insertion block runs."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "New Task"
   "TODO"
   '("work" "urgent")
   "" ; empty body — must be treated as no-body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   (concat
    "\\`\\* TODO New Task +:.*work.*urgent.*:\n"
    org-mcp-test--regex-id-drawer "\\'")))

(ert-deftest org-mcp-test-add-todo-body-with-same-level-headline ()
  "Test that adding TODO with body containing same-level headline is rejected."
  (org-mcp-test--assert-add-todo-rejects-body-headline
   org-mcp-test--content-empty
   "" ; top-level parent
   "Some initial text.\n* Another headline\nMore text."))

(ert-deftest org-mcp-test-add-todo-body-with-higher-level-headline ()
  "Test that adding TODO with body containing higher-level headline is rejected."
  (org-mcp-test--assert-add-todo-rejects-body-headline
   "* Parent\n"
   "Parent"
   "Some initial text.\n* Top level headline\nMore text."))

(ert-deftest org-mcp-test-add-todo-body-with-headline-at-eof ()
  "Test that adding TODO with body ending in headline at EOF is rejected."
  (org-mcp-test--assert-add-todo-rejects-body-headline
   org-mcp-test--content-empty
   "" ; top-level parent
   "Some initial text.\n* Headline at EOF"))

(ert-deftest org-mcp-test-add-todo-body-with-asterisk-only-at-eof ()
  "Test that body ending with just asterisk at EOF is correctly accepted.
A single asterisk without space is not a valid Org headline."
  ;; Should succeed since * without space is not a headline
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task"
   "TODO"
   '("work")
   "Some initial text.\n*"
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   (concat
    "\\`\\* TODO Task +:work:\n"
    org-mcp-test--regex-id-drawer
    "Some initial text\\.\n"
    "\\*\n\\'")))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-block ()
  "Test that adding TODO with body containing unbalanced block is rejected.
Unbalanced blocks like #+BEGIN_EXAMPLE without #+END_EXAMPLE should be
rejected in TODO body content."
  ;; Should reject unbalanced blocks
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "Task with unbalanced block"
   "TODO"
   '("work")
   "Here's an example:\n#+BEGIN_EXAMPLE\nsome code\nMore text after block"
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-end-block ()
  "Test that adding TODO with body containing unbalanced END block is rejected.
An #+END_EXAMPLE without matching #+BEGIN_EXAMPLE should be rejected."
  ;; Should reject unbalanced END blocks
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "Task with unbalanced END block"
   "TODO"
   '("work")
   "Some text before\n#+END_EXAMPLE\nMore text after"
   (format "org-headline://%s#" test-file)))

(ert-deftest org-mcp-test-add-todo-body-with-literal-block-end ()
  "Test that TODO body with END_SRC inside EXAMPLE block is accepted.
#+END_SRC inside an EXAMPLE block is literal text, not a block delimiter.
This is valid Org-mode syntax and should be allowed."
  ;; Should succeed - #+END_SRC is just literal text inside EXAMPLE block
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task with literal END_SRC"
   "TODO"
   '("work")
   "Example of source block:\n#+BEGIN_EXAMPLE\n#+END_SRC\n#+END_EXAMPLE\nText after."
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-with-literal-block-end))

(ert-deftest org-mcp-test-add-todo-after-sibling ()
  "Test adding TODO after a specific sibling."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "New Task After Second"
   "TODO"
   '("work")
   nil
   (format "org-headline://%s#Parent%%20Task"
           test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-after-second-child
   :todo-keywords '((sequence "TODO" "|" "DONE"))
   :tag-alist '("work")
   :ids (list org-mcp-test--content-nested-siblings-parent-id
              org-mcp-test--content-with-id-id)
   :after-uri org-mcp-test--content-with-id-uri))

(ert-deftest org-mcp-test-add-todo-after-uri-not-sibling ()
  "Test error when after_uri is not a child of parent_uri."
  ;; Error: Other Child is not a child of First Parent
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-wrong-levels
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#First%%20Parent" test-file)
   :after-uri (format "org-headline://%s#Second%%20Parent/Other%%20Child" test-file)))

(ert-deftest org-mcp-test-add-todo-parent-id-uri ()
  "Test adding TODO with parent specified as org-id:// URI."
  ;; Use org-id:// for parent instead of org-headline://
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child via ID"
   "TODO"
   '("work")
   nil
   (format "org-id://%s"
           org-mcp-test--content-nested-siblings-parent-id)
   (file-name-nondirectory test-file)
   org-mcp-test--pattern-add-todo-parent-id-uri
   :todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)"))
   :tag-alist '("work")
   :ids (list org-mcp-test--content-nested-siblings-parent-id
              org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-add-todo-mutex-tags-error ()
  "Test that mutually exclusive tags are rejected."
  (org-mcp-test--call-add-todo-expecting-error
   "#+TITLE: Test Org File\n\n"
   "Test Task"
   "TODO"
   ["work" "@office" "@home"] ; conflicting tags
   nil
   (format "org-headline://%s#" test-file)
   :todo-keywords '((sequence "TODO" "|" "DONE"))
   :tag-alist '(("work" . ?w)
                :startgroup
                ("@office" . ?o)
                ("@home" . ?h)
                :endgroup)))

(ert-deftest org-mcp-test-add-todo-mutex-tags-valid ()
  "Test that non-conflicting tags from mutex groups are accepted."
  (org-mcp-test--add-todo-and-check
   "#+TITLE: Test Org File\n\n"
   "Test Task"
   "TODO"
   ["work" "@office" "project"] ; no conflict
   nil
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-add-todo-with-mutex-tags
   :todo-keywords '((sequence "TODO" "|" "DONE"))
   :tag-alist '(("work" . ?w)
                :startgroup
                ("@office" . ?o)
                ("@home" . ?h)
                :endgroup ("project" . ?p))))

(ert-deftest org-mcp-test-add-todo-nil-tags ()
  "Test that adding TODO with nil tags creates headline without tags."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task Without Tags"
   "TODO"
   nil ; nil for tags
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-without-tags))

(ert-deftest org-mcp-test-add-todo-empty-list-tags ()
  "Test that adding TODO with empty list tags creates headline without tags."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task Without Tags"
   "TODO"
   '() ; empty list for tags
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-without-tags))

;; org-rename-headline tests

(ert-deftest org-mcp-test-rename-headline-simple ()
  "Test renaming a simple TODO headline."
  (let ((org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
    (org-mcp-test--call-rename-headline-and-check
     org-mcp-test--content-simple-todo
     "Original%20Task"
     "Original Task"
     "Updated Task"
     org-mcp-test--pattern-renamed-simple-todo)))

(ert-deftest org-mcp-test-rename-headline-title-mismatch ()
  "Test that rename fails when current title doesn't match."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (org-mcp-test--call-rename-headline-expecting-error
     org-mcp-test--content-simple-todo
     "Original%20Task"
     "Wrong Title"
     "Updated Task")))

(ert-deftest org-mcp-test-rename-headline-preserve-tags ()
  "Test that renaming preserves tags."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-tag-alist '("work" "urgent" "personal")))
    (org-mcp-test--call-rename-headline-and-check
     org-mcp-test--content-todo-with-tags
     "Task%20with%20Tags"
     "Task with Tags"
     "Renamed Task"
     org-mcp-test--pattern-renamed-todo-with-tags)))

(ert-deftest org-mcp-test-rename-headline-no-todo ()
  "Test renaming a regular headline without TODO state."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-nested-siblings
   "Parent%20Task/First%20Child%2050%25%20Complete"
   "First Child 50% Complete"
   "Updated Child"
   org-mcp-test--pattern-renamed-headline-no-todo))

(ert-deftest org-mcp-test-rename-headline-nested-path-navigation ()
  "Test correct headline path navigation in nested structures.
Verifies that the implementation correctly navigates nested headline
paths and only matches headlines at the appropriate hierarchy level."
  ;; Try to rename "First Parent/Target Headline"
  ;; But there's no Target Headline under First Parent!
  ;; The function should fail, but it might incorrectly
  ;; find Third Parent's Target Headline
  ;; This should throw an error because First Parent has no Target Headline
  (org-mcp-test--call-rename-headline-expecting-error
   org-mcp-test--content-wrong-levels
   "First%20Parent/Target%20Headline"
   "Target Headline"
   "Renamed Target Headline"))

(ert-deftest org-mcp-test-rename-headline-by-id ()
  "Test renaming a headline accessed by org-id URI."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-nested-siblings
   org-mcp-test--content-with-id-uri
   "Second Child"
   "Renamed Second Child"
   org-mcp-test--expected-regex-renamed-second-child
   `(,org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-rename-headline-id-not-found ()
  "Test error when ID doesn't exist."
  (let ((org-id-track-globally nil)
        (org-id-locations-file nil))
    (org-mcp-test--call-rename-headline-expecting-error
     org-mcp-test--content-nested-siblings
     "org-id://non-existent-id-12345"
     "Whatever"
     "Should Fail")))

(ert-deftest org-mcp-test-rename-headline-with-slash ()
  "Test renaming a headline containing a slash character.
Slashes must be properly URL-encoded to avoid path confusion."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-slash-not-nested-before
   "Parent%2FChild"
   "Parent/Child"
   "Parent/Child Renamed"
   org-mcp-test--pattern-renamed-slash-headline))

(ert-deftest org-mcp-test-rename-headline-slash-not-nested ()
  "Test that headline with slash is not treated as nested path.
Verifies that 'Parent/Child' is treated as a single headline,
not as Child under Parent."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-slash-not-nested-before
   "Parent%2FChild"
   "Parent/Child"
   "Parent-Child Renamed"
   org-mcp-test--regex-slash-not-nested-after))

(ert-deftest org-mcp-test-rename-headline-with-percent ()
  "Test renaming a headline containing a percent sign.
Percent signs must be properly URL-encoded to avoid double-encoding issues."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-nested-siblings
   "Parent%20Task/First%20Child%2050%25%20Complete"
   "First Child 50% Complete"
   "First Child 75% Complete"
   org-mcp-test--regex-percent-after))

(ert-deftest org-mcp-test-rename-headline-reject-empty-string ()
  "Test that renaming to an empty string is rejected."
  (org-mcp-test--assert-rename-headline-rejected
   "* Important Task
This task has content."
   "Important Task" ""))

(ert-deftest org-mcp-test-rename-headline-reject-whitespace-only ()
  "Test that renaming to whitespace-only is rejected."
  (org-mcp-test--assert-rename-headline-rejected
   "* Another Task
More content."
   "Another Task" "   "))

(ert-deftest org-mcp-test-rename-headline-reject-newline ()
  "Test that renaming to a title with embedded newline is rejected."
  (org-mcp-test--assert-rename-headline-rejected
   org-mcp-test--content-nested-siblings
   "Parent Task/First Child 50% Complete"
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-rename-headline-duplicate-first-match ()
  "Test that when multiple headlines have the same name, first match is renamed.
This test documents the first-match behavior when duplicate headlines exist."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-duplicate-headlines-before
   "Project%20Review"
   "Project Review"
   "Q1 Review"
   org-mcp-test--regex-duplicate-first-renamed))

(ert-deftest org-mcp-test-rename-headline-creates-id ()
  "Test that renaming a headline creates an Org ID and returns it."
  (let ((org-id-track-globally t)
        (org-id-locations-file (make-temp-file "test-org-id")))
    (org-mcp-test--call-rename-headline-and-check
     org-mcp-test--content-nested-siblings
     "Parent%20Task/Third%20Child%20%233"
     "Third Child #3"
     "Renamed Child"
     org-mcp-test--pattern-renamed-headline-with-id)))


(ert-deftest org-mcp-test-rename-headline-hierarchy ()
  "Test that headline hierarchy is correctly navigated.
Ensures that when searching for nested headlines, the function
correctly restricts search to the parent's subtree."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-hierarchy-before
   "Second%20Section/Target"
   "Target"
   "Renamed Target"
   org-mcp-test--regex-hierarchy-second-target-renamed))

(ert-deftest org-mcp-test-rename-headline-with-todo-keyword ()
  "Test that headlines with TODO keywords can be renamed.
The navigation function should find headlines even when they have TODO keywords."
  (org-mcp-test--call-rename-headline-and-check
   org-mcp-test--content-todo-keywords-before
   "Project%20Management/Review%20Documents"
   "Review Documents"
   "Q1 Planning Review"
   org-mcp-test--regex-todo-keywords-after))

;;; org-edit-body tests

(ert-deftest org-mcp-test-edit-body-single-line ()
  "Test org-edit-body tool for single-line replacement."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-edit-body-and-check
    test-file
    org-mcp-test--content-with-id-uri
    "Second child content."
    "Updated second child content."
    org-mcp-test--pattern-edit-body-single-line
    nil
    org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-edit-body-multiline ()
  "Test org-edit-body tool for multi-line replacement."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-todo
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-uri
     "Second line of content."
     "This has been replaced
with new multiline
content here."
     org-mcp-test--pattern-edit-body-multiline
     nil
     org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-edit-body-case-sensitive-match ()
  "Test that org-edit-body matches `old_body' case-sensitively.
The fixture body contains `hello world' (lowercase, first) and
`Hello world' (capitalized, second).  A request to replace
`Hello world' must touch only the capitalized occurrence;
case-insensitive matching would touch the lowercase line instead."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-case-mixed-body
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-uri
     "Hello world"
     "REPLACED"
     org-mcp-test--pattern-edit-body-case-sensitive
     nil
     org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-edit-body-multiple-without-replaceall ()
  "Test error for multiple occurrences without replace_all."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    (org-mcp-test--call-edit-body-expecting-error
     test-file "org-id://test-id" "occurrence of pattern" "REPLACED" nil)))

(ert-deftest org-mcp-test-edit-body-replace-all ()
  "Test org-edit-body tool with replace_all functionality."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    (org-mcp-test--call-edit-body-and-check
     test-file
     "org-id://test-id"
     "occurrence of pattern"
     "REPLACED"
     org-mcp-test--pattern-edit-body-replace-all
     t
     "test-id")))

(ert-deftest org-mcp-test-edit-body-replace-all-string-false ()
  "Test that wire-protocol string `\"false\"' is coerced to nil.
A client sending `{\"replace_all\": \"false\"}' (a string, not a
JSON boolean) delivers the literal Elisp string `\"false\"' at the
tool boundary, where `(not \"false\")' is nil and the
multiple-occurrence guard skips firing.  Removing the `\"false\"'
branch in `org-mcp--tool-edit-body' would let the string pass
through as truthy, silently bypassing the guard."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    ;; Should error because multiple occurrences exist
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     "org-id://test-id"
     "occurrence of pattern"
     "REPLACED"
     "false")))

(ert-deftest org-mcp-test-edit-body-replace-all-json-false ()
  "Test that a real MCP client's JSON `false' fires the safety guard.
Emacs's `json.el' decodes a JSON boolean `false' to the symbol
`:json-false' (the default value of `json-false'), NOT to nil.  A
real client sending `{\"replace_all\": false}' therefore delivers
`:json-false' at the tool boundary, where `(not :json-false)' is
nil and the multiple-occurrence guard skips firing -- silently
replacing every occurrence even though the client explicitly opted
out."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-repeated-text
      `("test-id")
    ;; Should error because multiple occurrences exist
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     "org-id://test-id"
     "occurrence of pattern"
     "REPLACED"
     :json-false)))

(ert-deftest org-mcp-test-edit-body-not-found ()
  "Test org-edit-body tool error when text is not found."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "nonexistent text"
     "replacement"
     nil)))

(ert-deftest org-mcp-test-edit-body-empty ()
  "Test org-edit-body tool can add content to empty body."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (let ((resource-uri
           (format "org-headline://%s#Parent%%20Task/Third%%20Child%%20%%233"
                   test-file)))
      (org-mcp-test--call-edit-body-and-check
       test-file
       resource-uri
       ""
       "New content added."
       org-mcp-test--pattern-edit-body-empty))))

(ert-deftest org-mcp-test-edit-body-empty-old-non-empty-body ()
  "Test error when old_body is empty but body has content."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "" ; Empty old_body
     "replacement"
     nil)))

(ert-deftest org-mcp-test-edit-body-empty-with-properties ()
  "Test adding content to empty body with properties drawer."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-no-body
      `(,org-mcp-test--timestamp-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "org-id://%s" org-mcp-test--timestamp-id)
     ""
     "Content added after properties."
     org-mcp-test--pattern-edit-body-empty-with-props
     nil
     org-mcp-test--timestamp-id)))

(ert-deftest
    org-mcp-test-edit-body-empty-with-deeper-heading-returns-target-uri ()
  "Test that empty-body edit with new content containing a deeper
heading returns the edit target's URI, not the deeper heading's.
The empty branch of `replace-body-content' has different
save-excursion marker mechanics from the non-empty branch covered
by `edit-body-accept-lower-level-headline'.  Combined with the
deeper-heading flavor of the URI-return bug, this exercises a code
path neither `edit-body-empty' (no deeper heading in body) nor
`edit-body-accept-lower-level-headline' (non-empty body) hits."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-with-id-no-body
      `(,org-mcp-test--timestamp-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "org-id://%s" org-mcp-test--timestamp-id)
     ""
     "intro text\n** Sub heading"
     org-mcp-test--pattern-edit-body-empty-with-deeper-heading
     nil
     org-mcp-test--timestamp-id)))

(ert-deftest org-mcp-test-edit-body-nested-headlines ()
  "Test that body edit on a parent returns the parent's URI and
preserves its child subtree.  The first-child flavor of the
URI-return bug: after body insertion, point landed on `** First
Child' (the parent's first child) and `org-id-get-create' attached
a fresh `:ID:' to it, returning that URI instead of the parent's
existing one.  Save-excursion in `tool-edit-body' now preserves
point on the parent so its existing `:ID:' is returned."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-and-check
     test-file
     (format "org-headline://%s#Parent%%20Task" test-file)
     "Some parent content."
     "Updated parent content"
     org-mcp-test--pattern-edit-body-nested-headlines
     nil
     org-mcp-test--content-nested-siblings-parent-id)))

(ert-deftest org-mcp-test-edit-body-reject-headline-in-middle ()
  "Test org-edit-body rejects new_body with headline marker in middle."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "replacement text
* This would become a headline"
     nil)))

(ert-deftest org-mcp-test-edit-body-accept-lower-level-headline ()
  "Test that body containing a strictly-deeper heading is accepted
and that the returned URI points to the edit target, not to the
body-internal heading.  `validate-body-no-headlines' only rejects
headings at or above the target level, so a level-3 sub-heading
inside a level-2 edit is legitimate child structure.  After
insertion, point can land past the deeper heading;
`org-id-get-create' in `complete-and-save' must still attach `:ID:'
to the edit target and return the target's URI -- otherwise the
deeper heading captures the URI and a fresh `:ID:' drawer."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-and-check
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "some text
*** Subheading content"
     org-mcp-test--pattern-edit-body-accept-lower-level
     nil
     org-mcp-test--content-with-id-id)))

(ert-deftest org-mcp-test-edit-body-reject-higher-level-headline ()
  "Test org-edit-body rejects new_body with higher-level headline.
When editing a level 2 node, level 1 headlines should be rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-nested-siblings))
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     (format "org-headline://%s#Parent%%20Task/Second%%20Child"
             test-file)
     "Second child content."
     "New text
* Top level heading"
     nil)))

(ert-deftest org-mcp-test-edit-body-reject-headline-at-start ()
  "Test org-edit-body rejects new_body with headline at beginning."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "* Heading at start"
     nil)))

(ert-deftest org-mcp-test-edit-body-reject-unbalanced-begin-block ()
  "Test org-edit-body rejects new_body with unbalanced BEGIN block."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Some text
#+BEGIN_EXAMPLE
Code without END_EXAMPLE"
     nil)))

(ert-deftest org-mcp-test-edit-body-reject-orphaned-end-block ()
  "Test org-edit-body rejects new_body with orphaned END block."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "Second child content."
     "Some text
#+END_SRC
Without BEGIN_SRC"
     nil)))

(ert-deftest org-mcp-test-edit-body-reject-mismatched-blocks ()
  "Test org-edit-body rejects new_body with mismatched blocks."
  (org-mcp-test--with-id-setup
   test-file
   org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-edit-body-expecting-error
    test-file
    org-mcp-test--content-with-id-uri
    "Second child content."
    "Text here
#+BEGIN_QUOTE
Some quote
#+END_EXAMPLE"
    nil)))

;; org-read-file tests

(ert-deftest org-mcp-test-tool-read-file ()
  "Test org-read-file tool returns same content as file resource."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   (let ((result-text (org-mcp-test--call-read-file test-file)))
     (should (string= result-text org-mcp-test--content-nested-siblings)))))

;; org-read-outline tests

(ert-deftest org-mcp-test-tool-read-outline ()
  "Test org-read-outline tool returns valid JSON outline structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   (let* ((result (org-mcp-test--call-read-outline test-file))
          (headings (alist-get 'headings result)))
     (should (= (length headings) 1))
     (should (string= (alist-get 'title (aref headings 0)) "Parent Task")))))

;; org-read-headline test

(ert-deftest org-mcp-test-tool-read-headline-empty-path ()
  "Test org-read-headline with empty `headline_path' signals validation error.
Anchors on the full `Field headline_path must be non-empty; use
org-read-file tool to read entire files' wording so the
wire-protocol error contract is locked: a regression that
reintroduces the legacy `Parameter X must ...' shape or drops the
recovery hint surfaces as a test failure."
  (org-mcp-test--with-temp-org-files
      ((test-file ""))
    (let* ((request
            (mcp-server-lib-create-tools-call-request
             "org-read-headline" 1
             `((file . ,test-file) (headline_path . ""))))
           (response
            (mcp-server-lib-process-jsonrpc-parsed
             request mcp-server-lib-ert-server-id))
           (err
            (should-error
             (mcp-server-lib-ert-process-tool-response response)
             :type 'mcp-server-lib-tool-error)))
      (should
       (string-match-p
        (regexp-quote
         "Field headline_path must be non-empty; use \
org-read-file tool to read entire files")
        (error-message-string err))))))

(ert-deftest org-mcp-test-tool-read-headline-single-level ()
  "Test org-read-headline with single-level path."
  (org-mcp--tool-read-headline-and-check
   org-mcp-test--content-slash-not-nested-before
   "Parent%2FChild"
   org-mcp-test--pattern-tool-read-headline-single))

(ert-deftest org-mcp-test-tool-read-headline-nested ()
  "Test org-read-headline with nested path."
  (org-mcp--tool-read-headline-and-check
   org-mcp-test--content-nested-siblings
   "Parent%20Task/First%20Child%2050%25%20Complete"
   org-mcp-test--pattern-tool-read-headline-nested))

(ert-deftest org-mcp-test-tool-read-by-id ()
  "Test org-read-by-id tool returns headline content by ID."
  (org-mcp-test--with-id-setup
   test-file org-mcp-test--content-nested-siblings
   `(,org-mcp-test--content-with-id-id)
   (org-mcp-test--call-read-by-id-and-check
    org-mcp-test--content-with-id-id
    org-mcp-test--pattern-tool-read-by-id)))

(ert-deftest org-mcp-test-tool-read-by-id-falls-back-to-allowed-files-scan ()
  "Test `org-read-by-id' resolves an ID absent from `org-id-locations'.
Locks the read-only side of the ID-resolution contract: when
`org-id-find-id-file' misses but the ID is present in an allowed
file (e.g. tracking just enabled, locations file stale or missing,
or the ID added by an external edit), `org-read-by-id' must scan
`org-mcp-allowed-files' and resolve the URI, matching the behaviour
of every modifying tool.  Without this regression test, a fix that
only routes one of the two paths through the shared lookup helper
would still let every existing read-by-id test pass because they
all pre-register the ID via `with-id-setup'."
  (org-mcp-test--with-temp-org-files
      ((test-file org-mcp-test--content-with-id-todo))
    (let ((org-id-track-globally t)
          (org-id-locations-file nil)
          (org-id-locations nil)
          (org-mcp-allowed-files (list test-file)))
      (should-not
       (org-id-find-id-file org-mcp-test--content-with-id-id))
      (org-mcp-test--call-read-by-id-and-check
       org-mcp-test--content-with-id-id
       (format
        (concat
         "\\`\\* TODO Task with ID\n"
         ":PROPERTIES:\n"
         ":ID: +%s\n"
         ":END:\n"
         "First line of content\\.\n"
         "Second line of content\\.\n"
         "Third line of content\\.?\\'")
        org-mcp-test--content-with-id-id)))))

;; Resource tests

(ert-deftest org-mcp-test-file-resource-template-in-list ()
  "Test that file template appears in resources/templates/list."
  (let ((org-mcp-allowed-files '("test.org")))
    (org-mcp-test--with-enabled
     (let ((templates
            (mcp-server-lib-ert-get-resource-templates-list)))
       ;; Check that we have four templates now
       (should (= (length templates) 4))
       ;; Check that we have all templates
       (let ((template-uris
              (mapcar
               (lambda (template)
                 (alist-get 'uriTemplate template))
               (append templates nil))))
         (should (member "org://{filename}" template-uris))
         (should (member "org-outline://{filename}" template-uris))
         (should (member "org-headline://{filename}" template-uris))
         (should (member "org-id://{uuid}" template-uris)))))))

(ert-deftest org-mcp-test-file-resource-not-in-list-after-disable ()
  "Test that resources are unregistered after `org-mcp-disable'."
  (let ((org-mcp-allowed-files '("test.org")))
    ;; Enable then disable
    (org-mcp-enable)
    (org-mcp-disable)
    ;; Start server and check resources
    (mcp-server-lib-ert-with-server
      :tools nil
      :resources nil
      (let ((resources (mcp-server-lib-ert-get-resource-list)))
        ;; Check that the resource list is empty
        (should (= (length resources) 0))))))

(ert-deftest org-mcp-test-file-resource-read ()
  "Test that reading a resource returns file content."
  (let ((test-content "* Test Heading\nThis is test content."))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((uri (format "org://%s" test-file)))
       (org-mcp-test--verify-resource-read
        uri
        test-content)))))

(ert-deftest org-mcp-test-outline-resource-returns-structure ()
  "Test that outline resource returns document structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-headline-resource))
   (let* ((uri (format "org-outline://%s" test-file))
          (request
            (mcp-server-lib-create-resources-read-request uri))
          (response-json
           (mcp-server-lib-process-jsonrpc request mcp-server-lib-ert-server-id))
          (response
           (json-parse-string response-json
                              :object-type 'alist))
          (result (alist-get 'result response))
          (contents (alist-get 'contents result)))
     (when (alist-get 'error response)
       (error
        "Resource request failed: %s"
        (alist-get 'message (alist-get 'error response))))
     (let* ((outline-json (alist-get 'text (aref contents 0)))
            (outline
             (json-parse-string outline-json
                                :object-type 'alist))
            (headings (alist-get 'headings outline)))
       (should (= (length headings) 2))
       (let ((first (aref headings 0)))
         (should
          (equal (alist-get 'title first) "First Section"))
         (should (= (alist-get 'level first) 1))
         (let ((children (alist-get 'children first)))
           (should (= (length children) 2))
           (should
            (equal
             (alist-get 'title (aref children 0))
             "Subsection 1.1"))
           (should
            (= (length (alist-get 'children (aref children 0))) 0))
           (should
            (equal
             (alist-get 'title (aref children 1))
             "Subsection 1.2"))
           (should
            (= (length (alist-get 'children (aref children 1))) 0))))
       (let ((second (aref headings 1)))
         (should
          (equal (alist-get 'title second) "Second Section"))
         (should (= (alist-get 'level second) 1))
         ;; Deep subsection is empty (level 3 under level 1)
         (should
          (= (length (alist-get 'children second)) 0)))))))

(ert-deftest org-mcp-test-headline-resource-returns-top-level-content ()
  "Test that headline resource returns top-level headline content."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-headline-resource))
   (let ((uri
          (format "org-headline://%s#First%%20Section"
                  test-file)))
     (org-mcp-test--verify-resource-read
      uri
      org-mcp-test--expected-first-section))))

(ert-deftest org-mcp-test-headline-resource-empty-fragment ()
  "Test that `org-headline://FILE#' returns the full file content.
Locks the contract that an empty fragment in an `org-headline://'
URI is equivalent to no fragment at all, so the resource handler
returns the entire file rather than erroring on a `(\"\")' headline
path produced by `split-string' on the empty fragment string."
  (let ((test-content "* Test Heading\nThis is test content."))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((uri (format "org-headline://%s#" test-file)))
       (org-mcp-test--verify-resource-read uri test-content)))))

(ert-deftest org-mcp-test-headline-resource-returns-nested-content ()
  "Test that headline resource returns nested headline content."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-headline-resource))
   (let ((uri
          (format (concat
                   "org-headline://%s#"
                   "First%%20Section/Subsection%%201.1")
                  test-file)))
     (org-mcp-test--verify-resource-read
      uri
      org-mcp-test--expected-subsection-1-1))))

(ert-deftest org-mcp-test-headline-resource-not-found ()
  "Test headline resource error for non-existent headline."
  (let ((test-content "* Existing Section\nSome content."))
    (org-mcp-test--with-temp-org-files
     ((test-file test-content))
     (let ((uri
            (format "org-headline://%s#Nonexistent" test-file)))
       (org-mcp-test--read-resource-expecting-error
        uri "Headline not found: 'Nonexistent'")))))

(ert-deftest org-mcp-test-headline-resource-file-with-hash ()
  "Test headline resource with # in filename."
  (org-mcp-test--with-temp-org-files
   ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
   ;; Test accessing the file with # encoded as %23
   (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
          (uri
           (format "org-headline://%s#Parent%%20Task/First%%20Child%%2050%%25%%20Complete"
                   encoded-path)))
     (org-mcp-test--verify-resource-read
      uri
      "** First Child 50% Complete\nFirst child content.\nIt spans multiple lines."))))

(ert-deftest org-mcp-test-headline-resource-headline-with-hash ()
  "Test headline resource with # in headline title."
  (let ((test-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--with-temp-org-files
     ((file test-content))
     ;; Test accessing headline with # encoded as %23
     (let ((uri
            (format "org-headline://%s#Parent%%20Task/Third%%20Child%%20%%233"
                    file)))
       (org-mcp-test--verify-resource-read
        uri
        "** Third Child #3")))))

(ert-deftest
    org-mcp-test-headline-resource-file-and-headline-with-hash
    ()
  "Test headline resource with # in both filename and headline."
  (org-mcp-test--with-temp-org-files
   ((file org-mcp-test--content-nested-siblings "org-mcp-test-file#"))
   ;; Test with both file and headline containing #
   (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
          (uri
           (format "org-headline://%s#Parent%%20Task/Third%%20Child%%20%%233"
                   encoded-path)))
     (org-mcp-test--verify-resource-read
      uri
      "** Third Child #3"))))

(ert-deftest org-mcp-test-headline-resource-txt-extension ()
  "Test that headline resource works with .txt files, not just .org files."
  (org-mcp-test--test-headline-resource-with-extension ".txt"))

(ert-deftest org-mcp-test-headline-resource-no-extension ()
  "Test that headline resource works with files having no extension."
  (org-mcp-test--test-headline-resource-with-extension nil))

(ert-deftest org-mcp-test-headline-resource-path-traversal ()
  "Test that path traversal with ../ in org-headline URIs is rejected."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   ;; Test with ../ in the filename part
   (let ((uri
          (format "org-headline://../%s#Parent%%20Task"
                  (file-name-nondirectory test-file))))
     (org-mcp-test--read-resource-expecting-error
      uri
      (format "Path must be absolute: ../%s"
              (file-name-nondirectory test-file))))))

(ert-deftest org-mcp-test-headline-resource-encoded-path-traversal ()
  "Test that URL-encoded path traversal in org-headline URIs is rejected."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   ;; Test with URL-encoded ../ (%2E%2E%2F) in the filename part
   ;; The encoding is NOT decoded, so %2E%2E%2F remains literal
   (let ((uri
          (format "org-headline://%%2E%%2E%%2F%s#Parent%%20Task"
                  (file-name-nondirectory test-file))))
     (org-mcp-test--read-resource-expecting-error
      uri
      (format "Path must be absolute: %%2E%%2E%%2F%s"
              (file-name-nondirectory test-file))))))

(ert-deftest org-mcp-test-headline-resource-navigation ()
  "Test that headline navigation respects structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-wrong-levels))
   ;; Test accessing "Target Headline" under "First Parent"
   ;; Should get the level-2 headline, NOT the level-3 one
   (let ((uri
          (format
           "org-headline://%s#First%%20Parent/Target%%20Headline"
           test-file)))
     ;; This SHOULD throw an error because First Parent has no such child
     ;; But the bug causes it to return the wrong headline
     (org-mcp-test--read-resource-expecting-error
      uri
      "Headline not found: 'First Parent/Target Headline'"))))

(ert-deftest org-mcp-test-id-resource-returns-content ()
  "Test that ID resource returns content for valid ID."
  (org-mcp-test--with-id-setup
   test-file org-mcp-test--content-id-resource
   `(,org-mcp-test--content-id-resource-id)
   (let ((uri (format "org-id://%s" org-mcp-test--content-id-resource-id)))
     (org-mcp-test--verify-resource-read
      uri
      org-mcp-test--content-id-resource))))

(ert-deftest org-mcp-test-id-resource-not-found ()
  "Test ID resource error for non-existent ID."
  (let ((test-content "* Section without ID\nNo ID here."))
    (org-mcp-test--with-id-setup test-file test-content '()
                                 (let ((uri "org-id://nonexistent-id-12345"))
                                   (org-mcp-test--read-resource-expecting-error
                                    uri "ID not found: 'nonexistent-id-12345'")))))

(ert-deftest org-mcp-test-id-resource-file-not-allowed ()
  "Test ID resource validates file is in allowed list."
  ;; Create two files - one allowed, one not
  (org-mcp-test--with-temp-org-files
   ((allowed-file "* Allowed\n")
    (other-file org-mcp-test--content-id-resource))
   (org-mcp-test--with-id-tracking
    (list allowed-file)
    `((,org-mcp-test--content-id-resource-id . ,other-file))
    (let ((uri (format "org-id://%s" org-mcp-test--content-id-resource-id)))
      ;; Should get an error for ID resolving to a non-allowed file
      (org-mcp-test--read-resource-expecting-error
       uri
       (format "ID '%s' resolves to a file not in the allowed list"
               org-mcp-test--content-id-resource-id))))))

(defconst org-mcp-test--repo-root
  (file-name-directory
   (or load-file-name buffer-file-name (expand-file-name "./")))
  "Repository root directory, captured at test-file load time.
Used by repo-level meta tests that read project files (Eask,
`org-mcp.el') by name rather than via `org-mcp-allowed-files'.")

(defun org-mcp-test--read-repo-file (relative-name)
  "Return the contents of RELATIVE-NAME under the repository root."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative-name org-mcp-test--repo-root))
    (buffer-string)))

(ert-deftest org-mcp-test-meta-version-strings-agree ()
  "Test that the package version is consistent across all sites.
The `Eask' file's `(package ...)' form, `org-mcp.el's `;; Version:'
header, and `NEWS' top-section heading must all report the same
version; drift between them leads to MELPA/Eask metadata
inconsistencies at release time and to changelog/release-state
confusion."
  (let* ((eask-content (org-mcp-test--read-repo-file "Eask"))
         (el-content (org-mcp-test--read-repo-file "org-mcp.el"))
         (news-content (org-mcp-test--read-repo-file "NEWS"))
         (eask-version
          (and (string-match
                "(package[ \t\n]+\"[^\"]+\"[ \t\n]+\"\\([^\"]+\\)\""
                eask-content)
               (match-string 1 eask-content)))
         (el-version
          (and (string-match
                "^;;[ \t]+Version:[ \t]+\\(\\S-+\\)"
                el-content)
               (match-string 1 el-content)))
         (news-version
          (and (string-match
                "^\\* Changes in org-mcp \\(\\S-+\\)"
                news-content)
               (match-string 1 news-content))))
    (should (stringp eask-version))
    (should (stringp el-version))
    (should (stringp news-version))
    (should (equal eask-version el-version))
    (should (equal eask-version news-version))))

;; Uniform type validation: every wire-protocol string parameter must
;; be rejected at the tool boundary when a non-string is supplied.
;; The validation happens before any file or URI is parsed, so a
;; throwaway temp file suffices for setup.

(defmacro org-mcp-test--assert-tool-non-string (tool-name params)
  "Assert calling TOOL-NAME with PARAMS errors at the tool boundary.
Sets up the standard test harness (a dummy temp file is allowed but
unused — validation fires before any file is touched).  Uses the
low-level JSON-RPC pipeline so a tool-error response surfaces as a
`mcp-server-lib-tool-error' signal we can match on."
  `(org-mcp-test--with-temp-org-files
       ((test-file ""))
     (should-error
      (let* ((request
              (mcp-server-lib-create-tools-call-request
               ,tool-name 1 ,params))
             (response
              (mcp-server-lib-process-jsonrpc-parsed
               request mcp-server-lib-ert-server-id)))
        (mcp-server-lib-ert-process-tool-response response))
      :type 'mcp-server-lib-tool-error)))

(ert-deftest org-mcp-test-add-todo-non-string-title ()
  "Test that a non-string `title' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-add-todo"
   `((title . 42) (todo_state . "TODO") (tags . nil) (body . nil)
     (parent_uri . "org-headline:///tmp/x.org#"))))

(ert-deftest org-mcp-test-add-todo-non-string-todo-state ()
  "Test that a non-string `todo_state' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-add-todo"
   `((title . "T") (todo_state . 42) (tags . nil) (body . nil)
     (parent_uri . "org-headline:///tmp/x.org#"))))

(ert-deftest org-mcp-test-add-todo-non-string-body ()
  "Test that a non-string `body' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-add-todo"
   `((title . "T") (todo_state . "TODO") (tags . nil) (body . 42)
     (parent_uri . "org-headline:///tmp/x.org#"))))

(ert-deftest org-mcp-test-add-todo-non-string-parent-uri ()
  "Test that a non-string `parent_uri' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-add-todo"
   `((title . "T") (todo_state . "TODO") (tags . nil) (body . nil)
     (parent_uri . 42))))

(ert-deftest org-mcp-test-add-todo-non-string-after-uri ()
  "Test that a non-string `after_uri' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-add-todo"
   `((title . "T") (todo_state . "TODO") (tags . nil) (body . nil)
     (parent_uri . "org-headline:///tmp/x.org#") (after_uri . 42))))

(ert-deftest org-mcp-test-update-todo-state-non-string-uri ()
  "Test that a non-string `uri' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-update-todo-state"
   `((uri . 42) (current_state . "TODO") (new_state . "DONE"))))

(ert-deftest org-mcp-test-update-todo-state-non-string-current-state ()
  "Test that a non-string `current_state' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-update-todo-state"
   `((uri . "org-id://x") (current_state . 42) (new_state . "DONE"))))

(ert-deftest org-mcp-test-update-todo-state-non-string-new-state ()
  "Test that a non-string `new_state' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-update-todo-state"
   `((uri . "org-id://x") (current_state . "TODO") (new_state . 42))))

(ert-deftest org-mcp-test-rename-headline-non-string-uri ()
  "Test that a non-string `uri' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-rename-headline"
   `((uri . 42) (current_title . "Old") (new_title . "New"))))

(ert-deftest org-mcp-test-rename-headline-non-string-current-title ()
  "Test that a non-string `current_title' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-rename-headline"
   `((uri . "org-id://x") (current_title . 42) (new_title . "New"))))

(ert-deftest org-mcp-test-rename-headline-non-string-new-title ()
  "Test that a non-string `new_title' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-rename-headline"
   `((uri . "org-id://x") (current_title . "Old") (new_title . 42))))

(ert-deftest org-mcp-test-edit-body-non-string-resource-uri ()
  "Test that a non-string `resource_uri' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-edit-body"
   `((resource_uri . 42) (old_body . "a") (new_body . "b")
     (replace_all . nil))))

(ert-deftest org-mcp-test-edit-body-non-string-old-body ()
  "Test that a non-string `old_body' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-edit-body"
   `((resource_uri . "org-id://x") (old_body . 42) (new_body . "b")
     (replace_all . nil))))

(ert-deftest org-mcp-test-edit-body-non-string-new-body ()
  "Test that a non-string `new_body' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-edit-body"
   `((resource_uri . "org-id://x") (old_body . "a") (new_body . 42)
     (replace_all . nil))))

(ert-deftest org-mcp-test-tool-read-file-non-string-file ()
  "Test that a non-string `file' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string "org-read-file" '((file . 42))))

(ert-deftest org-mcp-test-tool-read-outline-non-string-file ()
  "Test that a non-string `file' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string "org-read-outline" '((file . 42))))

(ert-deftest org-mcp-test-tool-read-headline-non-string-file ()
  "Test that a non-string `file' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-read-headline"
   `((file . 42) (headline_path . "P"))))

(ert-deftest org-mcp-test-tool-read-headline-non-string-path ()
  "Test that a non-string `headline_path' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string
   "org-read-headline"
   `((file . "/tmp/x.org") (headline_path . 42))))

(ert-deftest org-mcp-test-tool-read-by-id-non-string-uuid ()
  "Test that a non-string `uuid' is rejected at the tool boundary."
  (org-mcp-test--assert-tool-non-string "org-read-by-id" '((uuid . 42))))

;; File-header validation: every modifying tool must reject an
;; unterminated drawer in the file's header block (any keyword), and
;; distinguish "heading inside drawer" from a plain unterminated
;; drawer in the diagnostic.

(ert-deftest org-mcp-test-update-todo-state-unterminated-drawer ()
  "Test `org-update-todo-state' rejects a file with a malformed header."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-with-todo-and-body))
    (org-mcp-test--call-update-todo-state-expecting-error
     test-file
     (format "org-headline://%s#Existing%%20Heading" test-file)
     "TODO" "DONE")))

(ert-deftest org-mcp-test-rename-headline-unterminated-drawer ()
  "Test `org-rename-headline' rejects a file with a malformed header."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-with-todo-and-body))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((request
              (mcp-server-lib-create-tools-call-request
               "org-rename-headline" 1
               `((uri . ,(format "org-headline://%s#Existing%%20Heading"
                                 test-file))
                 (current_title . "Existing Heading")
                 (new_title . "Renamed"))))
            (response
             (mcp-server-lib-process-jsonrpc-parsed
              request mcp-server-lib-ert-server-id)))
       (mcp-server-lib-ert-process-tool-response response)))))

(ert-deftest org-mcp-test-edit-body-unterminated-drawer ()
  "Test `org-edit-body' rejects a file with a malformed header."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-with-todo-and-body))
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     (format "org-headline://%s#Existing%%20Heading" test-file)
     "Some body content."
     "Replaced.")))

(ert-deftest org-mcp-test-add-todo-unterminated-drawer ()
  "Test `org-add-todo' rejects a file with a malformed header."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-with-todo-and-body))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((params
             (org-mcp-test--build-add-todo-params
              "New Task" "TODO" '("work") nil
              (format "org-headline://%s#" test-file)))
            (request
             (mcp-server-lib-create-tools-call-request
              "org-add-todo" nil params))
            (response
             (mcp-server-lib-process-jsonrpc-parsed
              request mcp-server-lib-ert-server-id)))
       (mcp-server-lib-ert-process-tool-response response)))))

(ert-deftest org-mcp-test-add-todo-unterminated-logbook-drawer ()
  "Test that any drawer keyword (not just `:PROPERTIES:') is recognised.
A `:LOGBOOK:' drawer without `:END:' must be rejected with the same
class of error as an unterminated `:PROPERTIES:' drawer."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-logbook-unterminated))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((params
             (org-mcp-test--build-add-todo-params
              "New Task" "TODO" '("work") nil
              (format "org-headline://%s#" test-file)))
            (request
             (mcp-server-lib-create-tools-call-request
              "org-add-todo" nil params))
            (response
             (mcp-server-lib-process-jsonrpc-parsed
              request mcp-server-lib-ert-server-id)))
       (mcp-server-lib-ert-process-tool-response response)))))

(ert-deftest org-mcp-test-add-todo-heading-in-drawer ()
  "Test that a heading line trapped inside a header-block drawer is rejected."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-heading-in-drawer))
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((params
             (org-mcp-test--build-add-todo-params
              "New Task" "TODO" '("work") nil
              (format "org-headline://%s#" test-file)))
            (request
             (mcp-server-lib-create-tools-call-request
              "org-add-todo" nil params))
            (response
             (mcp-server-lib-process-jsonrpc-parsed
              request mcp-server-lib-ert-server-id)))
       (mcp-server-lib-ert-process-tool-response response)))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
