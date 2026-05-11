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

(defconst org-mcp-test--content-title-only
  "#+TITLE: Test Document
"
  "File with `#+TITLE:' header only and no headings.")

(defconst org-mcp-test--content-title-and-paragraph
  "#+TITLE: Test Document
Some unstructured paragraph.
"
  "File with `#+TITLE:' followed by a plain paragraph and no headings.")

(defconst org-mcp-test--agenda-basic-content
  "* TODO Test agenda line
SCHEDULED: <2026-04-26>
"
  "Single TODO task scheduled on 2026-04-26.")

(defconst org-mcp-test--agenda-month-content
  "* TODO Early April task
SCHEDULED: <2026-04-03>
* TODO On reference day
SCHEDULED: <2026-04-26>
* TODO Next month task
SCHEDULED: <2026-05-10>
"
  "Three scheduled tasks spanning the April-May 2026 month
boundary: 2026-04-03, 2026-04-26, and 2026-05-10.")

(defconst org-mcp-test--agenda-two-task-content
  "* TODO First agenda task
SCHEDULED: <2026-04-26>
* TODO Second agenda task
SCHEDULED: <2026-04-26>
"
  "Two sibling TODO tasks both scheduled on 2026-04-26.")

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

(defconst org-mcp-test--content-file-with-lowercase-properties-drawer
  "#+TITLE: Test Document
:properties:
:CATEGORY: work
:end:
* Existing Heading"
  "File with a lowercase `:properties:'...`:end:' drawer.")

(defconst org-mcp-test--content-file-with-unterminated-drawer
  "#+TITLE: Test Document
:PROPERTIES:
:CATEGORY: work
* Existing Heading
"
  "File with an unterminated file-level `:PROPERTIES:' drawer (no `:END:').")

(defconst org-mcp-test--content-file-with-heading-in-drawer
  "#+TITLE: Test Document
:PROPERTIES:
* Bogus Heading Inside Drawer
:END:
* Real Heading
"
  "File with a heading line inside what looks like a file-level drawer.")

(defconst org-mcp-test--content-file-with-eof-unterminated-drawer
  "#+TITLE: Test Document
:PROPERTIES:
:CATEGORY: work
"
  "File whose `:PROPERTIES:' drawer body runs to end of file without an
`:END:' line and without a heading line.")

(defconst org-mcp-test--content-file-with-end-typo-then-real-end
  "#+TITLE: Test Document
:PROPERTIES:
:CATEGORY: work
:END:typo
:END:
* Existing Heading
"
  "File whose `:PROPERTIES:' drawer body contains a `:END:typo' line
before the real `:END:'.")

(defconst org-mcp-test--content-file-with-properties-opener-stray
  "#+TITLE: Test Document
:PROPERTIES: stray text
:END:
* Existing Heading
"
  "File whose `:PROPERTIES:' opener has stray text on the same line.")

(defconst org-mcp-test--content-file-with-properties-after-blank-line
  "#+TITLE: Test Document

:PROPERTIES:
:CATEGORY: work
:END:
* Existing Heading"
  "File where `:PROPERTIES:' follows `#+TITLE:' across a blank line.")

(defconst org-mcp-test--content-heading-with-trailing-newline
  "* Existing Heading
"
  "Minimal well-formed file: a single heading line followed by `\\n'.")

(defconst org-mcp-test--content-bare-headings-no-header
  "* Existing
* Other"
  "Two bare top-level headings with no header block.")

(defconst org-mcp-test--content-drawer-only-at-top
  ":PROPERTIES:
:CATEGORY: work
:END:
* Existing Heading"
  "File whose very first line is `:PROPERTIES:' (no `#+TITLE:' or
other keyword before it).")

(defconst org-mcp-test--content-drawer-only-eof-no-newline
  ":PROPERTIES:
:CATEGORY: work
:END:"
  "Drawer-only file with no heading; `:END:' is the last line and
the file ends mid-line (no trailing `\\n').  `point-max' coincides
with the end of `:END:'.")

(defconst org-mcp-test--content-file-with-unterminated-logbook
  "#+TITLE: Test Document
:LOGBOOK:
CLOCK: [2026-05-14 Wed 09:00]--[2026-05-14 Wed 10:30] =>  1:30
* Existing Heading
"
  "File with an unterminated file-level `:LOGBOOK:' drawer (no `:END:').")

(defconst org-mcp-test--content-file-with-unterminated-customdrawer
  "#+TITLE: Test Document
:CUSTOMDRAWER:
some content
* Existing Heading
"
  "File with an unterminated file-level custom-named drawer.")

(defconst org-mcp-test--content-file-with-unterminated-hyphen-digit-drawer
  "#+TITLE: Test Document
:MY-DRAWER_1:
some content
* Existing Heading
"
  "File with an unterminated file-level `:MY-DRAWER_1:' drawer (no `:END:').")

(defconst org-mcp-test--content-file-with-indented-properties
  "#+TITLE: Test Document
 :PROPERTIES:
 :CATEGORY: work
 :END:
* Existing Heading"
  "File with a one-space-indented `:PROPERTIES:' drawer at file level.")

(defconst org-mcp-test--content-file-with-hashtag-paragraph
  "#+TITLE: Test Document
#hashtag-paragraph
* Existing Heading"
  "File with a `#hashtag'-style column-0 line.")

(defconst org-mcp-test--content-file-with-stray-end-at-top
  "#+TITLE: Test Document
:END:
* Existing Heading"
  "File with a bare `:END:' at column 0 with no preceding drawer opener.")

(defconst org-mcp-test--content-title-and-heading
  "#+TITLE: Foo
* Existing"
  "Minimal file with a `#+TITLE:' keyword and one heading.")

(defconst org-mcp-test--content-orphan-deeper-before-top-level
  "#+TITLE: Foo
*** Orphan
* Real
"
  "File with an orphan level-3 heading preceding a level-1 heading.")

(defconst org-mcp-test--content-orphan-deeper-only
  "#+TITLE: Foo
*** Orphan
"
  "File whose only heading is an orphan level-3 (no level-1 heading).")

(defconst org-mcp-test--content-parent-with-body-no-children
  "* Parent Task
Some parent body text.
More body text."
  "Parent headline with body lines and no child headings.")

(defconst org-mcp-test--content-parent-with-body-trailing-newline
  "* Parent Task
Parent body.
"
  "Parent with body and no children, file ending with `\\n'.")

(defconst org-mcp-test--content-parent-drawer-only-no-children
  "* Parent Task
:PROPERTIES:
:CATEGORY: research
:END:"
  "Parent with a property drawer but no body and no children.")

(defconst org-mcp-test--content-parent-with-all-metadata-and-child
  "* Parent Task
SCHEDULED: <2026-05-15 Fri>
:PROPERTIES:
:CATEGORY: research
:END:
:LOGBOOK:
CLOCK: [2026-05-14 Wed 09:00]--[2026-05-14 Wed 10:30] =>  1:30
:END:
Some parent body text.
** Existing Child"
  "Parent with planning line, property drawer, logbook drawer, plain
body, and one child heading.")

(defconst org-mcp-test--content-parent-no-children-then-other-top
  "* Parent Task
Parent body.
* Other Top"
  "Parent with body but no children, followed by another top-level
heading.")

(defconst org-mcp-test--content-parent-single-child-then-other-top-id
  "after-uri-single-child-then-other-top-id-001"
  "ID for `Only Child' in `content-parent-single-child-then-other-top'.")

(defconst org-mcp-test--content-parent-single-child-then-other-top
  (format
   "* Parent Task
** Only Child
:PROPERTIES:
:ID:       %s
:END:
Child content.
* Other Top"
   org-mcp-test--content-parent-single-child-then-other-top-id)
  "Parent with one ID'd child followed by another top-level heading.")

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

(defconst org-mcp-test--content-ancestor-id-parent-no-id-ancestor-id
  "ancestor-id-parent-no-id-ancestor-001"
  "ID for `Ancestor' in `content-ancestor-id-parent-no-id'.")

(defconst org-mcp-test--content-ancestor-id-parent-no-id
  (format
   "* Ancestor
:PROPERTIES:
:ID:       %s
:END:
** Parent
*** First Child"
   org-mcp-test--content-ancestor-id-parent-no-id-ancestor-id)
  "Ancestor with `:ID:'; nested `Parent' has none; grandchild has none.")

(defconst org-mcp-test--content-two-parents-one-with-id-child-id
  "after-uri-wrong-parent-child-id-001"
  "ID for `Unrelated Child' in
`content-two-parents-one-with-id-child'.")

(defconst org-mcp-test--content-two-parents-one-with-id-child
  (format
   "* Childless Parent
Content of childless parent.
* Other Parent
** Unrelated Child
:PROPERTIES:
:ID:       %s
:END:
Content of unrelated child."
   org-mcp-test--content-two-parents-one-with-id-child-id)
  "Two top-level parents; the second (`Other Parent') has a child
(`Unrelated Child') with an `:ID:', the first (`Childless Parent')
has body but no children.")

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

(defconst org-mcp-test--content-leading-source-block-with-drawer-syntax
  "#+BEGIN_SRC text
:LOGBOOK:
#+END_SRC
* Real Heading
"
  "File with a leading `#+BEGIN_SRC' block whose body contains a
`:LOGBOOK:'-looking line that is literal source content, not a
real drawer.  Followed by a single top-level heading.")

(defconst org-mcp-test--content-leading-source-block-unterminated
  "#+BEGIN_SRC text
some content
* Real Heading
"
  "File with an unterminated `#+BEGIN_SRC' block at file head: no
matching `#+END_SRC' before the first heading or end of buffer.")

(defconst org-mcp-test--content-leading-dynamic-block-with-drawer-syntax
  "#+BEGIN: clocktable :scope file
:LOGBOOK:
#+END:
* Real Heading
"
  "File with a leading dynamic block (`#+BEGIN: ... #+END:'
colon-terminated, no underscore) whose body contains a
`:LOGBOOK:'-looking line that is literal block content, not a
real drawer.  Followed by a single top-level heading.")

(defconst org-mcp-test--content-leading-block-with-dotted-name
  "#+BEGIN_my.block
:LOGBOOK:
#+END_my.block
* Real Heading
"
  "File with a leading named block whose name (`my.block') contains
a `.', which Org's parser accepts but a narrower `[-_[:alnum:]]'
character class would reject.  Body contains a `:LOGBOOK:'-looking
line that is literal block content, not a real drawer.")

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

(defconst org-mcp-test--expected-regex-top-level-with-header-end
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
  "Regex for top-level TODO appended at end of file (default/`end' position).")

(defconst org-mcp-test--expected-regex-top-level-with-header-start
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
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
   "\\*\\* Third Child #3\\'")
  "Regex for top-level TODO inserted before first heading (`start' position).")

(defconst org-mcp-test--regex-top-level-start-after-lowercase-properties
  (concat
   "\\`#\\+TITLE: Test Document\n"
   ":properties:\n"
   ":CATEGORY: work\n"
   ":end:\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\\'")
  "Whole-file regex for a `start' insert after a lowercase
`:properties:'/`:end:' file-level drawer.  The preserved file-level
drawer keeps its lowercase keywords; the new heading's auto-generated
ID drawer uses uppercase `:PROPERTIES:'/`:END:', distinguishing the
two drawers in the regex.")

(defconst org-mcp-test--regex-top-level-start-after-end-typo
  (concat
   "\\`#\\+TITLE: Test Document\n"
   ":PROPERTIES:\n"
   ":CATEGORY: work\n"
   ":END:typo\n"
   ":END:\n"
   "\\* TODO New Task +.*:work:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\n\\'")
  "Regex for a top-level `start' insert into a file whose drawer
contains a `:END:typo' line before the real `:END:'.")

(defconst org-mcp-test--regex-top-level-start-after-properties-stray
  (concat
   "\\`#\\+TITLE: Test Document\n"
   ":PROPERTIES: stray text\n"
   ":END:\n"
   "\\* TODO New Task +.*:work:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\n\\'")
  "Whole-file regex for a file with `:PROPERTIES: stray text\\n:END:\\n*
heading' after a `start' insert.  New heading lands between the stray
drawer-like paragraph and the existing heading.")

(defconst org-mcp-test--regex-top-level-start-after-properties-blank-line
  (concat
   "\\`#\\+TITLE: Test Document\n"
   "\n"
   ":PROPERTIES:\n"
   ":CATEGORY: work\n"
   ":END:\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\\'")
  "Whole-file regex for a file with `#+TITLE:', blank line, `:PROPERTIES:'
drawer, then a heading, after a `start' insert.  New heading lands
after the drawer.")

(defconst org-mcp-test--regex-top-level-end-single-newline
  (concat
   "\\`\\* Existing Heading\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Whole-file regex with `* Existing Heading' followed immediately
by `* TODO New Top Task' (exactly one `\\n' between -- no spurious
blank line).")

(defconst org-mcp-test--regex-child-end-single-newline
  (concat
   "\\`\\* Parent Task\n"
   "Parent body.\n"
   "\\*\\* TODO New Child +.*:work:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Whole-file regex with `Parent body.' followed immediately by
`** TODO New Child' (exactly one `\\n' between -- no spurious blank
line).  `* Parent Task' is the file's last subtree.")

(defconst org-mcp-test--regex-child-end-with-body-single-newline
  (concat
   "\\`\\* Parent Task\n"
   "Parent body.\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n\\'")
  "Whole-file regex with `\\* Parent Task' / `Parent body.' / `\\*\\*
TODO New Child ... :work:' + its `:ID:' drawer + the multiline
body, ending with a single trailing `\\n'.  `\\* Parent Task' is
the file's last subtree.")

(defconst org-mcp-test--regex-top-level-start-after-stray-end
  (concat
   "\\`#\\+TITLE: Test Document\n"
   ":END:\n"
   "\\* TODO New Task +.*:work:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\\'")
  "Whole-file regex for a file with `#+TITLE:', a bare `:END:' line, then
an existing heading, after a `start' insert.  New heading lands between
the bare `:END:' and the existing heading.")

(defconst org-mcp-test--regex-top-level-start-after-indented-properties
  (concat
   "\\`#\\+TITLE: Test Document\n"
   " :PROPERTIES:\n"
   " :CATEGORY: work\n"
   " :END:\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\\'")
  "Whole-file regex for a top-level `start' insert after a one-space-
indented `:PROPERTIES:' drawer.  The preserved indented drawer keeps
its leading single space; the new heading's auto-generated ID drawer
is at column 0, distinguishing the two drawers in the regex.")

(defconst org-mcp-test--regex-top-level-start-after-hashtag-paragraph
  (concat
   "\\`#\\+TITLE: Test Document\n"
   "#hashtag-paragraph\n"
   "\\* TODO New Task +.*:work:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\\'")
  "Whole-file regex for a top-level `start' insert that lands after a
`#hashtag-paragraph' line.")

(defconst org-mcp-test--regex-top-level-start-drawer-only-at-top
  (concat
   "\\`:PROPERTIES:\n"
   ":CATEGORY: work\n"
   ":END:\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing Heading\\'")
  "Whole-file regex for a file starting directly with a `:PROPERTIES:'
drawer (no `#+TITLE:' header), after a `start' insert.  New heading
lands after the drawer.")

(defconst
  org-mcp-test--regex-top-level-start-drawer-only-eof-no-newline
  (concat
   "\\`:PROPERTIES:\n"
   ":CATEGORY: work\n"
   ":END:\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Whole-file regex matching a 3-line `:PROPERTIES:' drawer
followed by `\\* TODO New Top Task ... :urgent:' and its `:ID:'
drawer.  Exactly one `\\n' between `:END:' and the new heading.")

(defconst org-mcp-test--regex-top-level-start-bare-headings
  (concat
   "\\`\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\* Existing\n"
   "\\* Other\\'")
  "Whole-file regex for a file containing bare headings only (no header
block), after a `start' insert.  New heading lands at `point-min',
before `* Existing'.")

(defconst
  org-mcp-test--regex-top-level-start-orphan-deeper-before-top-level
  (concat
   "\\`#\\+TITLE: Foo\n"
   "\\*\\*\\* Orphan\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\* Real\n\\'")
  "Whole-file regex after `start' insert into a file with an
orphan level-3 heading before a level-1 heading: new heading lands
between the orphan and `* Real', leaving the orphan unparented.")

(defconst org-mcp-test--regex-top-level-start-orphan-deeper-only
  (concat
   "\\`#\\+TITLE: Foo\n"
   "\\*\\*\\* Orphan\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Whole-file regex after `start' insert into a file whose only
heading is an orphan level-3: new heading lands at `point-max',
honouring the `no top-level heading → collapse to end' rule.")

(defconst org-mcp-test--regex-top-level-start-stacks-newest-first
  (concat
   "\\`#\\+TITLE: Foo\n"
   "\\* TODO Second Top +.*:urgent:\n"
   " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
   "\\* TODO First Top +.*:urgent:\n"
   " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
   "\\* Existing\\'")
  "Whole-file regex template: two stacked TODOs (`* TODO Second
Top' above `* TODO First Top') above `* Existing'.  The two `%s'
slots take the heading `:ID:' values in file order.")

(defconst org-mcp-test--regex-child-under-parent
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
    "\\*\\* TODO Child Task +.*:work:.*\n"
    org-mcp-test--regex-id-drawer "\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for child TODO (level 2) added under parent (level 1) with existing child (level 2).")

(defconst org-mcp-test--regex-child-under-parent-pinned
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
   " *:PROPERTIES:\n *:ID: +"
   org-mcp-test--content-with-id-id
   "\n *:END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\n"
   "\\*\\* TODO Child Task +.*:work:.*\n"
   " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
   "\\'")
  "Pinned-UUID variant of `org-mcp-test--regex-child-under-parent'.
The `%s' slot is the new heading's `:ID:' value; format with
`(regexp-quote uuid)' at the call site.")

(defconst org-mcp-test--regex-child-at-start-no-children
  (concat
   "\\`\\* Parent Task\n"
   "Some parent body text\\.\n"
   "More body text\\.\n"
   "\\*\\* TODO Child Task +.*:work:.*\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for child TODO added to parent with body but no children.")

(defconst org-mcp-test--regex-child-after-drawer-only
  (concat
   "\\`\\* Parent Task\n"
   ":PROPERTIES:\n"
   ":CATEGORY: research\n"
   ":END:\n"
   "\\*\\* TODO Child Task +.*:work:.*\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for child TODO added to a parent that has only a property
drawer (no body, no children).")

(defconst org-mcp-test--regex-child-at-start-after-all-metadata
  (concat
   "\\`\\* Parent Task\n"
   "SCHEDULED: <2026-05-15 Fri>\n"
   ":PROPERTIES:\n"
   ":CATEGORY: research\n"
   ":END:\n"
   ":LOGBOOK:\n"
   "CLOCK: \\[2026-05-14 Wed 09:00\\]--\\[2026-05-14 Wed 10:30\\] =>  1:30\n"
   ":END:\n"
   "Some parent body text\\.\n"
   "\\*\\* TODO Child Task +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\*\\* Existing Child\\'")
  "Pattern for child TODO added with `position=\"start\"' under a parent
that combines planning, property drawer, logbook drawer, plain body,
and an existing child.  The new heading sits flush against the existing
child, after all metadata.")

(defconst org-mcp-test--regex-child-end-no-blank-before-other-top
  (concat
   "\\`\\* Parent Task\n"
   "Parent body\\.\n"
   "\\*\\* Existing Child\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\* Other Top\\'")
  "Pattern for a child appended at end of parent, between `** Existing
Child' and `* Other Top'.  No blank line before `* Other Top'.")

(defconst org-mcp-test--regex-start-collapse-no-blank-before-other-top
  (concat
   "\\`\\* Parent Task\n"
   "Parent body\\.\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\* Other Top\\'")
  "Whole-file regex for `start' insert into a childless parent that is
followed by `* Other Top'.  No blank line between the new child and
`* Other Top'.")

(defconst org-mcp-test--regex-after-uri-no-blank-before-other-top
  (format
   (concat
    "\\`\\* Parent Task\n"
    "\\*\\* Only Child\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "Child content\\.\n"
    "\\*\\* TODO New After Only +.*:work:.*\n"
    org-mcp-test--regex-id-drawer
    "\\* Other Top\\'")
   org-mcp-test--content-parent-single-child-then-other-top-id)
  "Whole-file regex for after-uri insertion after the parent's last
child, when the parent is followed by another top-level heading.
No blank line before that following heading.")

(defconst org-mcp-test--regex-child-at-start-of-parent
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Some parent content\\.\n"
    "\\*\\* TODO Child Task +.*:work:.*\n"
    org-mcp-test--regex-id-drawer
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for child TODO added as first child of parent that already has children.")

(defconst org-mcp-test--regex-child-at-start-of-parent-pinned
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "\n"
   "\\* Parent Task\n"
   " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
   "Some parent content\\.\n"
   "\\*\\* TODO Child Task +.*:work:.*\n"
   " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
   "\\*\\* First Child 50%% Complete\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "\\*\\* Second Child\n"
   " *:PROPERTIES:\n *:ID: +"
   org-mcp-test--content-with-id-id
   "\n *:END:\n"
   "Second child content\\.\n"
   "\\*\\* Third Child #3\\'")
  "Pinned-UUID variant of `org-mcp-test--regex-child-at-start-of-parent'.
The `%s' slot is the new heading's `:ID:' value; format with
`(regexp-quote uuid)' at the call site.")

(defconst org-mcp-test--regex-child-at-start-with-body
  (format
   (concat
    "\\`#\\+TITLE: My Org Document\n"
    "\n"
    "\\* Parent Task\n"
    " *:PROPERTIES:\n *:ID: +nested-siblings-parent-id-002\n *:END:\n"
    "Some parent content\\.\n"
    "\\*\\* TODO Child Task +.*:work:.*\n"
    org-mcp-test--regex-id-drawer
    "This is the body text\\.\n"
    "It has multiple lines\\.\n"
    "With some content\\.\n"
    "\n"
    "\\*\\* First Child 50%% Complete\n"
    "First child content\\.\n"
    "It spans multiple lines\\.\n"
    "\\*\\* Second Child\n"
    " *:PROPERTIES:\n *:ID: +%s\n *:END:\n"
    "Second child content\\.\n"
    "\\*\\* Third Child #3\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for child TODO with body added as first child before existing children.")

(defconst org-mcp-test--regex-child-after-leading-source-block
  (concat
   "\\`#\\+BEGIN_SRC text\n"
   ":LOGBOOK:\n"
   "#\\+END_SRC\n"
   "\\* Real Heading\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern after adding a child under `Real Heading' in a file with
a leading `#+BEGIN_SRC' block.  The source block (including its
`:LOGBOOK:'-looking body line) is preserved verbatim; the new
child lands as a level-2 heading under `Real Heading' with an
auto-generated `:ID:' drawer on the new heading.")

(defconst org-mcp-test--regex-child-after-leading-dynamic-block
  (concat
   "\\`#\\+BEGIN: clocktable :scope file\n"
   ":LOGBOOK:\n"
   "#\\+END:\n"
   "\\* Real Heading\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern after adding a child under `Real Heading' in a file with
a leading `#+BEGIN: clocktable' dynamic block.  The dynamic block
(including its `:LOGBOOK:'-looking body line) is preserved
verbatim; the new child lands as a level-2 heading under `Real
Heading' with an auto-generated `:ID:' drawer on the new
heading.")

(defconst org-mcp-test--regex-child-after-leading-dotted-block
  (concat
   "\\`#\\+BEGIN_my\\.block\n"
   ":LOGBOOK:\n"
   "#\\+END_my\\.block\n"
   "\\* Real Heading\n"
   "\\*\\* TODO New Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer
   "\\'")
  "Pattern after adding a child under `Real Heading' in a file with
a leading `#+BEGIN_my.block' opener (block name containing `.').
The block is preserved verbatim; the new child lands as a level-2
heading under `Real Heading' with an auto-generated `:ID:' drawer.")

(defconst org-mcp-test--regex-second-child-same-level
  (concat
   "\\`\\* Top Level\n"
   "\\*\\* Review the package\n"
   "\\*\\*\\* Review org-mcp\\.el\n"
   org-mcp-test--regex-id-drawer
   "Main package file\n"
   "\\*\\*\\* TODO Second Child +.*:work:.*\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for second child (level 3) added at same level as first child (level 3) under parent (level 2).")

(defconst org-mcp-test--regex-top-level-end-with-body
  (concat
   "\\`\\* Existing Heading\n"
   "\\* TODO New Top Task +:[^\n]*\n"
   org-mcp-test--regex-id-drawer
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n\\'")
  "Pattern for a top-level TODO appended after an existing heading with
a body.  Whole-file anchored.")

(defconst org-mcp-test--regex-todo-with-body
  (concat
   "\\`\\* TODO Task with Body +:[^\n]*\n"
   org-mcp-test--regex-id-drawer
   (regexp-quote org-mcp-test--body-text-multiline)
   "\n\\'")
  "Pattern for TODO with body text.")

(defconst org-mcp-test--regex-todo-with-body-containing-deeper-heading
  (concat
   "\\`\\* TODO Task with Sub Heading +:[^\n]*\n"
   org-mcp-test--regex-id-drawer
   "Some text\\.\n"
   "\\*\\* Sub heading in body\n"
   "More text\\.\n?\\'")
  "Pattern for a top-level TODO whose body contains a level-2
sub-heading; the `:ID:' drawer immediately follows the new
heading, not the body's sub-heading.")

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
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for TODO item without any tags.")

(defconst org-mcp-test--regex-top-level-todo
  (concat
   "\\`\\* TODO New Task +:.*work.*urgent.*:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for top-level TODO item with work and urgent tags.")

(defconst org-mcp-test--regex-todo-tag-accept-valid
  (concat
   "\\`\\* TODO ValidTask +:work:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for TODO with a single `work' tag drawn from `org-tag-alist'.")

(defconst org-mcp-test--regex-todo-tag-validation-without-alist
  (concat
   "\\`\\* TODO Task1 +:"
   ".*validtag.*tag123.*my_tag.*@home.*:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for TODO with multiple valid tags accepted without `org-tag-alist'.")

(defconst org-mcp-test--regex-top-level-todo-after-title-only
  (concat
   "\\`#\\+TITLE: Test Document\n"
   "\\* TODO New Task +:.*work.*urgent.*:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Whole-file regex matching `#+TITLE: Test Document' followed
immediately by `\\* TODO New Task ...' and its `:ID:' drawer.
No blank line between the title and the new heading.")

(defconst org-mcp-test--regex-top-level-paragraph-no-heading
  (concat
   "\\`#\\+TITLE: Test Document\n"
   "Some unstructured paragraph\\.\n"
   "\\* TODO New Task +:.*work.*urgent.*:\n"
   org-mcp-test--regex-id-drawer "\\'")
  "Pattern for a new TODO appended after a plain paragraph in a
heading-less file.  The paragraph stays in the zeroth section above
the new heading; it must not become the heading's section body.")

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
   org-mcp-test--regex-id-drawer "\\'")
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
  "Whole-file regex: heading with only `:ID:' drawer metadata; new
body content on its own line after `:END:'; file ends with a
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
  "Whole-file regex: `* Parent Task' (with `:ID:' drawer) plus
`** Second Child' (also with `:ID:') whose body contains a
deeper `*** Subheading content' carrying no `:ID:' drawer.")

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

(defconst org-mcp-test--content-archive-simple
  "* TODO Task to Archive
Some content here."
  "Org task: a single TODO headline with one body line.")

(defconst org-mcp-test--content-archive-custom-location
  "* TODO Task to Archive
:PROPERTIES:
:ARCHIVE:  %s_custom::
:END:
Some content here."
  "Task with an `:ARCHIVE:' property selecting a non-default location.
The literal `%s' is org-archive's placeholder for the source file name
without its directory (`file-name-nondirectory', extension kept), so
the archive file becomes `<source>_custom'.")

(defconst org-mcp-test--content-archive-keyword-location
  "#+ARCHIVE: %s_fromkeyword::
* TODO Task to Archive
Some content here."
  "Task under a file-level `#+ARCHIVE:' keyword with no per-headline
`:ARCHIVE:' property.  The literal `%s' is org-archive's placeholder
for the source file's base name, so the archive file becomes
`<source>_fromkeyword'.")

(defconst org-mcp-test--content-archive-infile-location
  "* TODO Task to Archive
:PROPERTIES:
:ARCHIVE:  ::* Archived Tasks
:END:
Some content here."
  "Task whose `:ARCHIVE:' property selects an in-file location (the
file part before `::' is empty), so archiving moves the subtree under
a heading within the source file itself rather than to a separate
file.")

(defconst org-mcp-test--content-archive-nested
  "* Parent
Parent body.
** TODO Child to Archive
Child content here."
  "Two-level Org tree: a `Parent' headline with body and a nested
`Child to Archive' TODO subheading carrying its own body.")

(defconst org-mcp-test--expected-archive-nested-source-regex
  "\\`\\* Parent\nParent body\\.\\s-*\\'"
  "Regex matching an Org file with a single top-level `Parent' headline
followed by its body line and no sub-headings.")

(defconst org-mcp-test--content-archive-malformed-location
  "* TODO Task to Archive
:PROPERTIES:
:ARCHIVE:  no-separator-here
:END:
Some content here."
  "Task whose `:ARCHIVE:' property omits the `::' separator, so
org-archive's location parser rejects the spec as malformed.")

(defconst org-mcp-test--expected-archive-source-regex
  "\\`\\s-*\\'"
  "Regex matching an Org file whose content is empty or whitespace only.")

(defconst org-mcp-test--expected-archive-simple-unchanged-source-regex
  (concat "\\`"
          (regexp-quote org-mcp-test--content-archive-simple)
          "\\'")
  "Regex matching the exact content of
`org-mcp-test--content-archive-simple', byte for byte.")

(defconst org-mcp-test--expected-archive-infile-source-regex
  (concat
   "\\`\\* Archived Tasks\n"
   "\\(?:\n\\)*"                        ; blank lines
   "\\*\\* TODO Task to Archive\n"
   ":PROPERTIES:\n"
   "\\(?::[A-Z_]+:[ \t]+[^\n]*\n\\)+"
   ":END:\n"
   "Some content here\\.\n?"
   "\\'")
  "Regex matching an Org file with a top-level `Archived Tasks' heading,
under it a level-2 `TODO Task to Archive' entry with a PROPERTIES
drawer, and a single body line.")

(defun org-mcp-test--archive-file-regex (&optional id heading body)
  "Regex matching an Org archive file with one archived TODO entry.
The file has a mode-line comment, an \"Archived entries from file\"
section header, and a single `* TODO HEADING' entry with a PROPERTIES
drawer followed by BODY.  HEADING defaults to \"Task to Archive\" and
BODY to \"Some content here.\", the shape produced by the
`org-mcp-test--content-archive-simple' fixture.  When ID is non-nil,
the drawer must contain an `:ID:' line with that exact value, so a
single whole-file match also pins the archived entry's identity."
  (let ((heading (or heading "Task to Archive"))
        (body (or body "Some content here.")))
    (concat
     "\\`#[^\n]*\n"                       ; mode line
     "\\(?:\n\\)*"                        ; blank lines
     "Archived entries from file[^\n]*\n" ; section header
     "\\(?:\n\\)*"                        ; blank lines
     "\\* TODO " (regexp-quote heading) "\n"
     ":PROPERTIES:\n"
     (if id
         (concat
          "\\(?::[A-Z_]+:[ \t]+[^\n]*\n\\)*" ; properties before :ID:
          ":ID:[ \t]+" (regexp-quote id) "\n"
          "\\(?::[A-Z_]+:[ \t]+[^\n]*\n\\)*") ; properties after :ID:
       "\\(?::[A-Z_]+:[ \t]+[^\n]*\n\\)+")
     ":END:\n"
     (regexp-quote body) "\n?"
     "\\'")))

(defconst org-mcp-test--expected-archive-simple-file-regex
  (org-mcp-test--archive-file-regex)
  "Regex matching an Org archive file with a mode-line comment, an
\"Archived entries from file\" section header, and a single
`* TODO Task to Archive' entry with a PROPERTIES drawer and a
\"Some content here.\" body.")

(defconst org-mcp-test--expected-archive-with-id-file-regex
  (org-mcp-test--archive-file-regex
   org-mcp-test--content-with-id-id
   "Task with ID"
   "First line of content.\nSecond line of content.\nThird line of content.")
  "Regex matching an Org archive file with a mode-line comment, an
\"Archived entries from file\" section header, and a single `Task with
ID' TODO entry whose original `:ID:' is preserved among the ARCHIVE_*
properties in its PROPERTIES drawer, followed by three body lines.")

(defconst org-mcp-test--content-preexisting-archive-entry
  "* TODO Pre-existing archived entry\n"
  "A single TODO headline line with no body or properties.")

(defconst org-mcp-test--archive-unsaved-error-regex
  (concat "Cannot archive: an Emacs buffer visiting this "
          "file has unsaved")
  "Partial error string for the unsaved-changes guard, up to but not
including the recovery-hint suffix.")

;; Test helpers

(defun org-mcp-test--read-file (file)
  "Read and return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun org-mcp-test--verify-file-matches (test-file expected-pattern)
  "Verify TEST-FILE content matches EXPECTED-PATTERN regexp."
  (should (string-match-p expected-pattern (org-mcp-test--read-file test-file))))

(defun org-mcp-test--verify-buffer-matches (buffer expected-pattern)
  "Verify BUFFER content matches EXPECTED-PATTERN regexp."
  (should
   (string-match-p
    expected-pattern
    (with-current-buffer buffer (buffer-string)))))

(defun org-mcp-test--discard-buffer-visiting (file)
  "Kill every buffer visiting FILE, discarding unsaved changes."
  (dolist (buf (buffer-list))
    (when-let* ((buf-file (buffer-file-name buf))
                ((string= (expand-file-name buf-file)
                          (expand-file-name file))))
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))))

(defmacro org-mcp-test--with-archive-file (archive-file path &rest body)
  "Bind ARCHIVE-FILE to PATH and evaluate BODY in an `unwind-protect'.
On exit, kill any buffer visiting ARCHIVE-FILE (discarding unsaved
changes) and delete the file if it exists."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,archive-file ,path))
     (unwind-protect
         (progn ,@body)
       (org-mcp-test--discard-buffer-visiting ,archive-file)
       (when (file-exists-p ,archive-file)
         (delete-file ,archive-file)))))

(defun org-mcp-test--archive-tool-response (uri)
  "Call the org-archive-subtree tool with URI; return the parsed response."
  (mcp-server-lib-process-jsonrpc-parsed
   (mcp-server-lib-create-tools-call-request
    "org-archive-subtree" 1 `((uri . ,uri)))
   mcp-server-lib-ert-server-id))

(defun org-mcp-test--archive-subtree (uri archive-file)
  "Archive subtree at URI; assert success and landing in ARCHIVE-FILE.
Return the parsed tool result alist."
  (let ((result (json-read-from-string
                 (mcp-server-lib-ert-call-tool
                  "org-archive-subtree" `((uri . ,uri))))))
    (should (equal (alist-get 'success result) t))
    (should (string= archive-file (alist-get 'archive_file result)))
    result))

(defun org-mcp-test--archive-subtree-id (uri archive-file)
  "Archive subtree at URI to ARCHIVE-FILE; return its Org ID.
Wrap `org-mcp-test--archive-subtree', assert the result URI is an
`org-id://' URI, and return its ID -- freshly created when the
headline lacked one, or the existing ID otherwise."
  (let ((returned-uri
         (alist-get 'uri
                    (org-mcp-test--archive-subtree uri archive-file))))
    (should (string-match "\\`org-id://\\(.+\\)\\'" returned-uri))
    (match-string 1 returned-uri)))

(defmacro org-mcp-test--assert-error-and-file
    (test-file error-form &optional error-message-regex)
  "Assert that ERROR-FORM throws an error and TEST-FILE remains unchanged.
ERROR-MESSAGE-REGEX, if non-nil, must match the signalled error's
message string -- used to pin the presence of specific guidance in
the wire-protocol error text."
  (declare (indent 1) (debug t))
  `(let* ((original-content (org-mcp-test--read-file ,test-file))
          (err
           (should-error ,error-form
                         :type 'mcp-server-lib-tool-error)))
     (should
      (string= (org-mcp-test--read-file ,test-file) original-content))
     (when ,error-message-regex
       (should
        (string-match-p ,error-message-regex
                        (error-message-string err))))))

(defmacro org-mcp-test--assert-archive-error
    (test-file uri &optional error-message-regex)
  "Assert archiving URI errors and TEST-FILE stays unchanged.
ERROR-MESSAGE-REGEX, if non-nil, must match the signalled error's
message string."
  (declare (indent 1) (debug (form form &optional form)))
  `(org-mcp-test--assert-error-and-file ,test-file
     (let* ((response (org-mcp-test--archive-tool-response ,uri))
            (result
             (mcp-server-lib-ert-process-tool-response response)))
       (error "Expected error but got success: %s" result))
     ,error-message-regex))

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (mcp-server-lib-ert-with-server
           :tools t
           :resources t
           :name org-mcp--server-id
           :version org-mcp--version
           :instructions org-mcp--instructions
           ,@body)
       (org-mcp-disable))))

(defmacro org-mcp-test--with-temp-org-files (file-specs &rest body)
  "Create temporary Org files, execute BODY, and ensure cleanup.
FILE-SPECS is a list of file specifications.
Each spec is (VAR CONTENT [FILENAME-PREFIX]).
VAR is the variable to bind the temp file path to.
CONTENT is the initial content to write to the file.
FILENAME-PREFIX is optional, defaults to \"org-mcp-test\".
All created files are automatically added to `org-mcp-allowed-files'.
BODY is executed with org-mcp enabled.
Returns the value of the last form in BODY."
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
  (declare (indent 2) (debug (form form body)))
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
  (declare (indent 2) (debug (symbolp form form body)))
  `(org-mcp-test--with-temp-org-files
    ((,file-var ,initial-content))
    (org-mcp-test--with-id-tracking
     (list ,file-var)
     (mapcar (lambda (id) (cons id ,file-var)) ,ids)
     ,@body)))

(defmacro org-mcp-test--with-archive-setup
    (file-var initial-content ids &rest body)
  "Like `org-mcp-test--with-id-setup' with org-archive options pinned.
Binds `org-archive-location', `org-archive-default-command',
`org-archive-subtree-save-file-p', `org-archive-file-header-format',
and `org-archive-mark-done' to their stock defaults so archive
assertions exercise org-archive's documented default behavior rather
than the developer's or CI's global configuration.  Also binds
`org-adapt-indentation' to nil so the archived property drawer and body
land at column 0 regardless of Org version: its default flipped from t
to nil in Org 9.5, and the t default (Org bundled with Emacs 27.2)
indents drawer and body, which the archive assertions do not expect.
A test needing a non-default value rebinds it inside BODY.
Finally binds `default-archive-file' to the source file's default
archive path (its name with `_archive' appended) via
`org-mcp-test--with-archive-file', discarding any buffer visiting that
file and deleting it on exit.  A test archiving to a non-default
location wraps BODY in its own `org-mcp-test--with-archive-file'."
  (declare (indent 3) (debug (symbolp form form body)))
  `(let ((org-archive-location "%s_archive::")
         (org-archive-default-command 'org-archive-subtree)
         (org-archive-subtree-save-file-p 'from-org)
         (org-archive-file-header-format
          "\nArchived entries from file %s\n\n")
         (org-archive-mark-done nil)
         (org-adapt-indentation nil))
     (org-mcp-test--with-id-setup ,file-var ,initial-content ,ids
       (org-mcp-test--with-archive-file default-archive-file
           (concat ,file-var "_archive")
         ,@body))))

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
    (title todo-state tags body parent-uri &optional after-uri position)
  "Build params alist for the `org-add-todo' MCP tool.
TITLE, TODO-STATE, TAGS, BODY, and PARENT-URI are always
emitted; passing nil for TAGS or BODY exercises the
explicit-`null' wire form for those keys.  `&optional' fields
AFTER-URI and POSITION are omitted when nil, exercising the
absent-key wire form (the common real-client shape).  Tests
needing the opposite wire form build the params alist directly;
see e.g. `add-todo-position-nil-wire-accepted'."
  (append
   (list (cons 'title title)
         (cons 'todo_state todo-state)
         (cons 'tags tags)
         (cons 'body body)
         (cons 'parent_uri parent-uri))
   (and after-uri (list (cons 'after_uri after-uri)))
   (and position (list (cons 'position position)))))

(cl-defmacro org-mcp-test--call-add-todo-expecting-error
    (initial-content title todo-state tags body parent-uri
                     &key todo-keywords tag-alist after-uri
                          position ids error-message-regex)
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
:AFTER-URI is the URI of a sibling to insert after.
:POSITION is optional placement (\"start\" or \"end\").
:IDS is optional list of ID strings to register (nil for no ID tracking).
:ERROR-MESSAGE-REGEX, if non-nil, must match the signalled error's
message string."
  `(org-mcp-test--with-add-todo-setup
    test-file ,initial-content ,todo-keywords
    ,tag-alist ,ids
    (org-mcp-test--assert-error-and-file
     test-file
     (let* ((params
             (org-mcp-test--build-add-todo-params
              ,title ,todo-state ,tags ,body ,parent-uri
              ,after-uri ,position))
            (request
              (mcp-server-lib-create-tools-call-request
               "org-add-todo" nil params))
            (response (mcp-server-lib-process-jsonrpc-parsed request
                                                             mcp-server-lib-ert-server-id))
            (result (mcp-server-lib-ert-process-tool-response response)))
       ;; If we get here, the tool succeeded when we expected failure
       (error "Expected error but got success: %s" result))
     ,error-message-regex)))

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

(defun org-mcp-test--field-non-string-regex (field-name bad-value)
  "Return the exact error regex for non-string FIELD-NAME with BAD-VALUE.
The regex literal-matches the full validator diagnostic -- field
name, printed value, and `type-of' name."
  (regexp-quote
   (format "Field %s must be a string, got: %S (type: %s)"
           field-name bad-value (type-of bad-value))))

(defmacro org-mcp-test--assert-tool-error-message-regex
    (tool-name params error-message-regex)
  "Assert calling TOOL-NAME with PARAMS errors with message matching REGEX.
TOOL-NAME is the registered tool name (a string).  PARAMS is the
JSON-RPC parameter alist.  ERROR-MESSAGE-REGEX must match the
signalled error's message string.

For read tools there is no file state to verify unchanged, so this
wraps just the JSON-RPC pipeline plus an error-type and
error-message assertion.  Modifying tools should use their own
`call-X-expecting-error' helpers, which additionally route through
`org-mcp-test--assert-error-and-file' to pin file-unchanged behaviour."
  `(let ((org-mcp-allowed-files nil))
     (org-mcp-test--with-enabled
       (let* ((request
                (mcp-server-lib-create-tools-call-request
                 ,tool-name 1 ,params))
              (response
                (mcp-server-lib-process-jsonrpc-parsed
                 request mcp-server-lib-ert-server-id))
              (err
               (should-error
                (mcp-server-lib-ert-process-tool-response response)
                :type 'mcp-server-lib-tool-error)))
         (should
          (string-match-p ,error-message-regex
                          (error-message-string err)))))))

(defun org-mcp-test--assert-add-todo-non-string-title (bad-value)
  "Assert BAD-VALUE for `title' is rejected at the tool boundary."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   bad-value "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :error-message-regex
   (org-mcp-test--field-non-string-regex "title" bad-value)))

(defun org-mcp-test--assert-add-todo-non-string-body (bad-value)
  "Assert BAD-VALUE for `body' is rejected at the tool boundary."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") bad-value
   (format "org-headline://%s#" test-file)
   :error-message-regex
   (org-mcp-test--field-non-string-regex "body" bad-value)))

(defun org-mcp-test--assert-add-todo-non-string-todo-state (bad-value)
  "Assert BAD-VALUE for `todo_state' is rejected at the tool boundary."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" bad-value '("work") nil
   (format "org-headline://%s#" test-file)
   :error-message-regex
   (org-mcp-test--field-non-string-regex "todo_state" bad-value)))

(defun org-mcp-test--assert-add-todo-non-string-parent-uri (bad-value)
  "Assert BAD-VALUE for `parent_uri' is rejected at the tool boundary."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   bad-value
   :error-message-regex
   (org-mcp-test--field-non-string-regex "parent_uri" bad-value)))

(defun org-mcp-test--assert-add-todo-non-string-after-uri (bad-value)
  "Assert BAD-VALUE for `after_uri' is rejected at the tool boundary."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :after-uri bad-value
   :error-message-regex
   (org-mcp-test--field-non-string-regex "after_uri" bad-value)))

(defun org-mcp-test--assert-add-todo-non-string-position (bad-value)
  "Assert BAD-VALUE for `position' is rejected at the tool boundary."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position bad-value
   :error-message-regex
   (org-mcp-test--field-non-string-regex "position" bad-value)))

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

(defmacro org-mcp-test--assert-add-todo-after-uri-rejected
    (initial-content parent-fragment after-uri-value ids error-regex)
  "Assert `org-add-todo' under PARENT-FRAGMENT rejects AFTER-URI-VALUE.
INITIAL-CONTENT is the fixture, PARENT-FRAGMENT the URI fragment
identifying the parent, AFTER-URI-VALUE the value passed for the
`after_uri' field.  IDS is a list of IDs to register against
TEST-FILE, or nil for none.  ERROR-REGEX must match the signalled
error message, pinning which validator fires.

A macro rather than a function so that AFTER-URI-VALUE (and IDS,
when supplied) can reference the surrounding `test-file' binding
introduced by `with-add-todo-setup'."
  `(org-mcp-test--call-add-todo-expecting-error
    ,initial-content
    "New Task" "TODO" '("work") nil
    (format "org-headline://%s#%s" test-file ,parent-fragment)
    :after-uri ,after-uri-value
    :ids ,ids
    :error-message-regex ,error-regex))

(defun org-mcp-test--assert-add-todo-result-shape (result title basename)
  "Assert RESULT has the standard 4-field `org-add-todo' response shape.
TITLE is the expected `title' field; BASENAME the expected `file'.
Call this from any test that invokes `org-add-todo' directly (e.g.
composition tests that cannot use `org-mcp-test--add-todo-and-check')
so result-shape coverage is not silently weakened."
  (should (= (length result) 4))
  (should (equal (alist-get 'success result) t))
  (should (string-match-p "\\`org-id://.+" (alist-get 'uri result)))
  (should (equal (alist-get 'file result) basename))
  (should (equal (alist-get 'title result) title)))

(defun org-mcp-test--add-todo-start-and-return-uuid
    (title parent-uri basename)
  "Call `org-add-todo' for TITLE at start of PARENT-URI; return new UUID.
Uses the top-level start-family default params (TAGS '(\"urgent\"), no
body, no `after_uri'), asserts the standard 4-field result shape against
BASENAME, and returns the UUID stripped from the result's `org-id://' URI.
Intended for composition tests that need the freshly inserted heading's
identifier to compose into a follow-up whole-file regex."
  (let* ((params
          (org-mcp-test--build-add-todo-params
           title "TODO" '("urgent") nil parent-uri nil "start"))
         (result-text
          (mcp-server-lib-ert-call-tool "org-add-todo" params))
         (result (json-read-from-string result-text)))
    (org-mcp-test--assert-add-todo-result-shape result title basename)
    (substring (alist-get 'uri result) (length "org-id://"))))

(cl-defmacro org-mcp-test--add-todo-and-check
    (initial-content
     title todo-state tags body parent-uri
     basename expected-pattern
     &key todo-keywords tag-alist ids after-uri position override-bindings
     pin-new-heading-uuid)
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
:POSITION is placement (\"start\" or \"end\").
:OVERRIDE-BINDINGS is a list of let-style bindings to override
variables after setup, e.g., ((org-tag-alist nil)).
:PIN-NEW-HEADING-UUID, when non-nil, treats EXPECTED-PATTERN as a
`format' template carrying one `%s' slot for the new heading's
`:ID:' value.  The macro captures the UUID from the tool's
returned `org-id://' URI and weaves it into that slot before
matching, so the test pins that the URI identifies the new
heading and not some other one."
  (let ((checking-logic
         `(let* ((params
                  (org-mcp-test--build-add-todo-params
                   ,title ,todo-state ,tags ,body ,parent-uri
                   ,after-uri ,position))
                 (result-text (mcp-server-lib-ert-call-tool "org-add-todo" params))
                 (result (json-read-from-string result-text)))
            (org-mcp-test--assert-add-todo-result-shape
             result ,title ,basename)
            ,(if pin-new-heading-uuid
                 `(let ((new-uuid
                         (substring (alist-get 'uri result)
                                    (length "org-id://"))))
                    (org-mcp-test--verify-file-matches
                     test-file
                     (format ,expected-pattern (regexp-quote new-uuid))))
               `(org-mcp-test--verify-file-matches
                 test-file ,expected-pattern)))))
    `(org-mcp-test--with-add-todo-setup
      test-file
      ,initial-content ,todo-keywords ,tag-alist ,ids
      ,(if override-bindings
           `(let ,override-bindings
              ,checking-logic)
         checking-logic))))

(defmacro org-mcp-test--add-todo-top-level-start-and-check
    (fixture expected-regex &optional title tags)
  "Run a top-level `position=\"start\"' `org-add-todo' check.
FIXTURE is the initial Org content; EXPECTED-REGEX is the
whole-buffer regex to verify after insertion.  TITLE defaults to
\"New Top Task\"; TAGS defaults to '(\"urgent\").  All other
parameters of `org-mcp-test--add-todo-and-check' are fixed to the
top-level `position=\"start\"' family's standard shape."
  `(org-mcp-test--add-todo-and-check
    ,fixture
    (or ,title "New Top Task")
    "TODO"
    (or ,tags '("urgent"))
    nil ; no body
    (format "org-headline://%s#" test-file)
    (file-name-nondirectory test-file)
    ,expected-regex
    :position "start"))

;; Helper functions for testing org-update-todo-state MCP tool

(defun org-mcp-test--call-update-todo-state-expecting-error
    (test-file resource-uri current-state new-state
               &optional error-message-regex)
  "Call org-update-todo-state tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI to update.
CURRENT-STATE is the current TODO state.
NEW-STATE is the new TODO state to set.
ERROR-MESSAGE-REGEX, if non-nil, must match the signalled error's
message string."
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
       (error "Expected error but got success: %s" result))
     error-message-regex)))

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
    (initial-content headline-path-or-uri current-title new-title
                     &optional error-message-regex)
  "Call org-rename-headline tool expecting an error and verify file unchanged.
INITIAL-CONTENT is the initial Org file content.
HEADLINE-PATH-OR-URI is either a headline path fragment, full URI,
or any non-string value (passed through verbatim for type-error tests).
CURRENT-TITLE is the current title for validation.
NEW-TITLE is the new title to set.
ERROR-MESSAGE-REGEX, if non-nil, must match the signalled error's
message string."
  (org-mcp-test--with-temp-org-files
   ((test-file initial-content))
   (let ((uri (cond
               ((not (stringp headline-path-or-uri)) headline-path-or-uri)
               ((string-prefix-p "org-" headline-path-or-uri)
                headline-path-or-uri)
               (t (format "org-headline://%s#%s"
                          test-file headline-path-or-uri)))))
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
        (error "Expected error but got success: %s" result))
      error-message-regex))))

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
    (test-file resource-uri old-body new-body
               &optional replace-all error-message-regex)
  "Call org-edit-body tool expecting an error and verify file unchanged.
TEST-FILE is the test file path to verify remains unchanged.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
REPLACE-ALL if true, replace all occurrences (default: nil).  The
`replace_all' key is always emitted, so passing nil exercises the
explicit-`null' wire form; tests needing absent-key build the
params alist directly.
ERROR-MESSAGE-REGEX, if non-nil, must match the signalled error's
message string."
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
     (error "Expected error but got success: %s" result))
   error-message-regex))

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

(defun org-mcp-test--verify-resource-text-matches (uri expected-pattern)
  "Read resource at URI, assert success and text matches EXPECTED-PATTERN."
  (let* ((request (mcp-server-lib-create-resources-read-request uri))
         (response-json
          (mcp-server-lib-process-jsonrpc
           request mcp-server-lib-ert-server-id))
         (response
          (json-parse-string response-json :object-type 'alist)))
    (should-not (alist-get 'error response))
    (let ((contents (alist-get 'contents (alist-get 'result response))))
      (should
       (string-match-p
        expected-pattern (alist-get 'text (aref contents 0)))))))

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

;;; org-get-todo-config tests

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

;;; org-get-tag-config tests

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

;;; org-get-allowed-files tests

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

;; org-get-agenda tests

(ert-deftest org-mcp-test-tool-get-agenda-day ()
  "Test org-get-agenda with day view.
Pins the day-view contract: the JSON echoes the input `date', reports
`start_day' as that same resolved day, and the agenda renders the
scheduled task.  `org-agenda-format-date' is pinned so the rendered
date header is deterministic regardless of ambient Org configuration."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((org-agenda-format-date "%Y-%m-%d")
          (result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "day") (date . "2026-04-26")))))
          (agenda (alist-get 'agenda result))
          (view (alist-get 'view result))
          (date (alist-get 'date result))
          (start-day (alist-get 'start_day result)))
     (should (string= view "day"))
     (should (string= date "2026-04-26"))
     (should (string= start-day "2026-04-26"))
     (should (string-match "2026-04-26" agenda))
     (should (string-match "Test agenda line" agenda)))))

(ert-deftest org-mcp-test-tool-get-agenda-week ()
  "Test org-get-agenda with week view.
Pins the week-view contract: with the week starting on Monday, a view
anchored on Sunday 2026-04-26 reports `start_day' as the preceding
Monday 2026-04-20.  `org-agenda-start-on-weekday' is pinned so the week
boundary is deterministic."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((org-agenda-start-on-weekday 1)
          (result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "week") (date . "2026-04-26")))))
          (agenda (alist-get 'agenda result))
          (view (alist-get 'view result))
          (date (alist-get 'date result))
          (start-day (alist-get 'start_day result)))
     (should (string= view "week"))
     (should (string= date "2026-04-26"))
     (should (string= start-day "2026-04-20"))
     (should (string-match "Test agenda line" agenda)))))

(ert-deftest org-mcp-test-tool-get-agenda-month ()
  "Test org-get-agenda month view spans the whole calendar month.
Pins calendar-month alignment with `org-agenda-month-view': a month
view anchored on 2026-04-26 reports `start_day' as the first of the
month and covers all of April -- including the April 3 task that
precedes the reference day -- while excluding the May 10 task that a
rolling 30-day window from the 26th would wrongly include."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-month-content))
   (let* ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "month") (date . "2026-04-26")))))
          (agenda (alist-get 'agenda result))
          (view (alist-get 'view result))
          (date (alist-get 'date result))
          (start-day (alist-get 'start_day result)))
     (should (string= view "month"))
     (should (string= date "2026-04-26"))
     (should (string= start-day "2026-04-01"))
     (should (string-match "Early April task" agenda))
     (should-not (string-match "Next month task" agenda)))))

(ert-deftest org-mcp-test-tool-get-agenda-view-case-insensitive ()
  "Test org-get-agenda accepts mixed-case view name."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "DaY") (date . "2026-04-26")))))
          (view (alist-get 'view result)))
     (should (string= view "day")))))

(ert-deftest org-mcp-test-tool-get-agenda-no-existing-files ()
  "Test org-get-agenda errors when no allowed files exist on disk."
  (let ((org-mcp-allowed-files '("/no/such/org-mcp-agenda-test-file.org")))
    (org-mcp-test--with-enabled
     (let* ((params '((view . "day") (date . "2026-04-26")))
            (request (mcp-server-lib-create-tools-call-request
                      "org-get-agenda" nil params))
            (response (mcp-server-lib-process-jsonrpc-parsed
                       request mcp-server-lib-ert-server-id)))
       (should-error
        (mcp-server-lib-ert-process-tool-response response)
        :type 'mcp-server-lib-tool-error)))))

(ert-deftest org-mcp-test-tool-get-agenda-bad-view ()
  "Test org-get-agenda errors on invalid view."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((params '((view . "fortnight") (date . "2026-04-26")))
          (request (mcp-server-lib-create-tools-call-request
                    "org-get-agenda" nil params))
          (response (mcp-server-lib-process-jsonrpc-parsed
                     request mcp-server-lib-ert-server-id)))
     (should-error
      (mcp-server-lib-ert-process-tool-response response)
      :type 'mcp-server-lib-tool-error))))

(ert-deftest org-mcp-test-tool-get-agenda-default-date ()
  "Test org-get-agenda reports the literal \"today\" when date is omitted.
Pins the documented default-date contract: omitting `date' returns the
literal string \"today\" in the JSON `date' field and still renders an
agenda."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "day")))))
          (agenda (alist-get 'agenda result))
          (view (alist-get 'view result))
          (date (alist-get 'date result)))
     (should (string= view "day"))
     (should (string= date "today"))
     (should (stringp agenda))
     (should (> (length agenda) 0)))))

(ert-deftest org-mcp-test-tool-get-agenda-week-defaults-to-today ()
  "Test org-get-agenda week view reports \"today\" when date is omitted.
Pins the default-date contract for the week span, completing the
day/week/month matrix: omitting `date' returns the literal string
\"today\" in the JSON `date' field and still renders an agenda."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "week")))))
          (agenda (alist-get 'agenda result))
          (view (alist-get 'view result))
          (date (alist-get 'date result)))
     (should (string= view "week"))
     (should (string= date "today"))
     (should (stringp agenda))
     (should (> (length agenda) 0)))))

(ert-deftest org-mcp-test-tool-get-agenda-ignores-org-agenda-start-day ()
  "Test org-get-agenda day view ignores a non-nil `org-agenda-start-day'.
Pins the omitted-`date' contract under scope isolation: when `date' is
omitted the day view anchors on today regardless of the user's global
`org-agenda-start-day'.  Without isolating that global, `org-agenda-list'
falls back to it and the agenda silently anchors on the configured day
instead of today.  `org-today' is stubbed so today is deterministic --
the day-view path resolves today via `org-today', which `current-time'
stubbing does not reach -- and `org-agenda-start-day' is set to a
different day to prove the global does not leak into the resolved
`start_day'."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (cl-letf (((symbol-function 'org-today)
              (lambda ()
                (calendar-absolute-from-gregorian '(6 1 2026)))))
     (let* ((org-agenda-start-day "2026-06-10")
            (result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-get-agenda" '((view . "day")))))
            (view (alist-get 'view result))
            (date (alist-get 'date result))
            (start-day (alist-get 'start_day result)))
       (should (string= view "day"))
       (should (string= date "today"))
       (should (string= start-day "2026-06-01"))))))

(ert-deftest org-mcp-test-tool-get-agenda-unparseable-date ()
  "Test org-get-agenda forwards a day/week `date' to Org verbatim.
Pins org-mcp's own contract for the day/week path: the `date' string is
passed through to `org-agenda-list' as its start day unchanged and
echoed back verbatim in the response.  `org-agenda-list' is stubbed so
the test does not depend on how the installed Org version resolves
unrecognized input -- resolution org-mcp deliberately delegates to Org."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let (forwarded-start-day)
     (cl-letf (((symbol-function 'org-agenda-list)
                (lambda (&optional _arg start-day &rest _)
                  (setq forwarded-start-day start-day)
                  (with-current-buffer
                      (get-buffer-create org-agenda-buffer-tmp-name)
                    (insert "stub agenda body\n")
                    (set (make-local-variable 'org-starting-day)
                         (calendar-absolute-from-gregorian
                          '(6 1 2026)))))))
       (let* ((result
               (json-read-from-string
                (mcp-server-lib-ert-call-tool
                 "org-get-agenda"
                 '((view . "day") (date . "notadate")))))
              (agenda (alist-get 'agenda result))
              (date (alist-get 'date result)))
         (should (equal forwarded-start-day "notadate"))
         (should (string= date "notadate"))
         (should (stringp agenda)))))))

(ert-deftest org-mcp-test-tool-get-agenda-month-unparseable-date ()
  "Test org-get-agenda month view passes `date' to `org-read-date' verbatim.
The month branch resolves `date' itself via `org-read-date' at a
different point than the day/week spans, which forward the string to
`org-agenda-list'.  Pins org-mcp's own contract for that distinct path:
the `date' string is handed to `org-read-date' unchanged and echoed
back verbatim.  `org-read-date' is stubbed so the test does not depend
on how the installed Org version resolves unrecognized input -- which
org-mcp deliberately delegates to Org -- and `start_day' is not
asserted."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let (forwarded-date)
     (cl-letf (((symbol-function 'org-read-date)
                (lambda (&optional _with-time _to-time from-string &rest _)
                  (setq forwarded-date from-string)
                  (encode-time (list 0 0 12 1 6 2026 nil -1 nil)))))
       (let* ((result
               (json-read-from-string
                (mcp-server-lib-ert-call-tool
                 "org-get-agenda"
                 '((view . "month") (date . "notadate")))))
              (agenda (alist-get 'agenda result))
              (view (alist-get 'view result))
              (date (alist-get 'date result)))
         (should (equal forwarded-date "notadate"))
         (should (string= view "month"))
         (should (string= date "notadate"))
         (should (stringp agenda)))))))

(ert-deftest org-mcp-test-tool-get-agenda-empty-date ()
  "Test org-get-agenda rejects an empty or whitespace-only date.
Pins that a blank `date' is a validation error rather than being
silently treated as omitted: clients must omit the key to get today."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (dolist (blank-date '("" " " "\u00A0"))
     (let* ((params `((view . "day") (date . ,blank-date)))
            (request (mcp-server-lib-create-tools-call-request
                      "org-get-agenda" nil params))
            (response (mcp-server-lib-process-jsonrpc-parsed
                       request mcp-server-lib-ert-server-id)))
       (should-error
        (mcp-server-lib-ert-process-tool-response response)
        :type 'mcp-server-lib-tool-error)))))

(ert-deftest org-mcp-test-tool-get-agenda-ignores-other-agenda-files ()
  "Test org-get-agenda ignores the user's `org-agenda-files'.
Pins the scope-isolation contract: the agenda is built only from
`org-mcp-allowed-files', so a task living in a file the user has in
`org-agenda-files' but not in the allowed list must not leak into the
result, while the allowed file's task still appears."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let ((foreign-file
          (make-temp-file
           "org-mcp-foreign" nil ".org"
           org-mcp-test--agenda-month-content)))
     (unwind-protect
         (let* ((org-agenda-files (list foreign-file))
                (result
                 (json-read-from-string
                  (mcp-server-lib-ert-call-tool
                   "org-get-agenda"
                   '((view . "day") (date . "2026-04-26")))))
                (agenda (alist-get 'agenda result)))
           (should (string-match "Test agenda line" agenda))
           (should-not (string-match "On reference day" agenda)))
       (delete-file foreign-file)))))

(ert-deftest org-mcp-test-tool-get-agenda-ignores-file-restriction-lock ()
  "Test org-get-agenda ignores an active agenda file restriction lock.
Pins the scope-isolation contract against
`org-agenda-set-restriction-lock': when the user has locked the agenda
to a file outside `org-mcp-allowed-files', the lock is the `org-restrict'
symbol property on `org-agenda-files', which `org-agenda-list' honors
above the dynamic variable.  The tool must still build only from the
allowed files, so the foreign task must not leak.  The lock state is
restored afterward so a concurrent interactive agenda keeps its
restriction."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let ((foreign-file
          (make-temp-file
           "org-mcp-foreign" nil ".org"
           org-mcp-test--agenda-month-content))
         (saved-restrict (get 'org-agenda-files 'org-restrict)))
     (unwind-protect
         (let* ((foreign-truename (file-truename foreign-file))
                (org-agenda-restrict t)
                (org-agenda-overriding-restriction 'file))
           (put 'org-agenda-files 'org-restrict (list foreign-truename))
           (let* ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-get-agenda"
                     '((view . "day") (date . "2026-04-26")))))
                  (agenda (alist-get 'agenda result)))
             (should (string-match "Test agenda line" agenda))
             (should-not (string-match "On reference day" agenda))
             (should (equal (get 'org-agenda-files 'org-restrict)
                            (list foreign-truename)))))
       (put 'org-agenda-files 'org-restrict saved-restrict)
       (delete-file foreign-file)))))

(ert-deftest org-mcp-test-tool-get-agenda-ignores-subtree-restriction-lock ()
  "Test org-get-agenda ignores an active agenda subtree restriction lock.
Pins that a subtree lock on an allowed file does not narrow the tool's
agenda: `org-agenda-list' narrows scanning to
`org-agenda-restrict-begin'..`org-agenda-restrict-end' when the scanned
buffer is `org-agenda-restrict', so without isolating that state the
tool would drop entries outside the locked subtree.  Both sibling tasks
must appear."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-two-task-content))
   (let ((saved-restrict (get 'org-agenda-files 'org-restrict))
         (buf (find-file-noselect agenda-file)))
     (unwind-protect
         (let ((org-agenda-restrict buf)
               (org-agenda-overriding-restriction 'subtree))
           (with-current-buffer buf
             (goto-char (point-min))
             (org-back-to-heading t)
             (move-marker org-agenda-restrict-begin (point))
             (move-marker org-agenda-restrict-end
                          (save-excursion
                            (org-end-of-subtree t t)
                            (point))))
           (put 'org-agenda-files 'org-restrict
                (list (file-truename agenda-file)))
           (let* ((result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-get-agenda"
                     '((view . "day") (date . "2026-04-26")))))
                  (agenda (alist-get 'agenda result)))
             (should (string-match "First agenda task" agenda))
             (should (string-match "Second agenda task" agenda))))
       (put 'org-agenda-files 'org-restrict saved-restrict)
       (move-marker org-agenda-restrict-begin nil)
       (move-marker org-agenda-restrict-end nil)
       (when (buffer-live-p buf)
         (kill-buffer buf))))))

(ert-deftest org-mcp-test-tool-get-agenda-preserves-global-state ()
  "Test org-get-agenda isolates the globals `org-agenda-list' mutates.
Pins the global-state isolation across a single tool call:
- a live interactive agenda's `org-agenda-markers' stay attached to
  their buffer -- the tool does not detach them via
  `org-agenda-reset-markers';
- `org-agenda-buffer-name' is not left naming the killed private
  buffer;
- `org-agenda-buffer', which `org-agenda-prepare' repoints at the
  private build buffer on the non-sticky path, is restored to the live
  interactive agenda buffer rather than left dangling at the killed
  private buffer;
- `org-agenda-pre-window-conf', which `org-agenda-prepare-window'
  overwrites on the non-sticky path before `org-agenda-mode' can make it
  buffer-local, is returned unchanged;
- `org-agenda-contributing-files', which `org-agenda-prepare' resets and
  `org-agenda-format-item' accumulates into -- a plain global that
  `org-agenda-mode' does not make buffer-local, so without isolation the
  tool overwrites the user's live value.
The sentinels for `org-agenda-pre-window-conf' and
`org-agenda-contributing-files' are set after building the interactive
agenda (whose own build overwrites them) so the assertions isolate the
tool call's effect."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let ((org-agenda-files (list agenda-file))
         (org-agenda-sticky nil)
         (org-agenda-buffer-name "*Org Agenda*")
         (org-agenda-pre-window-conf nil)
         (org-agenda-contributing-files nil)
         ;; Restore these non-buffer-local globals `org-agenda-list'
         ;; mutates, so dead state does not leak into later tests.
         (org-agenda-buffer org-agenda-buffer)
         (org-agenda-markers org-agenda-markers))
     (unwind-protect
         (progn
           (org-agenda-list nil "2026-04-26" 'day)
           (let ((markers (copy-sequence org-agenda-markers))
                 (agenda-buffer org-agenda-buffer))
             (should markers)
             (should (cl-every #'marker-buffer markers))
             (setq org-agenda-pre-window-conf 'sentinel)
             (setq org-agenda-contributing-files 'sentinel)
             (mcp-server-lib-ert-call-tool
              "org-get-agenda"
              '((view . "day") (date . "2026-04-26")))
             (should (cl-every #'marker-buffer markers))
             (should (string= org-agenda-buffer-name "*Org Agenda*"))
             (should (eq org-agenda-buffer agenda-buffer))
             (should (buffer-live-p org-agenda-buffer))
             (should (eq org-agenda-pre-window-conf 'sentinel))
             (should (eq org-agenda-contributing-files 'sentinel))))
       (when-let* ((buf (get-buffer org-agenda-buffer-name)))
         (kill-buffer buf))))))

(ert-deftest org-mcp-test-tool-get-agenda-month-resolves-reference-date ()
  "Test org-get-agenda month view snaps `start_day' to the parsed month.
Pins that the month branch resolves the `date' string and snaps to the
first of *that* calendar month and year -- not the fixture's April 2026
-- so a reference day in a different month and year reports `start_day'
as the first of the resolved month.  Exercises the `decode-time'
month/year extraction that only the month span performs."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "month") (date . "2025-02-15")))))
          (view (alist-get 'view result))
          (start-day (alist-get 'start_day result)))
     (should (string= view "month"))
     (should (string= start-day "2025-02-01")))))

(ert-deftest org-mcp-test-tool-get-agenda-month-defaults-to-current-month ()
  "Test org-get-agenda month view with no `date' snaps to the current month.
Pins the nil-`date' month branch of `org-mcp--agenda-start-day': it
decodes `current-time' and reports `start_day' as the first of the
current calendar month.  `current-time' is stubbed to a fixed
mid-month instant so the assertion is deterministic and independent of
the test host's clock and timezone."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (cl-letf (((symbol-function 'current-time)
              (lambda ()
                (encode-time (list 0 0 12 15 6 2027 nil -1 nil)))))
     (let* ((result
             (json-read-from-string
              (mcp-server-lib-ert-call-tool
               "org-get-agenda" '((view . "month")))))
            (view (alist-get 'view result))
            (start-day (alist-get 'start_day result)))
       (should (string= view "month"))
       (should (string= start-day "2027-06-01"))))))

(ert-deftest org-mcp-test-tool-get-agenda-week-unsnapped-weekday ()
  "Test org-get-agenda week view honors a nil `org-agenda-start-on-weekday'.
Pins that week alignment follows the user's
`org-agenda-start-on-weekday': when it is nil the week is not snapped
to a fixed weekday, so `start_day' equals the reference day itself
rather than the preceding Monday."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let* ((org-agenda-start-on-weekday nil)
          (result
           (json-read-from-string
            (mcp-server-lib-ert-call-tool
             "org-get-agenda" '((view . "week") (date . "2026-04-26")))))
          (view (alist-get 'view result))
          (start-day (alist-get 'start_day result)))
     (should (string= view "week"))
     (should (string= start-day "2026-04-26")))))

(ert-deftest org-mcp-test-tool-get-agenda-ignores-directory-entries ()
  "Test org-get-agenda excludes directory entries from the allow-list.
Pins the scope-isolation contract against directory expansion: a
directory in `org-mcp-allowed-files' must not reach `org-agenda-list',
which would expand it into every contained Org file and leak tasks
from outside the allow-list into the result."
  (org-mcp-test--with-temp-org-files
   ((agenda-file org-mcp-test--agenda-basic-content))
   (let ((foreign-dir (make-temp-file "org-mcp-agenda-dir" t)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "foreign.org" foreign-dir)
             (insert org-mcp-test--agenda-month-content))
           (let* ((org-mcp-allowed-files (list agenda-file foreign-dir))
                  (result
                   (json-read-from-string
                    (mcp-server-lib-ert-call-tool
                     "org-get-agenda"
                     '((view . "day") (date . "2026-04-26")))))
                  (agenda (alist-get 'agenda result)))
             (should (string-match "Test agenda line" agenda))
             (should-not (string-match "On reference day" agenda))))
       (delete-directory foreign-dir t)))))

(ert-deftest org-mcp-test-tool-get-agenda-cleans-buffer-on-error ()
  "Test `org-mcp--agenda-buffer-text' reclaims its buffer on a build error.
Pins error-path cleanup: if `org-agenda-list' signals after creating
the private agenda buffer, `org-mcp--agenda-buffer-text' must kill it
rather than leak a hidden buffer."
  (cl-letf (((symbol-function 'org-agenda-list)
             (lambda (&rest _)
               (get-buffer-create org-agenda-buffer-tmp-name)
               (error "Simulated agenda failure"))))
    (should-error
     (org-mcp--agenda-buffer-text '("/no/such/file.org") nil 'day))
    (should-not (get-buffer org-mcp--agenda-buffer-name))))

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

(ert-deftest org-mcp-test-update-todo-state-empty-on-no-state-heading ()
  "Test that `current_state=\"\"' matches a heading with no TODO state.
The documented contract -- pass `\"\"' to indicate the heading has
no TODO state -- previously failed because `string='
treated nil (what `org-get-todo-state' returns) and `\"\"' as
distinct, so the only working path for a no-state heading was the
undocumented JSON null.  Locks the fix that normalises `\"\"' to nil
before the state-match comparison."
  (let ((test-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let* ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
             (resource-uri
              (format "org-headline://%s#Parent%%20Task" test-file))
             (params
              `((uri . ,resource-uri)
                (current_state . "")
                (new_state . "TODO")))
             (result-text
              (mcp-server-lib-ert-call-tool
               "org-update-todo-state" params))
             (result (json-read-from-string result-text)))
        (should (equal (alist-get 'success result) t))
        (should (equal (alist-get 'previous_state result) ""))
        (should (equal (alist-get 'new_state result) "TODO"))))))

(ert-deftest org-mcp-test-update-todo-state-empty-on-with-state-mismatches ()
  "Test that `current_state=\"\"' against a TODO-bearing heading errors.
The complement of `empty-on-no-state-heading': the comparison must
remain strict when the heading actually has a TODO state, so a
caller asserting `\"\"' on a `TODO' heading still gets a state
mismatch."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "" "DONE")))))

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

(ert-deftest org-mcp-test-update-todo-state-non-string-current-state ()
  "Pin that non-string `current_state' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal `string='
state-match comparison would signal `wrong-type-argument'."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri 42 "DONE"
         (org-mcp-test--field-non-string-regex "current_state" 42))))))

(ert-deftest org-mcp-test-update-todo-state-null-current-state-rejected ()
  "Pin that JSON null (nil) `current_state' is rejected at the tool boundary.
Previously the validator was called with `allow-nil=t' and silently
accepted nil, then coerced it to `\"\"' in the response: an
undocumented working path that happened to match no-TODO-state
headings because `(string= nil nil)' returns t.  After dropping
`allow-nil', nil takes the same rejection path as any other
non-string."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri nil "DONE"
         (org-mcp-test--field-non-string-regex "current_state" nil))))))

(ert-deftest org-mcp-test-update-todo-state-non-string-uri ()
  "Pin that non-string `uri' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream URI
parsing runs."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (org-mcp-test--call-update-todo-state-expecting-error
       test-file 42 "TODO" "DONE"
       (org-mcp-test--field-non-string-regex "uri" 42)))))

(ert-deftest org-mcp-test-update-todo-state-non-string-new-state ()
  "Pin that non-string `new_state' is rejected at the tool boundary.
Locks the tool-boundary string-field guard ahead of the value-keyword
guard: `org-mcp--validate-string-field' fires before
`org-mcp--validate-todo-state' so all string parameters share the
same error class regardless of whether the value is also
keyword-constrained."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-files
        ((test-file test-content))
      (let ((resource-uri
             (format "org-headline://%s#Task%%20with%%20ID" test-file)))
        (org-mcp-test--call-update-todo-state-expecting-error
         test-file resource-uri "TODO" 42
         (org-mcp-test--field-non-string-regex "new_state" 42))))))

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

(ert-deftest org-mcp-test-update-todo-state-unterminated-drawer ()
  "Test that `org-update-todo-state' validates the file header.
A state change on a heading in a file whose own header block contains
a malformed `:PROPERTIES:' drawer is rejected before any modification."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-with-todo-and-body))
    (org-mcp-test--call-update-todo-state-expecting-error
     test-file
     (format "org-headline://%s#Existing%%20Heading" test-file)
     "TODO" "DONE")))

;;; org-add-todo tests

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

(ert-deftest org-mcp-test-add-todo-top-level-trailing-slash-parent-uri ()
  "Test top-level `parent_uri' with trailing `/' (no fragment).
The README documents `org-headline://filename.org/' as the
canonical top-level form; verify it parses as a no-fragment
top-level reference equivalently to the bare and empty-`#' forms."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s/" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo))

(ert-deftest org-mcp-test-add-todo-top-level-trailing-slash-before-empty-fragment ()
  "Test that `org-headline://FILE/#' resolves as top-level.
Locks the parser behaviour at the tool boundary: the trailing
`/' on the file path must strip and the empty fragment must
collapse so that `parent-path' is nil and `tool-add-todo' takes
the top-level insertion branch.  Without either half of the
collapse, `(or parent-path parent-id)' in `tool-add-todo' would
be truthy on a (\"\") parent-path and `navigate-to-parent' would
be called with an empty title.  Companion to
`add-todo-top-level-trailing-slash-parent-uri' (slash, no `#')
and `add-todo-child-parent-uri-trailing-slash-before-fragment'
\(slash + populated fragment)."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s/#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-empty-file ()
  "Test position=\"start\" on an empty file behaves like default."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-position-end-empty-file ()
  "Test position=\"end\" on an empty file behaves like default."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo
   :position "end"))

(ert-deftest org-mcp-test-add-todo-top-level-position-end-paragraph-no-heading ()
  "Test position=\"end\" on a heading-less file with a plain paragraph.
The walker stops at the paragraph (it is neither blank, nor a
`#'-prefixed line, nor a drawer opener), so `header-end' lands
before the paragraph rather than at `point-max'.  The new heading
must land at end of buffer -- after the paragraph -- so the
paragraph is preserved as zeroth-section content rather than being
absorbed into the new heading's body.  Pins the don't-absorb rule
the walker design encodes (see the `#'-prefixed branch's comment
in `org-mcp--skip-file-header-element')."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-title-and-paragraph
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-paragraph-no-heading
   :position "end"))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-paragraph-no-heading ()
  "Test position=\"start\" on a heading-less file with a plain paragraph.
With no top-level heading to insert before, `start' must coincide
with `end' at end-of-buffer; otherwise the paragraph that the
walker stopped on would be absorbed into the new heading's body.
Locks the same don't-absorb rule as the `position=\"end\"' twin
test for the `start' branch of `insert-top-level-heading'."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-title-and-paragraph
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-paragraph-no-heading
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-title-only-no-heading ()
  "Test inserting top-level TODO into a header-only file (no headings).
Locks the `insert-top-level-heading' empty-branch contract for the
`file-header only' case: with `#+TITLE:' present but no existing
heading, the new heading must be inserted flush after the title and
the existing header content preserved verbatim."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-title-only
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo-after-title-only))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-title-only ()
  "Test position=\"start\" on a header-only file (no headings).
Exercises the `has-headline=nil' arm of
`org-mcp--insert-top-level-heading' with a non-nil POSITION.
The empty-file tests don't catch a regression in that arm (no
header to differ on), and the title-only-default test doesn't
either (POSITION=nil); this one does."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-title-only
   "New Task"
   "TODO"
   '("work" "urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-todo-after-title-only
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-drawer-only-at-top ()
  "Test position=\"start\" when file starts with `:PROPERTIES:' directly.
With no `#+TITLE:' or other header line before the drawer, the walker
enters the `:PROPERTIES:' branch on its first iteration.  Pins that
the drawer recognition isn't gated on any preceding header content."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-drawer-only-at-top
   org-mcp-test--regex-top-level-start-drawer-only-at-top))

(ert-deftest
    org-mcp-test-add-todo-top-level-position-start-drawer-only-eof-no-newline ()
  "Test position=\"start\" on a drawer-only file ending mid-line at `:END:'.
Edge case: no heading follows the drawer and `:END:' has no
terminating `\\n', so `point-max' is at the end of `:END:'.  The
walker advances past the drawer, the heading search finds nothing,
and `start' collapses to `end' at `point-max'.  The drawer must
remain verbatim and the new heading must land with exactly one
`\\n' separator (supplied by `ensure-line-start')."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-drawer-only-eof-no-newline
   org-mcp-test--regex-top-level-start-drawer-only-eof-no-newline))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-bare-headings ()
  "Pin that `start' insert handles a file whose first character is `*'.
`validate-and-skip-file-header' returns `point-min' for a bare-headings file, so
the heading-search regex must match at position 1; `ensure-line-start'
must no-op at beginning-of-buffer.  No other start fixture exercises
the regex anchor at `point-min', so a regression in the search start
position would silently pass every other test."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-bare-headings-no-header
   org-mcp-test--regex-top-level-start-bare-headings))

(ert-deftest
    org-mcp-test-add-todo-top-level-position-start-orphan-deeper-before-top-level
    ()
  "Pin that the `start' top-level anchor is level-1-only, not any-level.
The MCP tool description says `position=\"start\"' at top level
inserts before the first existing top-level heading.  An orphan
deeper heading (e.g. `*** Foo' with no `* Parent') sitting before
the first level-1 heading must NOT win the placement anchor; the
new heading must land before the first true level-1 heading and
leave the orphan unparented.  A regression that broadens the
search regex back to `^\\*+ ' would silently reparent the orphan
as a level-3 child of the new heading."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-orphan-deeper-before-top-level
   org-mcp-test--regex-top-level-start-orphan-deeper-before-top-level))

(ert-deftest
    org-mcp-test-add-todo-top-level-position-start-orphan-deeper-only
    ()
  "Pin that `position=\"start\"' on a file whose only heading is an
orphan level-3 honours the `collapse to end' rule.  With no
level-1 heading anywhere, the documented behaviour for `start' is
to land at `point-max', so the new heading sits after the orphan
and the orphan stays unparented.  A regression that broadened the
search regex to `^\\*+ ' would match the orphan and insert before
it, silently reparenting it as a level-3 child of the new
heading.  Guards the level-1-only anchor on the no-level-1-heading
half of the placement contract; the
`orphan-deeper-before-top-level' twin guards the with-level-1 half."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-orphan-deeper-only
   org-mcp-test--regex-top-level-start-orphan-deeper-only))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-stacks-newest-first ()
  "Test that consecutive position=\"start\" inserts stack newest-first.
Two sequential `org-add-todo' calls with `position=\"start\"' must
leave the second insertion before the first one, so the order from
top is: second, first, original.  Locks the composition contract:
after a `start' insert the walker correctly identifies the
newly-inserted heading as the next \"first existing heading\"."
  (org-mcp-test--with-add-todo-setup
      test-file
      org-mcp-test--content-title-and-heading
      nil nil nil
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (basename (file-name-nondirectory test-file))
           (uuid-1
            (org-mcp-test--add-todo-start-and-return-uuid
             "First Top" parent-uri basename))
           (uuid-2
            (org-mcp-test--add-todo-start-and-return-uuid
             "Second Top" parent-uri basename)))
      (org-mcp-test--verify-file-matches
       test-file
       (format
        org-mcp-test--regex-top-level-start-stacks-newest-first
        (regexp-quote uuid-2) (regexp-quote uuid-1))))))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-after-indented-properties ()
  "Test position=\"start\" consumes an indented `:PROPERTIES:' drawer.
Org's parser accepts leading whitespace on drawer openers/closers
and still classifies a space-indented `:PROPERTIES:' at file level
as `property-drawer'.  The walker must mirror this: consume the
drawer through its indented `:END:' and place the new heading
after the entire header block.  Without the leading-whitespace
handling, the walker stops on the indented opener and
`position=\"start\"' silently reparents the file-level
`:CATEGORY:' onto the new heading."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-file-with-indented-properties
   org-mcp-test--regex-top-level-start-after-indented-properties))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-after-hashtag-paragraph ()
  "Test position=\"start\" consumes a `#hashtag'-style paragraph line.
Org's element parser classifies `#hashtag-paragraph' as `paragraph'
\(not `comment' -- Org comments require `# ' or `#$'), but the
walker's `^#' branch consumes any column-0 `#'-prefixed line by
design.  The new heading must land AFTER the paragraph so it stays
in the file's zeroth section.  A regression that narrowed `^#' to
match Org's strict comment grammar would stop on this line and the
paragraph would silently become the new heading's section body."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-file-with-hashtag-paragraph
   org-mcp-test--regex-top-level-start-after-hashtag-paragraph
   "New Task"
   '("work")))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-after-stray-end ()
  "Test position=\"start\" treats a bare `:END:' as ordinary content.
A `:END:' line with no preceding drawer opener is paragraph text in
Org's grammar, not a drawer opener.  The walker must classify it as
header-terminating content; otherwise the broadened drawer-opener
regex would match `:END:' itself and start hunting for a closing
`:END:' that does not exist.  The new heading lands before the
first existing heading and after the stray `:END:', so the stray
line stays as zeroth-section content rather than being absorbed
into the new heading's body."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-file-with-stray-end-at-top
   org-mcp-test--regex-top-level-start-after-stray-end
   "New Task"
   '("work")))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-after-lowercase-properties ()
  "Test `position=\"start\"' skips a file-level drawer whose opener
and closer use lowercase keywords (`:properties:' ... `:end:').
Pins the `case-fold-search' binding in
`org-mcp--skip-file-header-element' that lets the closer regex
match `:end:' as well as `:END:'."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-file-with-lowercase-properties-drawer
   org-mcp-test--regex-top-level-start-after-lowercase-properties))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-after-properties-blank-line ()
  "Test position=\"start\" skips `:PROPERTIES:' that follows `#+TITLE:'.
A blank line between `#+TITLE:' and the drawer causes Org's own parser
to classify the drawer as generic (not `property-drawer'), but the
walker still treats it as part of the leading header.  Pins this
permissive behavior: the alternative (stopping before the drawer) would
silently rebind the drawer to the newly inserted heading as its property
drawer."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-file-with-properties-after-blank-line
   org-mcp-test--regex-top-level-start-after-properties-blank-line))

(ert-deftest org-mcp-test-add-todo-top-level-position-start-with-header ()
  "Test position=\"start\" at top level adds after #+TITLE before first heading."
  (org-mcp-test--add-todo-top-level-start-and-check
   org-mcp-test--content-nested-siblings
   org-mcp-test--expected-regex-top-level-with-header-start))

(ert-deftest org-mcp-test-add-todo-top-level-with-header ()
  "Test adding top-level TODO after header comments appends at end of file."
  (let ((initial-content org-mcp-test--content-nested-siblings))
    (org-mcp-test--add-todo-and-check
     initial-content
     "New Top Task"
     "TODO"
     '("urgent")
     nil ; no body
     (format "org-headline://%s#" test-file)
     (file-name-nondirectory test-file)
     org-mcp-test--expected-regex-top-level-with-header-end)))

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

(ert-deftest org-mcp-test-add-todo-title-mixed-ascii-and-nbsp ()
  "Test that a title mixing ASCII whitespace and NBSP is rejected.
Pins the validator's combined character class against a refactor
that splits it into separate ASCII and NBSP alternations: such a
split would wrongly accept inputs like \" \\u00A0\" (space + NBSP)
because neither alternation matches the whole string."
  (org-mcp-test--assert-add-todo-invalid-title " \u00A0"))

(ert-deftest org-mcp-test-add-todo-embedded-newline-title ()
  "Test that adding TODO with embedded newline in title throws error."
  (org-mcp-test--assert-add-todo-invalid-title
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-add-todo-non-string-title ()
  "Pin that non-string `title' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal
`string-empty-p' or `string-match-p' calls would signal
`wrong-type-argument'."
  (org-mcp-test--assert-add-todo-non-string-title 42))

(ert-deftest org-mcp-test-add-todo-non-string-todo-state ()
  "Pin that non-string `todo_state' is rejected at the tool boundary.
Locks the tool-boundary string-field guard ahead of the value-keyword
guard: `org-mcp--validate-string-field' fires before
`org-mcp--validate-todo-state' so all string parameters share the
same error class regardless of whether the value is also
keyword-constrained."
  (org-mcp-test--assert-add-todo-non-string-todo-state 42))

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

(ert-deftest org-mcp-test-add-todo-leading-source-block-with-drawer-syntax ()
  "Walker treats `#+BEGIN_SRC' / `#+END_SRC' as a single bounded element.
A `:LOGBOOK:'-looking line inside a leading source block is literal
source content, not a real drawer.  Without block-pair awareness the
walker enters the drawer branch on `:LOGBOOK:', fails to find
`:END:' before `* Real Heading', and falsely errors with `Heading
line inside :LOGBOOK: drawer in file header block'.  Locks the
block-pair behavior by adding a child under `Real Heading' and
verifying the source block is preserved verbatim."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-leading-source-block-with-drawer-syntax
   "New Child"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Real%%20Heading" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-after-leading-source-block))

(ert-deftest org-mcp-test-add-todo-leading-source-block-unterminated ()
  "Walker errors on an unterminated `#+BEGIN_*' block in the file header.
A `#+BEGIN_SRC' opener with no matching `#+END_SRC' before end of
buffer is malformed.  Locks symmetric error handling between
unterminated drawers (already pinned) and unterminated blocks: the
walker silently consumes everything to end-of-buffer without an
error otherwise, so an unterminated block could swallow the rest of
the file -- including legitimate headings -- before the walker
terminated."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-leading-source-block-unterminated
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#Real%%20Heading" test-file)))

(ert-deftest org-mcp-test-add-todo-leading-dynamic-block-with-drawer-syntax ()
  "Walker treats `#+BEGIN:' / `#+END:' dynamic blocks as bounded elements.
Org has two block syntaxes: named blocks (`#+BEGIN_SRC' /
`#+END_SRC') and dynamic blocks (`#+BEGIN: clocktable' / `#+END:',
colon-terminated, name after `:' rather than after `_').  Both
shapes can hold drawer-looking or heading-looking literal content
in their bodies, so both must be recognised by the walker;
otherwise a clocktable or columnview in the file header reopens
the same `Heading line inside :NAME: drawer in file header
block' false positive that the named-block handling closes."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-leading-dynamic-block-with-drawer-syntax
   "New Child"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Real%%20Heading" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-after-leading-dynamic-block))

(ert-deftest org-mcp-test-add-todo-leading-block-with-dotted-name ()
  "Walker recognises block names containing non-alphanumeric chars.
Org's parser accepts any non-whitespace in block names (e.g.
`#+BEGIN_my.block').  A narrower walker character class such as
`[-_[:alnum:]]' would let such an opener fall through to the
generic `#'-prefix branch and re-introduce the CR-004
false-positive for exotic names.  Locks the alignment with the
body-block validator's `\\\\S-+' convention."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-leading-block-with-dotted-name
   "New Child"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Real%%20Heading" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-after-leading-dotted-block))

(ert-deftest org-mcp-test-add-todo-child-parent-uri-trailing-slash-before-fragment ()
  "Test that `org-headline://FILE/#H' resolves like `org-headline://FILE#H'.
The trailing `/' on the documented top-level form
`org-headline://FILE/' canonicalises to the no-slash file path.
A URI that mixes the trailing-slash marker with a fragment
\(`org-headline://FILE/#H') is parseable and unambiguous and must
resolve to the same (file . headline) pair as the no-slash form.
Without uniform stripping in `org-mcp--split-headline-uri', the
file part becomes `FILE/' and `validate-file-access' fails with a
misleading not-allowed error for a URI that just happens to mix
conventions."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s/#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-under-parent))

(defun org-mcp-test--assert-add-todo-error-message (parent-uri expected-substr)
  "Call `org-add-todo' on an empty file and assert the tool error message.
PARENT-URI is the parent_uri value to pass through the wire-protocol
boundary.  EXPECTED-SUBSTR is a literal substring of the expected
`mcp-server-lib-tool-error' message; matched via `regexp-quote'.
Used to lock the validation-boundary error class for malformed URIs
that previously surfaced as a misleading downstream error."
  (org-mcp-test--with-temp-org-files
      ((test-file ""))
    (let* ((params
            (org-mcp-test--build-add-todo-params
             "Task" "TODO" nil nil parent-uri nil))
           (request
            (mcp-server-lib-create-tools-call-request
             "org-add-todo" 1 params))
           (response
            (mcp-server-lib-process-jsonrpc-parsed
             request mcp-server-lib-ert-server-id))
           (err
            (should-error
             (mcp-server-lib-ert-process-tool-response response)
             :type 'mcp-server-lib-tool-error)))
      (should
       (string-match-p
        (regexp-quote expected-substr)
        (error-message-string err))))))

(ert-deftest org-mcp-test-add-todo-bare-org-id-parent-uri-rejected ()
  "Bare `org-id://' parent_uri is rejected at the validation boundary.
A bare prefix carries no UUID, so it cannot identify a parent.  This
locks the error class as the URI-format validation error rather than
the misleading downstream `Cannot find ID '\\='\\='' shape that a
no-meaningful-content lookup would surface."
  (org-mcp-test--assert-add-todo-error-message
   "org-id://"
   "Invalid resource URI format: org-id://"))

(ert-deftest org-mcp-test-add-todo-bare-org-headline-parent-uri-rejected ()
  "Bare `org-headline://' parent_uri is rejected at the validation boundary.
A bare prefix carries no filename, so it cannot identify a parent.
This locks the error class as the URI-format validation error rather
than a downstream file-access error from passing an empty filename
through `org-mcp--validate-file-access'."
  (org-mcp-test--assert-add-todo-error-message
   "org-headline://"
   "Invalid resource URI format: org-headline://"))

(ert-deftest org-mcp-test-add-todo-whitespace-org-id-parent-uri-rejected ()
  "Whitespace-only `org-id://' parent_uri is rejected at the dispatch boundary.
The suffix after the prefix carries no meaningful content -- semantically
the same as a bare prefix -- so the dispatch boundary must classify it
as a URI-format error, not let it fall through to a downstream
`Cannot find ID '   '' shape with a wasted full-allowed-files scan."
  (org-mcp-test--assert-add-todo-error-message
   "org-id://   "
   "Invalid resource URI format: org-id://   "))

(ert-deftest org-mcp-test-add-todo-nbsp-org-headline-parent-uri-rejected ()
  "NBSP-only `org-headline://' parent_uri is rejected at the dispatch boundary.
NBSP (U+00A0) carries no meaningful content for a URI suffix.  Emacs
27.2's `[[:space:]]' class does NOT match NBSP, so a `string-blank-p'
check would let this slip through; the dispatch boundary must
explicitly match NBSP so a no-meaningful-content suffix is rejected
with the URI-format validation error regardless of which blank
character was used."
  (org-mcp-test--assert-add-todo-error-message
   "org-headline://\u00A0"
   "Invalid resource URI format: org-headline://\u00A0"))

(ert-deftest org-mcp-test-add-todo-doubly-prefixed-parent-uri-rejected ()
  "Doubly-prefixed `org-id://org-headline://...' parent_uri is rejected.
A suffix that itself contains another URI scheme separator
indicates a doubly-prefixed (pasted-twice) URI rather than an ID
to resolve.  Without an explicit guard at `extract-uri-suffix'
the inner scheme is treated as the ID and surfaces downstream as
`Cannot find ID 'org-headline://foo'' -- misleading because the
actual problem is the URI shape, not the ID."
  (org-mcp-test--assert-add-todo-error-message
   "org-id://org-headline://foo"
   "Invalid resource URI format: org-id://org-headline://foo"))

(ert-deftest org-mcp-test-add-todo-empty-after-uri-rejected ()
  "Test that adding a child TODO with empty `after_uri' is rejected.
Empty string is distinct from absent/nil: absent appends as last
child, but `\"\"' must error."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   ""
   nil
   "must not be empty or whitespace-only"))

(ert-deftest org-mcp-test-add-todo-whitespace-after-uri-rejected ()
  "Test that adding a child TODO with whitespace-only `after_uri' is rejected.
A whitespace-only string carries the same no-meaningful-content
intent as `\"\"' (which is already rejected by
`org-mcp-test-add-todo-empty-after-uri-rejected') but slipped
through the `string-empty-p' guard and surfaced as the prefix-shape
`Field after_uri is not org-id://: ...' error.  Lock the
no-meaningful-content rejection at the validation boundary so
whitespace and the empty string take the same error path."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "   "
   nil
   "must not be empty or whitespace-only"))

(ert-deftest org-mcp-test-add-todo-nbsp-after-uri-rejected ()
  "Test that `after_uri' containing only NBSP (U+00A0) is rejected.
On Emacs 27.2 `[[:space:]]' does not match NBSP, so a pure-NBSP
`after_uri' must take the same error path as ASCII whitespace via
the explicit NBSP branch of the validator's combined character
class.  Locks that branch against a refactor that drops NBSP
coverage and lets a whitespace-only value through."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "\u00A0"
   nil
   "must not be empty or whitespace-only"))

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

(ert-deftest org-mcp-test-add-todo-position-nil-wire-accepted ()
  "Test that an explicit JSON `null' on the wire for `position' succeeds.
The positive half of the `position' wire contract: JSON `null'
must be accepted as equivalent to omitting the key, landing on
the default `end' placement.  Like its `after_uri' sibling, this
bypasses `org-mcp-test--build-add-todo-params' (which strips nil
optional keys) and builds the params alist directly with
`(cons 'position nil)' so the wire-null contract is pinned
independent of helper shape -- a regression that tightened
`org-mcp--validate-position' to require an explicit \"start\" or
\"end\" string would otherwise pass every existing position
test, since none of them put nil on the wire."
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
                  (cons 'position nil)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-add-todo" params))
           (result (json-read-from-string result-text)))
      (org-mcp-test--assert-add-todo-result-shape
       result "Child Task" (file-name-nondirectory test-file))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-child-under-parent))))

(ert-deftest
    org-mcp-test-add-todo-position-nil-with-after-uri-wire-accepted ()
  "JSON `null' for `position' must not trip the mutex with `after_uri'.
The position/after_uri mutex must read JSON `null' as equivalent
to omitting the key, so `{\"position\": null, \"after_uri\":
<id>}' succeeds and honours the `after_uri' placement.  At the
tool boundary an absent key and a wire-level JSON `null' are
indistinguishable -- both arrive as Elisp nil through
`json-read-from-string' -- and the project's wire contract is
that nil means \"unspecified\".  Bypasses
`org-mcp-test--build-add-todo-params' (which strips nil optional
keys) and builds the params alist directly with `(cons 'position
nil)' so a regression that tightened
`org-mcp--check-position-after-uri-mutex' to trip on any non-nil
raw `position' value (including wire null) would fail here."
  (org-mcp-test--with-add-todo-setup
      test-file org-mcp-test--content-nested-siblings nil nil
      (list org-mcp-test--content-nested-siblings-parent-id
            org-mcp-test--content-with-id-id)
    (let* ((params
            (list (cons 'title "New Task After Second")
                  (cons 'todo_state "TODO")
                  (cons 'tags '("work"))
                  (cons 'body nil)
                  (cons 'parent_uri
                        (format "org-headline://%s#Parent%%20Task"
                                test-file))
                  (cons 'after_uri
                        org-mcp-test--content-with-id-uri)
                  (cons 'position nil)))
           (result-text
            (mcp-server-lib-ert-call-tool "org-add-todo" params))
           (result (json-read-from-string result-text)))
      (org-mcp-test--assert-add-todo-result-shape
       result "New Task After Second"
       (file-name-nondirectory test-file))
      (org-mcp-test--verify-file-matches
       test-file org-mcp-test--regex-todo-after-second-child))))

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
Reproduces the emacs.org scenario: level 2 parent (via path), level 3 sibling (via ID).
Fixture ends mid-line at `point-max', also locking the no-body EOF path."
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

(ert-deftest org-mcp-test-add-todo-with-body-containing-deeper-headline ()
  "Test that `:ID:' lands on the new heading, not on a deeper
headline inside the body.
`org-mcp--validate-body-no-headlines' deliberately allows headlines
strictly deeper than the new heading's level (they form valid child
structure).  Regression: with such a body, the final
`org-back-to-heading t' on the post-body point used to walk
backward to the deepest headline in the body, so
`org-id-get-create' tagged the wrong entry and the tool returned
that entry's URI instead of the new heading's."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task with Sub Heading"
   "TODO"
   '("work")
   "Some text.\n** Sub heading in body\nMore text."
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-todo-with-body-containing-deeper-heading))

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

(ert-deftest org-mcp-test-add-todo-top-level-end-with-body ()
  "Test top-level insert with body lands without a trailing blank line.
The default-end branch of `org-mcp--insert-top-level-heading'
inserts `* TITLE' manually without a trailing `\\n', which pairs
with the caller's `(insert \"\\n\" body)' form to produce exactly
one `\\n' after the body.  Switching the branch to
`org-mcp--insert-heading-line' (its own trailing `\\n' would
double up with the body-insertion form) would land a trailing
blank line at end of file."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-heading-with-trailing-newline
   "New Top Task"
   "TODO"
   '("work")
   org-mcp-test--body-text-multiline
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-end-with-body))

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

(ert-deftest org-mcp-test-add-todo-body-with-asterisk-tab-content ()
  "Test that body containing `*\\tfoo' is accepted.
Org's heading grammar requires a literal space after the stars,
so `*\\tfoo' parses as a paragraph rather than a heading.  Pins
that `org-mcp--validate-body-no-headlines' matches Org's grammar
and does not over-reject tab-after-stars body content."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-empty
   "Task"
   "TODO"
   '("work")
   "Some initial text.\n*\tnot a heading"
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   (concat
    "\\`\\* TODO Task +:work:\n"
    org-mcp-test--regex-id-drawer
    "Some initial text\\.\n"
    "\\*\tnot a heading\n\\'")))

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

(ert-deftest org-mcp-test-add-todo-non-string-parent-uri ()
  "Pin that non-string `parent_uri' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream URI
parsing runs."
  (org-mcp-test--assert-add-todo-non-string-parent-uri 42))

(ert-deftest org-mcp-test-add-todo-non-string-body ()
  "Pin that non-string `body' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before downstream
`string-match' or `insert' calls would signal `wrong-type-argument'."
  (org-mcp-test--assert-add-todo-non-string-body 42))

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

(ert-deftest org-mcp-test-add-todo-after-uri-last-child-parent-followed-by-other ()
  "Test after_uri at parent's last child with following top-level heading.
The matched sibling is the parent's last (and only) child; the parent
is followed by another top-level heading.  `org-end-of-subtree' of
the matched sibling lands at column 0 of that following top-level
heading.  Locks the no-spurious-blank-line invariant at the after_uri
call site, complementing
`org-mcp-test-add-todo-child-end-parent-followed-by-other' at the
no-after_uri call site through `org-mcp--position-for-new-child'."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-single-child-then-other-top
   "New After Only"
   "TODO"
   '("work")
   nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-after-uri-no-blank-before-other-top
   :todo-keywords '((sequence "TODO" "|" "DONE"))
   :tag-alist '("work")
   :ids `(,org-mcp-test--content-parent-single-child-then-other-top-id)
   :after-uri (format "org-id://%s"
                      org-mcp-test--content-parent-single-child-then-other-top-id)))

(ert-deftest org-mcp-test-add-todo-after-uri-not-sibling ()
  "Test error when `after_uri's referent is not a child of `parent_uri'.
The fixture's `Childless Parent' has no children; the registered ID
points to `Unrelated Child' under `Other Parent'.  The walker must
reject the placement with an error naming the missing sibling, and
the file must be left unchanged."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-two-parents-one-with-id-child
   "Childless%20Parent"
   (format "org-id://%s"
           org-mcp-test--content-two-parents-one-with-id-child-id)
   `(,org-mcp-test--content-two-parents-one-with-id-child-id)
   "not found under parent"))

(ert-deftest org-mcp-test-add-todo-after-uri-id-among-children-misses ()
  "Test error when parent has children but none has the requested ID.
Targets `Parent Task' in the nested-siblings fixture, which has three
children.  The after_uri is an `org-id://' URI with a UUID that does
not appear as a child's `:ID:' property, so the walker traverses all
siblings, fails to match, and rejects the placement.  This is the
companion to `add-todo-after-uri-not-sibling' (which covers the
no-children branch) and exercises the has-children-none-match branch
of `org-mcp--position-after-sibling'."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "org-id://00000000-0000-0000-0000-000000000000"
   nil
   "not found under parent"))

(ert-deftest org-mcp-test-add-todo-after-uri-wrong-scheme ()
  "Pin rejection of `after_uri' that is not in `org-id://' form.
The walker only accepts `org-id://{uuid}' as a sibling reference; any
other URI scheme is rejected at validation time, before the sibling
lookup runs."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   (format "org-headline://%s#Parent%%20Task/Second%%20Child"
           test-file)
   nil
   "is not org-id://"))

(ert-deftest org-mcp-test-add-todo-after-uri-empty-id-after-prefix ()
  "Test that a bare `org-id://' (scheme prefix, empty UUID) is rejected.
The string satisfies the scheme-prefix check but extracts to an
empty UUID; the validation boundary must reject it rather than
let it fall through to sibling lookup."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "org-id://"
   nil
   "is not org-id://"))

(ert-deftest org-mcp-test-add-todo-after-uri-whitespace-uuid-after-prefix ()
  "Test that `org-id://' followed by a whitespace-only UUID is rejected.
The whole-URI whitespace guard catches `\"   \"' but not
`\"org-id://   \"' -- the extracted UUID is non-empty and slips
past `string-empty-p'.  The validator docstring promises this case
is caught (citing the same misleading `Sibling with ID  not found
under parent' surface that the empty-suffix branch was added to
prevent); lock the symmetric rejection at the validation boundary."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "org-id://   "
   nil
   "is not org-id://"))

(ert-deftest org-mcp-test-add-todo-after-uri-nbsp-uuid-after-prefix ()
  "Test that `org-id://' followed by an NBSP-only UUID is rejected.
On Emacs 27.2 `[[:space:]]' does not match NBSP, so an NBSP-only
suffix must take the same error path as ASCII whitespace via the
explicit NBSP branch of `org-mcp--blank-or-nbsp-only-p'.  Locks
that branch against a refactor that drops NBSP coverage from the
suffix check and lets the value through."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "org-id://\u00A0"
   nil
   "is not org-id://"))

(ert-deftest org-mcp-test-add-todo-after-uri-equals-parent-id ()
  "Test rejection when `after_uri's ID equals `parent_uri's own ID.
The sibling scan walks only the parent's children, so the parent
ID never matches there; the boundary must reject the input with
the `refers to parent_uri itself' discriminator rather than fall
through to the sibling-lookup path."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   (format "org-id://%s"
           org-mcp-test--content-nested-siblings-parent-id)
   nil
   "refers to parent_uri itself"))

(ert-deftest org-mcp-test-add-todo-after-uri-top-level-parent ()
  "Test rejection when `after_uri' is paired with a top-level `parent_uri'.
A top-level `parent_uri' (no fragment) has no sibling reference
slot, so any non-empty `after_uri' must be rejected at the tool
boundary.  Without this check the after_uri value is silently
discarded -- the parent-shape guard skips the after-sibling
walker for top-level inserts -- and the new heading lands at end
of file, giving callers a false-success path when they intended
after-sibling placement.  Uses the fixture parent's own `:ID:'
as the after_uri value to confirm the parent-shape check fires
before the ID lookup, independent of whether the ID exists."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-nested-siblings
   ""
   (format "org-id://%s"
           org-mcp-test--content-nested-siblings-parent-id)
   nil
   "top-level"))

(ert-deftest org-mcp-test-add-todo-after-uri-equals-ancestor-id ()
  "Test that the parent-self `after_uri' check uses the parent's own
`:ID:', not an inherited one.
Targets `Parent' (no `:ID:' of its own) under `Ancestor' (has
`:ID:').  Passing the ancestor's `:ID:' as `after_uri' must surface
`not found under parent' (the ID names neither the parent nor any
of its children), not the spurious `refers to parent_uri itself'
that would fire if `:ID:' were resolved through inheritance -- i.e.
if `org-mcp--position-after-sibling' passed INHERIT=t to
`org-entry-get', or if `:ID:' were added to
`org-use-property-inheritance'."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-ancestor-id-parent-no-id
   "Ancestor/Parent"
   (format "org-id://%s"
           org-mcp-test--content-ancestor-id-parent-no-id-ancestor-id)
   nil
   "not found under parent"))

(ert-deftest org-mcp-test-add-todo-after-uri-literal-nil-string ()
  "Test that `after_uri=\"org-id://nil\"' under a parent with no `:ID:'
surfaces the `not found under parent' diagnostic, not the parent-self
rejection.
Pins the nil-guard on the parent-self check: `org-entry-get nil \"ID\"'
returns nil when the parent lacks `:ID:', and `(string= nil \"nil\")'
returns t because `string=' coerces the symbol `nil' to its print
name.  Without the guard the check would fire spuriously and surface
the misleading `refers to parent_uri itself' message for the
literal-`nil' UUID."
  (org-mcp-test--assert-add-todo-after-uri-rejected
   org-mcp-test--content-ancestor-id-parent-no-id
   "Ancestor/Parent"
   "org-id://nil"
   nil
   (regexp-quote "Sibling with ID nil not found under parent")))

(ert-deftest org-mcp-test-add-todo-invalid-position ()
  "Test that `position' values outside {\"start\", \"end\"} are rejected.
Anchors on `Field position must be one of' so a regression that
widens the accepted set, or that routes the rejection through a
different validator (e.g. the mutex), surfaces as a test failure."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "middle"
   :error-message-regex "Field position must be one of"))

(ert-deftest org-mcp-test-add-todo-invalid-position-uppercase ()
  "Pin that `org-mcp--validate-position' compares POSITION case-sensitively,
rejecting \"START\".  A future `downcase'-tolerant rewrite would
silently widen the accepted set."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "START"
   :error-message-regex "Field position must be one of"))

(ert-deftest org-mcp-test-add-todo-invalid-position-empty ()
  "Test that `position=\"\"' is rejected, not silently treated as absent.
The empty string is distinct from nil: absent defaults to
end-of-parent placement, but `\"\"' is not in the allowed value
set.  Anchors on `Field position must be one of' so a regression
conflating `\"\"' with absence trips the test."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position ""
   :error-message-regex "Field position must be one of"))

(ert-deftest org-mcp-test-add-todo-invalid-position-whitespace ()
  "Test that `position' containing only ASCII whitespace is rejected.
The catch-all branch of `org-mcp--validate-position' (anything
other than nil, \"start\", or \"end\") rejects whitespace-only
strings.  Companion to `add-todo-whitespace-after-uri-rejected'
which pins the same property for `after_uri'."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "   "
   :error-message-regex "Field position must be one of"))

(ert-deftest org-mcp-test-add-todo-invalid-position-nbsp ()
  "Test that `position' containing only NBSP (U+00A0) is rejected.
Companion to `add-todo-nbsp-after-uri-rejected'.  `validate-position'
compares against the literal strings \"start\" and \"end\" with
`string=', so a pure-NBSP value falls through to the catch-all
error regardless of Emacs 27.2's `[[:space:]]' behaviour."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "\u00A0"
   :error-message-regex "Field position must be one of"))

(ert-deftest org-mcp-test-add-todo-invalid-position-empty-with-after-uri ()
  "Test ordering: empty `position' beats the mutex when `after_uri' is set.
With `position=\"\"' AND a valid `after_uri', two validation rules
could fire: the per-field rejection from `org-mcp--validate-position'
and the cross-field rejection from
`org-mcp--check-position-after-uri-mutex'.  Per-field validation
must run first, so the more specific `Field position must be one
of ...' error reaches the caller rather than the mutex's generic
`mutually exclusive' message."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-nested-siblings
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   :after-uri org-mcp-test--content-with-id-uri
   :position ""
   :ids (list org-mcp-test--content-nested-siblings-parent-id
              org-mcp-test--content-with-id-id)
   :error-message-regex "Field position must be one of"))

(ert-deftest
    org-mcp-test-add-todo-invalid-position-empty-with-after-uri-equals-parent-id ()
  "Test ordering: empty `position' beats the parent-self check.
Companion to `add-todo-invalid-position-empty-with-after-uri'
(mutex case): with `position=\"\"' AND `after_uri' that names
`parent_uri's own `:ID:', the per-field rejection from
`org-mcp--validate-position' must fire BEFORE the parent-self
check inside `org-mcp--position-after-sibling'.  Pins the
per-field-first contract for the parent-self cross-field check."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-nested-siblings
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   :after-uri
   (format "org-id://%s"
           org-mcp-test--content-nested-siblings-parent-id)
   :position ""
   :ids (list org-mcp-test--content-nested-siblings-parent-id)
   :error-message-regex "Field position must be one of"))

(ert-deftest
    org-mcp-test-add-todo-invalid-position-empty-with-after-uri-top-level-parent ()
  "Test ordering: empty `position' beats the not-top-level check.
Companion to `add-todo-invalid-position-empty-with-after-uri'
(mutex case): with `position=\"\"' AND a top-level `parent_uri' (no
fragment) combined with any `after_uri', the per-field rejection
from `org-mcp--validate-position' must fire BEFORE
`org-mcp--check-after-uri-not-top-level'.  Pins the
per-field-first contract for the not-top-level cross-field check."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-empty
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :after-uri "org-id://does-not-matter"
   :position ""
   :error-message-regex "Field position must be one of"))

(ert-deftest org-mcp-test-add-todo-position-non-string ()
  "Pin that non-string `position' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal `string='
comparison would signal `wrong-type-argument'."
  (org-mcp-test--assert-add-todo-non-string-position 42))

(ert-deftest org-mcp-test-add-todo-after-uri-non-string ()
  "Pin that non-string `after_uri' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal
`string-empty-p' call would signal `wrong-type-argument'."
  (org-mcp-test--assert-add-todo-non-string-after-uri 42))

(ert-deftest org-mcp-test-add-todo-top-level-unterminated-drawer-start ()
  "Test that position=\"start\" rejects an unterminated file-level drawer."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-drawer
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-unterminated-logbook-names-keyword ()
  "Pin the walker's keyword-agnostic contract on a well-known keyword.
An unterminated file-level `:LOGBOOK:' surfaces `:LOGBOOK:' in the
validation error, not the hardcoded `:PROPERTIES:' from the original
walker.  See `unterminated-customdrawer-names-keyword' for the
custom-name half that distinguishes \"walks any keyword\" from
\"hardcodes the well-known set\"."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-logbook
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "start"
   :error-message-regex ":LOGBOOK:"))

(ert-deftest org-mcp-test-add-todo-top-level-unterminated-customdrawer-names-keyword ()
  "Test the unterminated-drawer error names a custom drawer keyword.
Locks the walker's keyword-agnostic contract: an unterminated
file-level `:CUSTOMDRAWER:' must surface in the error message
literally, not collapsed to `:PROPERTIES:' or any other well-known
name.  The `:PROPERTIES:' and `:LOGBOOK:' tests alone cannot
distinguish \"walks any keyword\" from \"hardcodes the well-known
set\"."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-customdrawer
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "start"
   :error-message-regex ":CUSTOMDRAWER:"))

(ert-deftest
    org-mcp-test-add-todo-top-level-unterminated-hyphen-digit-drawer-names-keyword
    ()
  "Pin the drawer-keyword regex's `[-_[:alnum:]]+' character class on
non-alphabetic input.  An unterminated file-level `:MY-DRAWER_1:'
combines hyphen, underscore, and digit; the walker must enter the
drawer branch and surface the name in the error.  The
`:PROPERTIES:', `:LOGBOOK:', and `:CUSTOMDRAWER:' tests alone cannot
distinguish `[-_[:alnum:]]+' from a narrower `[[:alpha:]]+'."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-hyphen-digit-drawer
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "start"
   :error-message-regex ":MY-DRAWER_1:"))

(ert-deftest org-mcp-test-add-todo-top-level-eof-unterminated-drawer ()
  "Test eobp-exit drawer error uses the `Unterminated' wording.
A drawer body that runs to end of file (no heading line, no `:END:')
exits the walker's body-scan loop via `eobp', not via the heading
clause, and must surface the `Unterminated :NAME: drawer' diagnostic.
This pins the case-(a) branch separately from the heading-in-drawer
case (which uses the `Heading line inside' wording)."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-eof-unterminated-drawer
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "start"
   :error-message-regex "Unterminated :PROPERTIES: drawer"))

(ert-deftest org-mcp-test-add-todo-top-level-heading-in-drawer-start ()
  "Pin that position=\"start\" rejects a heading inside a file-level drawer.
Org's parser never recognizes a `:PROPERTIES:' drawer that contains a
heading line; the header-skip walker matches that behaviour and errors
out rather than silently consume the heading.  The diagnostic
distinguishes this case from a true unterminated drawer by naming the
heading as the proximate cause."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-heading-in-drawer
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "start"
   :error-message-regex "Heading line inside :PROPERTIES: drawer"))

(ert-deftest org-mcp-test-add-todo-top-level-end-typo-then-real-end ()
  "Test position=\"start\" treats only a bare `:END:' as drawer terminator.
A line such as `:END:typo' must not be mistaken for the closer; the
walker has to skip it and find the real `:END:' on the next line.
Locks in correct handling for a previously corruption-prone case."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-file-with-end-typo-then-real-end
   "New Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-start-after-end-typo
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-properties-opener-stray-text ()
  "Test position=\"start\" treats only a bare `:PROPERTIES:' as a drawer opener.
`org-element-parse-buffer' parses `:PROPERTIES: stray text\\n:END:\\n' as a
`paragraph' (Org's drawer-opener grammar requires the line to end at the
second colon), both in the input file and in the result file after the
insert.  The walker must therefore stop on the opener line; the new
heading then lands before the first existing heading and after the
paragraph, preserving the paragraph as zeroth-section content rather
than absorbing it into the new heading's body."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-file-with-properties-opener-stray
   "New Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-start-after-properties-stray
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-unterminated-drawer-end ()
  "Test that position=\"end\" also rejects an unterminated file-level drawer."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-drawer
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#" test-file)
   :position "end"))

(ert-deftest org-mcp-test-add-todo-parent-insert-unterminated-drawer ()
  "Test parent-specified insert validates the file header.
A child insert under a heading in a file whose own header block
contains a malformed `:PROPERTIES:' drawer is rejected with the
same validation error that fires for top-level inserts.  Locks
in that file-header validation is a per-tool guarantee, not a
per-call-path one."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-drawer
   "Child Task" "TODO" '("work") nil
   (format "org-headline://%s#Existing%%20Heading" test-file)))

(ert-deftest org-mcp-test-add-todo-parent-insert-unterminated-drawer-start ()
  "Test child insert with `position=\"start\"' validates the file header.
Companion to `add-todo-parent-insert-unterminated-drawer' (which
exercises the default-position routing): pins that file-header
validation also fires on the `position=\"start\"' child route, so a
refactor that conditionalises the walker on position would fail
loudly here.  Mirrors the explicit `start'/`end' coverage already
present for top-level inserts."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-file-with-unterminated-drawer
   "Child Task" "TODO" '("work") nil
   (format "org-headline://%s#Existing%%20Heading" test-file)
   :position "start"))

(ert-deftest org-mcp-test-add-todo-position-start-after-all-metadata ()
  "Test position=\"start\" with all metadata categories present.
The parent has a planning line, a property drawer, a logbook drawer,
and a plain-text body, plus one existing child.  Locks the documented
contract that the new heading lands after every metadata category
belonging to the parent, flush against the first existing child."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-with-all-metadata-and-child
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-at-start-after-all-metadata
   :position "start"))

(ert-deftest org-mcp-test-add-todo-position-start-parent-body-no-children ()
  "Test position=\"start\" on a parent with body but no children."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-with-body-no-children
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-at-start-no-children
   :position "start"))

(ert-deftest org-mcp-test-add-todo-position-start-parent-drawer-only ()
  "Test position=\"start\" on a parent with only a property drawer.
Parent has neither body nor children, only a `:PROPERTIES:' drawer.
Locks in that `org-mcp--position-for-new-child' falls through the
`org-goto-first-child' guard to `org-end-of-subtree' and lands past
the property drawer rather than inside it."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-drawer-only-no-children
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-after-drawer-only
   :position "start"))

(ert-deftest org-mcp-test-add-todo-position-start-parent-id-uri ()
  "Test position=\"start\" with parent specified via `org-id://' URI."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-id://%s"
           org-mcp-test--content-nested-siblings-parent-id)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-at-start-of-parent-pinned
   :ids (list org-mcp-test--content-nested-siblings-parent-id
              org-mcp-test--content-with-id-id)
   :position "start"
   :pin-new-heading-uuid t))

(ert-deftest org-mcp-test-add-todo-position-start-with-body ()
  "Test position=\"start\" with body content before existing children.
Verifies that body content is inserted between the new heading and
the existing first child, with one blank-line separator."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child Task"
   "TODO"
   '("work")
   org-mcp-test--body-text-multiline
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-at-start-with-body
   :position "start"))

(ert-deftest org-mcp-test-add-todo-position-start-with-children ()
  "Test position=\"start\" before existing children.
The parent fixture has a `:PROPERTIES:' drawer and a plain-text
body line; the new heading must land after both and before the
first existing child."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-at-start-of-parent-pinned
   :position "start"
   :pin-new-heading-uuid t))

(ert-deftest org-mcp-test-add-todo-position-end-with-children ()
  "Test position=\"end\" on a parent with children appends as last child."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-nested-siblings
   "Child Task"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-under-parent-pinned
   :position "end"
   :pin-new-heading-uuid t))

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

(ert-deftest org-mcp-test-add-todo-position-start-collapse-parent-followed-by-other ()
  "Test position=\"start\" on a parent with no children, followed by other content.
The `start' branch collapses to end-of-parent semantics when the parent
has no children, going through `org-mcp--position-for-new-child'.
This exercises the same root code path as
`org-mcp-test-add-todo-child-end-parent-followed-by-other' but at a
different call site, locking in the same no-blank-line invariant."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-no-children-then-other-top
   "New Child"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-start-collapse-no-blank-before-other-top
   :position "start"))

(ert-deftest org-mcp-test-add-todo-top-level-end-trailing-newline ()
  "Test position=\"end\" append into a file already ending with `\\n'.
Locks `ensure-line-start''s contract for the `point-max' insertion path:
when the buffer already ends with `\\n', the helper must not insert a
duplicate.  Most existing successful-insertion tests use fixtures
without a trailing newline (which exercise the
`ensure-line-start-inserts' branch); this one covers the
`already-has-newline-don't-double-up' branch."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-heading-with-trailing-newline
   "New Top Task"
   "TODO"
   '("urgent")
   nil ; no body
   (format "org-headline://%s#" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-top-level-end-single-newline
   :position "end"))

(ert-deftest org-mcp-test-add-todo-child-end-parent-trailing-newline ()
  "Test child append into a parent body ending in `\\n' at end of file.
Exercises `ensure-line-start''s no-op branch in the child code path:
`org-mcp--position-for-new-child' lands point at `point-max', and
`(char-before)' is the trailing `\\n', so the helper must not insert
a duplicate separator.  Companion to
`org-mcp-test-add-todo-top-level-end-trailing-newline' which covers
the same `(char-before)' = `\\n' branch from the top-level path."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-with-body-trailing-newline
   "New Child"
   "TODO"
   '("work")
   nil ; no body
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-end-single-newline))

(ert-deftest org-mcp-test-add-todo-child-end-parent-trailing-newline-with-body ()
  "Test body insert via `position=\"end\"' when parent is last subtree.
Companion to `add-todo-child-end-parent-trailing-newline' (no body)
and `add-todo-after-uri-eof-no-trailing-newline-with-body' (body via
after-uri).  Pins the `heading-newline-at-eof' trim on the
`position-for-new-child 'end'' code path: without coverage here,
the trim's interaction with `ensure-line-start''s no-op branch is
only locked in via the after-uri route."
  (org-mcp-test--add-todo-and-check
   org-mcp-test--content-parent-with-body-trailing-newline
   "New Child"
   "TODO"
   '("work")
   org-mcp-test--body-text-multiline
   (format "org-headline://%s#Parent%%20Task" test-file)
   (file-name-nondirectory test-file)
   org-mcp-test--regex-child-end-with-body-single-newline))

(ert-deftest org-mcp-test-add-todo-position-start-with-after-uri ()
  "Test that `position=\"start\"' combined with `after_uri' is rejected.
Uses `org-id://does-not-matter' as the after_uri so the mutex
must fire BEFORE URI resolution — otherwise the downstream
\"Sibling not found\" error would surface instead.  Anchors on
`mutually exclusive' to pin both the error class and that
ordering."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-nested-siblings
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   :after-uri "org-id://does-not-matter"
   :position "start"
   :error-message-regex "mutually exclusive"))

(ert-deftest org-mcp-test-add-todo-position-end-with-after-uri ()
  "Test that explicit `position=\"end\"' combined with `after_uri' is rejected.
`\"end\"' is the placement that a nil/absent `position' would also
select by default, but the mutex distinguishes an explicit value
from omission and still rejects the pair.  Anchors on `mutually
exclusive' to pin that the mutex consults the raw pre-normalised
string, not the post-validation symbol — otherwise an `end' symbol
would be indistinguishable from a nil that defaulted to it."
  (org-mcp-test--call-add-todo-expecting-error
   org-mcp-test--content-nested-siblings
   "New Task" "TODO" '("work") nil
   (format "org-headline://%s#Parent%%20Task" test-file)
   :after-uri org-mcp-test--content-with-id-uri
   :position "end"
   :error-message-regex "mutually exclusive"))

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

;;; org-rename-headline tests

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

(ert-deftest org-mcp-test-rename-headline-unterminated-drawer ()
  "Test that `org-rename-headline' validates the file header.
A rename targeting a heading in a file whose own header block contains
a malformed `:PROPERTIES:' drawer is rejected before any modification."
  (org-mcp-test--call-rename-headline-expecting-error
   org-mcp-test--content-malformed-header-with-todo-and-body
   "Existing%20Heading"
   "Existing Heading"
   "Renamed Heading"))

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

(ert-deftest org-mcp-test-rename-headline-non-string-new-title ()
  "Pin that non-string `new_title' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before
`org-mcp--validate-headline-title' would signal `wrong-type-argument'
through its `string-empty-p' or `string-match-p' calls."
  (org-mcp-test--call-rename-headline-expecting-error
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   "Parent Task"
   42 ; non-string new_title
   (org-mcp-test--field-non-string-regex "new_title" 42)))

(ert-deftest org-mcp-test-rename-headline-non-string-current-title ()
  "Pin that non-string `current_title' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal `string='
title-match comparison would signal `wrong-type-argument'."
  (org-mcp-test--call-rename-headline-expecting-error
   org-mcp-test--content-nested-siblings
   "Parent%20Task"
   42 ; non-string current_title
   "Should Fail"
   (org-mcp-test--field-non-string-regex "current_title" 42)))

(ert-deftest org-mcp-test-rename-headline-non-string-uri ()
  "Pin that non-string `uri' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream URI
parsing runs."
  (org-mcp-test--call-rename-headline-expecting-error
   org-mcp-test--content-nested-siblings
   42 ; non-string uri
   "Whatever"
   "Should Fail"
   (org-mcp-test--field-non-string-regex "uri" 42)))

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

(ert-deftest org-mcp-test-edit-body-non-string-new-body ()
  "Pin that non-string `new_body' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before
`validate-body-no-unbalanced-blocks' or downstream `insert' calls
would signal `wrong-type-argument'."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     "anything"
     42 ; non-string new_body
     nil
     (org-mcp-test--field-non-string-regex "new_body" 42))))

(ert-deftest org-mcp-test-edit-body-non-string-old-body ()
  "Pin that non-string `old_body' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal `string='
or `regexp-quote' calls would signal `wrong-type-argument'."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     org-mcp-test--content-with-id-uri
     42 ; non-string old_body
     "replacement"
     nil
     (org-mcp-test--field-non-string-regex "old_body" 42))))

(ert-deftest org-mcp-test-edit-body-non-string-resource-uri ()
  "Pin that non-string `resource_uri' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream URI
parsing runs."
  (org-mcp-test--with-id-setup test-file
      org-mcp-test--content-nested-siblings
      `(,org-mcp-test--content-with-id-id)
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     42 ; non-string resource_uri
     "anything"
     "replacement"
     nil
     (org-mcp-test--field-non-string-regex "resource_uri" 42))))

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

(ert-deftest org-mcp-test-edit-body-unterminated-drawer ()
  "Test that `org-edit-body' validates the file header.
A body edit under a heading in a file whose own header block contains
a malformed `:PROPERTIES:' drawer is rejected with the same validation
error that fires for `org-add-todo', so file-header integrity is a
guarantee for every modifying tool."
  (org-mcp-test--with-temp-org-files
      ((test-file
        org-mcp-test--content-malformed-header-with-todo-and-body))
    (org-mcp-test--call-edit-body-expecting-error
     test-file
     (format "org-headline://%s#Existing%%20Heading" test-file)
     "Some body content."
     "REPLACED"
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
  "Test adding content to empty body with properties drawer.
The pre-existing `:ID:' drawer is preserved verbatim across the
edit -- no orphaned-ID cascade attaches a fresh `:ID:' to the
inserted content."
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

;; org-archive-subtree tests

(ert-deftest org-mcp-test-archive-subtree-success ()
  "Test successful subtree archiving by headline URI.
Archiving a headline that lacks an Org ID creates one, returns it as
an `org-id://' URI, and the new ID travels into the archive file with
the subtree."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let* ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                        test-file))
           (id (org-mcp-test--archive-subtree-id uri default-archive-file)))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-source-regex)
      (org-mcp-test--verify-file-matches
       default-archive-file
       (org-mcp-test--archive-file-regex id)))))

(ert-deftest org-mcp-test-archive-subtree-nested-child ()
  "Archive a nested (level-2) subtree reached via a `Parent/Child' path.
The child is navigated through its parent, archived to the separate
file with a freshly created ID, and removed from the source while the
parent and its body remain -- exercising the navigation/archive
composition the all-top-level fixtures cannot."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-nested nil
    (let* ((uri
            (format "org-headline://%s#Parent/Child%%20to%%20Archive"
                    test-file))
           (id (org-mcp-test--archive-subtree-id uri default-archive-file)))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-nested-source-regex)
      (org-mcp-test--verify-file-matches
       default-archive-file
       (org-mcp-test--archive-file-regex
        id "Child to Archive" "Child content here.")))))

(ert-deftest org-mcp-test-archive-subtree-by-id ()
  "Test successful subtree archiving by ID URI.
A headline that already has an ID keeps it, and the tool returns that
same ID as the `org-id://' URI."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-with-id-todo
      `(,org-mcp-test--content-with-id-id)
    (let* ((uri (format "org-id://%s" org-mcp-test--content-with-id-id))
           (id (org-mcp-test--archive-subtree-id uri default-archive-file)))
      (should (string= org-mcp-test--content-with-id-id id))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-source-regex)
      (org-mcp-test--verify-file-matches
       default-archive-file
       org-mcp-test--expected-archive-with-id-file-regex))))

(ert-deftest org-mcp-test-archive-subtree-modified-buffer-error ()
  "Test archive fails when file has unsaved changes."
  ;; No archive bindings needed: this aborts before any archive logic runs.
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-archive-simple))
   (org-mcp-test--with-file-buffer buffer test-file
     (with-current-buffer buffer
       (goto-char (point-max))
       (insert "\n* TODO Another Task")
       (should (buffer-modified-p)))
     (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                        test-file)))
       (org-mcp-test--assert-archive-error test-file uri
         org-mcp-test--archive-unsaved-error-regex)))))

(ert-deftest org-mcp-test-archive-subtree-honors-archive-property ()
  "Pin that a headline `:ARCHIVE:' property selects the archive location.
The returned `archive_file' and the file actually written follow the
per-headline `:ARCHIVE:' property in preference to the default
`org-archive-location', so the default `_archive' file is never
created."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-custom-location nil
    (org-mcp-test--with-archive-file custom-archive-file
        (concat test-file "_custom")
      (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                         test-file)))
        (org-mcp-test--archive-subtree uri custom-archive-file)
        (should (file-exists-p custom-archive-file))
        (should-not (file-exists-p default-archive-file))))))

(ert-deftest org-mcp-test-archive-subtree-guard-fires-on-custom-location ()
  "Pin that the unsaved-changes guard checks the resolved archive file,
not the default `_archive', when a per-headline `:ARCHIVE:' property
redirects the destination.  This catches drift between the tool's
pre-flight `org-archive--compute-location' call and the file
`org-archive-subtree' actually opens: the guard must fire on the
`_custom' file a modified buffer is visiting."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-custom-location nil
    (org-mcp-test--with-archive-file custom-archive-file
        (concat test-file "_custom")
      (org-mcp-test--with-file-buffer buffer custom-archive-file
        (with-current-buffer buffer
          (insert org-mcp-test--content-preexisting-archive-entry)
          (should (buffer-modified-p)))
        (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                           test-file)))
          (org-mcp-test--assert-archive-error test-file uri
            org-mcp-test--archive-unsaved-error-regex))))))

(ert-deftest org-mcp-test-archive-subtree-honors-archive-keyword ()
  "Pin that a file-level `#+ARCHIVE:' keyword selects the archive location.
With no per-headline `:ARCHIVE:' property, the returned `archive_file'
and the file actually written follow the `#+ARCHIVE:' keyword in
preference to the default `org-archive-location', so the default
`_archive' file is never created."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-keyword-location nil
    (org-mcp-test--with-archive-file custom-archive-file
        (concat test-file "_fromkeyword")
      (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                         test-file)))
        (org-mcp-test--archive-subtree uri custom-archive-file)
        (should (file-exists-p custom-archive-file))
        (should-not (file-exists-p default-archive-file))))))

(ert-deftest org-mcp-test-archive-subtree-non-string-uri ()
  "Pin that non-string `uri' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream URI
parsing runs."
  ;; No archive bindings needed: this aborts before any archive logic runs.
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-archive-simple))
   (org-mcp-test--assert-archive-error test-file 42
     (org-mcp-test--field-non-string-regex "uri" 42))))

(ert-deftest org-mcp-test-archive-subtree-nonexistent-headline ()
  "Pin that archiving a headline absent from the file errors and leaves
the source unchanged.  `org-mcp--goto-headline-from-uri' runs before
any archive logic, so a missing headline aborts the tool with no
write."
  ;; No archive bindings needed: this aborts before any archive logic runs.
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-archive-simple))
   (let ((uri (format "org-headline://%s#Nonexistent%%20Task"
                      test-file)))
     (org-mcp-test--assert-archive-error test-file uri
       "Cannot find headline: Nonexistent Task"))))

(ert-deftest org-mcp-test-archive-subtree-file-level-uri ()
  "Pin that a fragment-less `org-headline://' URI errors and leaves the
source unchanged.  Such a URI identifies a file but no headline, so the
tool must reject it through the `mcp-server-lib-tool-error' channel
rather than archive at point-min before the first heading."
  ;; No archive bindings needed: this aborts before any archive logic runs.
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-archive-simple))
   (let ((uri (format "org-headline://%s/" test-file)))
     (org-mcp-test--assert-archive-error test-file uri
       "URI must identify a headline to archive"))))

(ert-deftest org-mcp-test-archive-subtree-malformed-location-error ()
  "Pin that a malformed `:ARCHIVE:' location surfaces as a tool error.
A spec missing the `::' separator is user-controlled input, so it must
be reported through the same `mcp-server-lib-tool-error' channel as the
tool's other input validation -- not escape as a JSON-RPC internal
error -- and leave the source file unchanged."
  ;; No archive bindings needed: this aborts at archive-location
  ;; computation, before any archive logic runs.
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-archive-malformed-location))
   (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                      test-file)))
     (org-mcp-test--assert-archive-error test-file uri
       "Invalid archive location"))))

(ert-deftest org-mcp-test-archive-subtree-returned-uri-resolution ()
  "Pin resolvability of the `org-id://' URI returned by archiving.
After archiving, the returned `org-id://' URI is unresolvable when
the archive file is outside `org-mcp-allowed-files', and resolves to
the archived headline once the archive file is added to the allowed
list."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let* ((archive-uri
            (format "org-headline://%s#Task%%20to%%20Archive"
                    test-file))
           (id (org-mcp-test--archive-subtree-id
                archive-uri default-archive-file))
           (returned-uri (concat "org-id://" id)))
      ;; Archive file not allowed: the returned URI is unresolvable.
      (org-mcp-test--read-resource-expecting-error
       returned-uri
       (format
        "ID '%s' resolves to a file not in the allowed list" id))
      ;; Archive file allowed: the URI resolves to the moved entry.
      (let ((org-mcp-allowed-files (list test-file default-archive-file)))
        (org-mcp-test--verify-resource-text-matches
         returned-uri
         (concat ":ID:[ \t]+" (regexp-quote id)))))))

(ert-deftest org-mcp-test-archive-subtree-in-file-location ()
  "Pin in-file archiving: when the `:ARCHIVE:' location has an empty
file part, the subtree moves under a heading within the source file
itself, the returned `archive_file' is the source path, and no
separate archive file is created.  Because the entry stays in the
source file, which is already allowed, the returned `org-id://' URI
resolves via `resources/read' immediately -- with no change to
`org-mcp-allowed-files', unlike the separate-file case in
`org-mcp-test-archive-subtree-returned-uri-resolution'."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-infile-location nil
    (let* ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                        test-file))
           (id (org-mcp-test--archive-subtree-id uri test-file))
           (returned-uri (concat "org-id://" id)))
      (org-mcp-test--verify-resource-text-matches
       returned-uri
       (concat ":ID:[ \t]+" (regexp-quote id)))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-infile-source-regex)
      (should-not (file-exists-p default-archive-file)))))

(ert-deftest org-mcp-test-archive-subtree-kills-opened-archive-buffer ()
  "Pin that archiving leaves no buffer visiting the archive file when
the tool itself opened it.  `org-archive-subtree' opens the archive
file via `find-file-noselect'; the tool kills that buffer so a
long-running server does not accumulate buffers visiting archive
files."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file)))
      (org-mcp-test--archive-subtree uri default-archive-file)
      (should-not (find-buffer-visiting default-archive-file)))))

(ert-deftest org-mcp-test-archive-subtree-keeps-preexisting-archive-buffer ()
  "Pin that archiving preserves a buffer the user already had open on
the archive file, killing only buffers the tool opened itself."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file))
          (buffer (find-file-noselect default-archive-file)))
      (org-mcp-test--archive-subtree uri default-archive-file)
      (should (buffer-live-p buffer)))))

(ert-deftest org-mcp-test-archive-subtree-refreshes-other-archive-buffers ()
  "Pin that archiving reverts *every* buffer visiting the archive file
to the freshly written on-disk content, not only the one
`org-archive-subtree' filled and the tool saved.  When two buffers
visit the archive file, the second would otherwise keep its stale
pre-archive content while disk holds the archived subtree."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file))
          (buffer-a (find-file-noselect default-archive-file))
          (buffer-b (generate-new-buffer "second-archive-visitor")))
      (with-current-buffer buffer-b
        (set-visited-file-name default-archive-file t)
        (set-buffer-modified-p nil))
      (org-mcp-test--archive-subtree uri default-archive-file)
      (org-mcp-test--verify-buffer-matches
       buffer-a org-mcp-test--expected-archive-simple-file-regex)
      (org-mcp-test--verify-buffer-matches
       buffer-b org-mcp-test--expected-archive-simple-file-regex))))

(ert-deftest org-mcp-test-archive-subtree-saves-archive-when-save-disabled ()
  "Pin that the archive file is written to disk even when
`org-archive-subtree-save-file-p' is nil.  The tool persists the
archive itself rather than relying on org-archive's save policy, so a
user setting that disables org-archive's own save cannot silently
drop the archived entry while the source is emptied."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((org-archive-subtree-save-file-p nil)
          (uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file)))
      (org-mcp-test--archive-subtree uri default-archive-file)
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-source-regex)
      (org-mcp-test--verify-file-matches
       default-archive-file
       org-mcp-test--expected-archive-simple-file-regex))))

(ert-deftest org-mcp-test-archive-subtree-modified-archive-buffer-error ()
  "Pin that archiving fails when the destination archive file has
unsaved changes in a visiting buffer, mirroring the source-file
guard.  Without this guard `org-archive-subtree' would force-save the
archive buffer, flushing the user's unsaved edits to disk."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (org-mcp-test--with-file-buffer buffer default-archive-file
      (with-current-buffer buffer
        (insert org-mcp-test--content-preexisting-archive-entry)
        (should (buffer-modified-p)))
      (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                         test-file)))
        (org-mcp-test--assert-archive-error test-file uri
          org-mcp-test--archive-unsaved-error-regex)))))

(ert-deftest org-mcp-test-archive-subtree-ignores-default-command ()
  "Pin that archiving moves the subtree to a file regardless of
`org-archive-default-command'.  With the command customized to
`org-archive-set-tag' (which would otherwise tag the entry in place
without moving it), the tool still empties the source and writes the
archive file."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((org-archive-default-command 'org-archive-set-tag)
          (uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file)))
      (org-mcp-test--archive-subtree uri default-archive-file)
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-source-regex)
      (should (file-exists-p default-archive-file)))))

(ert-deftest org-mcp-test-archive-subtree-no-id-leak-on-archive-guard ()
  "Pin that a failed archive-file guard registers no Org ID.
When the destination archive file has unsaved changes the guard
aborts before `org-id-get-create' runs, so nothing is added to the
global `org-id-locations' for an archive that never happened.  Were
the ID created first it would point at the source file, which on the
aborted path never receives it on disk -- a stale location entry."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (org-mcp-test--with-file-buffer buffer default-archive-file
      (with-current-buffer buffer
        (insert org-mcp-test--content-preexisting-archive-entry)
        (should (buffer-modified-p)))
      (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                         test-file)))
        (org-mcp-test--assert-archive-error test-file uri
          org-mcp-test--archive-unsaved-error-regex)
        ;; `org-mcp-test--with-id-tracking' let-binds `org-id-locations'
        ;; to nil; with the guard aborting before `org-id-get-create'
        ;; nothing converts it to a populated hash table, so nil is the
        ;; positive signal that no ID leaked.  A non-nil value here
        ;; would mean the ID was created before the guard fired.
        (should (null org-id-locations))))))

(ert-deftest org-mcp-test-archive-subtree-persists-archive-before-source ()
  "Pin that the archive is saved to disk before the source is emptied.
With `org-archive-subtree-save-file-p' nil the tool is the only thing
that persists the archive.  Simulating a failure of the source write
leaves the archive file already on disk with the entry (a recoverable
duplicate) and the source still holding it, rather than dropping the
entry from both files."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((org-archive-subtree-save-file-p nil)
          (uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file))
          (orig-write-region (symbol-function 'write-region)))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end filename &rest args)
                   (if (string= (expand-file-name filename)
                                (expand-file-name test-file))
                       (error "Simulated source write failure")
                     (apply orig-write-region
                            start end filename args)))))
        (should
         (alist-get 'error (org-mcp-test--archive-tool-response uri))))
      (should (file-exists-p default-archive-file))
      (org-mcp-test--verify-file-matches
       default-archive-file
       org-mcp-test--expected-archive-simple-file-regex)
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-simple-unchanged-source-regex))))

(ert-deftest org-mcp-test-archive-subtree-kills-opened-buffer-on-error ()
  "Pin buffer cleanup and entry survival when the source refresh fails.
`org-mcp--refresh-file-buffers' is stubbed to signal only for the
source file, so it fires after both the archive and source files have
been written -- the archive refresh runs normally first.  The tool's
cleanup still kills the buffer it opened (no leaked buffer visiting the
archive file), the source is emptied on disk, and the archived entry
survives on disk in the archive file rather than being lost."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file))
          (orig-refresh (symbol-function 'org-mcp--refresh-file-buffers)))
      (cl-letf (((symbol-function 'org-mcp--refresh-file-buffers)
                 (lambda (file &rest args)
                   (if (string= (expand-file-name file)
                                (expand-file-name test-file))
                       (error "Simulated refresh failure")
                     (apply orig-refresh file args)))))
        (should
         (alist-get 'error (org-mcp-test--archive-tool-response uri))))
      (should-not (find-buffer-visiting default-archive-file))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-source-regex)
      (org-mcp-test--verify-file-matches
       default-archive-file
       org-mcp-test--expected-archive-simple-file-regex))))

(ert-deftest org-mcp-test-archive-subtree-kills-opened-buffer-on-save-failure ()
  "Pin cleanup and source safety when the archive `save-buffer' fails.
With `org-archive-subtree-save-file-p' nil the tool performs the only
save of the tool-opened archive buffer, so stubbing `save-buffer' to
signal exercises a failure before the source is emptied on disk.  The
cleanup still reclaims the buffer, the source file keeps the subtree,
and no archive file is written -- so the entry is never lost."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((org-archive-subtree-save-file-p nil)
          (uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file)))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _)
                   (error "Simulated save failure"))))
        (should
         (alist-get 'error (org-mcp-test--archive-tool-response uri))))
      (should-not (find-buffer-visiting default-archive-file))
      (should-not (file-exists-p default-archive-file))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-simple-unchanged-source-regex))))

(ert-deftest org-mcp-test-archive-subtree-tolerates-killed-archive-buffer ()
  "Pin that cleanup tolerates a tool-opened archive buffer already killed.
If a hook kills the archive buffer after it is saved but before the
unwind cleanup runs, the liveness guard keeps `with-current-buffer'
from erroring on a dead buffer, so the archive still succeeds.  The
source `write-region' is stubbed to kill the archive buffer first,
reproducing that ordering deterministically."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file))
          (orig-write-region (symbol-function 'write-region)))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end filename &rest args)
                   (when (string= (expand-file-name filename)
                                  (expand-file-name test-file))
                     (org-mcp-test--discard-buffer-visiting
                      default-archive-file))
                   (apply orig-write-region start end filename args))))
        (org-mcp-test--archive-subtree uri default-archive-file)
        (should-not (find-buffer-visiting default-archive-file))))))

(ert-deftest org-mcp-test-archive-subtree-kills-opened-buffer-on-archive-throw ()
  "Pin buffer cleanup when `org-archive-subtree' itself throws mid-move.
`org-archive-subtree' opens the archive buffer with `find-file-noselect'
and only later pastes into it; stubbing `org-paste-subtree' to signal
reproduces a throw after the buffer is opened but before the tool's own
persistence runs.  Cleanup must still reclaim the tool-opened archive
buffer, leave no archive file on disk, and keep the subtree in the
source so the entry is never lost."
  (org-mcp-test--with-archive-setup
      test-file org-mcp-test--content-archive-simple nil
    (let ((uri (format "org-headline://%s#Task%%20to%%20Archive"
                       test-file)))
      (cl-letf (((symbol-function 'org-paste-subtree)
                 (lambda (&rest _)
                   (error "Simulated archive paste failure"))))
        (should
         (alist-get 'error (org-mcp-test--archive-tool-response uri))))
      (should-not (find-buffer-visiting default-archive-file))
      (should-not (file-exists-p default-archive-file))
      (org-mcp-test--verify-file-matches
       test-file
       org-mcp-test--expected-archive-simple-unchanged-source-regex))))

;;; org-read-file tests

(ert-deftest org-mcp-test-tool-read-file ()
  "Test org-read-file tool returns same content as file resource."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   (let ((result-text (org-mcp-test--call-read-file test-file)))
     (should (string= result-text org-mcp-test--content-nested-siblings)))))

(ert-deftest org-mcp-test-tool-read-file-non-string-file ()
  "Pin that non-string `file' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream
file-path predicates run."
  (org-mcp-test--assert-tool-error-message-regex
   "org-read-file" '((file . 42))
   (org-mcp-test--field-non-string-regex "file" 42)))

;;; org-read-outline tests

(ert-deftest org-mcp-test-tool-read-outline ()
  "Test org-read-outline tool returns valid JSON outline structure."
  (org-mcp-test--with-temp-org-files
   ((test-file org-mcp-test--content-nested-siblings))
   (let* ((result (org-mcp-test--call-read-outline test-file))
          (headings (alist-get 'headings result)))
     (should (= (length headings) 1))
     (should (string= (alist-get 'title (aref headings 0)) "Parent Task")))))

(ert-deftest org-mcp-test-tool-read-outline-non-string-file ()
  "Pin that non-string `file' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream
file-path predicates run."
  (org-mcp-test--assert-tool-error-message-regex
   "org-read-outline" '((file . 42))
   (org-mcp-test--field-non-string-regex "file" 42)))

;;; org-read-headline tests

(ert-deftest org-mcp-test-tool-read-headline-non-string-file ()
  "Pin that non-string `file' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal `concat'
combining file and headline path would signal `wrong-type-argument'."
  (org-mcp-test--assert-tool-error-message-regex
   "org-read-headline"
   '((file . 42) (headline_path . "anything"))
   (org-mcp-test--field-non-string-regex "file" 42)))

(ert-deftest org-mcp-test-tool-read-headline-non-string-path ()
  "Pin that non-string `headline_path' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before the internal
`string-empty-p' / `concat' combining the headline path would signal
`wrong-type-argument'."
  (org-mcp-test--assert-tool-error-message-regex
   "org-read-headline"
   '((file . "/tmp/x.org") (headline_path . 42))
   (org-mcp-test--field-non-string-regex "headline_path" 42)))

(ert-deftest org-mcp-test-tool-read-headline-empty-path ()
  "Test org-read-headline with empty `headline_path' signals validation error.
Anchors on the full `Field headline_path must be non-empty; use
org-read-file tool to read entire files' wording so the
wire-protocol error contract is locked: a regression that
reintroduces the legacy `Parameter X must ...' shape or drops the
recovery hint surfaces as a test failure."
  (org-mcp-test--assert-tool-error-message-regex
   "org-read-headline"
   '((file . "/tmp/x.org") (headline_path . ""))
   (regexp-quote
    "Field headline_path must be non-empty; use \
org-read-file tool to read entire files")))

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

;;; org-read-by-id tests

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

(ert-deftest org-mcp-test-tool-read-by-id-non-string-uuid ()
  "Pin that non-string `uuid' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream ID
lookup runs."
  (org-mcp-test--assert-tool-error-message-regex
   "org-read-by-id" '((uuid . 42))
   (org-mcp-test--field-non-string-regex "uuid" 42)))

;;; Resource tests

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

;;; Meta tests

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
header, the `NEWS' top-section heading, the `org-mcp--version'
defconst literal in source, and the loaded `org-mcp--version' constant
reported as `serverInfo.version' must all report the same version;
drift between them leads to MELPA/Eask metadata inconsistencies at
release time, to changelog/release-state confusion, and to a stale
version in the MCP `initialize' result.  Pinning the source literal
also locks in the hardcoded-string contract: `org-mcp--version' must
be a plain literal, not a value derived at load time."
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
         (const-version
          (and (string-match
                "(defconst[ \t\n]+org-mcp--version[ \t\n]+\"\\([^\"]+\\)\""
                el-content)
               (match-string 1 el-content)))
         (news-version
          (and (string-match
                "^\\* Changes in org-mcp \\(\\S-+\\)"
                news-content)
               (match-string 1 news-content))))
    (should (stringp eask-version))
    (should (stringp el-version))
    (should (stringp const-version))
    (should (stringp news-version))
    (should (equal eask-version el-version))
    (should (equal eask-version const-version))
    (should (equal eask-version news-version))
    (should (equal eask-version org-mcp--version))))

;;; org-grep helpers

(defun org-mcp-test--call-grep (pattern &optional file case-sensitive)
  "Call org-grep tool and return the parsed JSON result as alist.
PATTERN is the search string, FILE limits search to one file,
CASE-SENSITIVE controls case matching."
  (let* ((params `((pattern . ,pattern)
                   ,@(when file `((file . ,file)))
                   ,@(when case-sensitive `((case_sensitive . t)))))
         (result-json (mcp-server-lib-ert-call-tool "org-grep" params)))
    (json-parse-string result-json :object-type 'alist :array-type 'list)))

;;; org-grep tests

(ert-deftest org-mcp-test-grep-column-zero-star-non-heading-not-a-boundary ()
  "Pin that a column-0 `*bold*' line in the pre-heading area is not
treated as a section boundary.  Matches after it (up to the first
real heading) must still appear in the pre-heading group.

Regression for the `^\\*' vs `^\\*+ ' boundary-regex bug: the bare
`^\\*' matched `*bold*' and truncated the pre-heading region there,
dropping matches that followed."
  (org-mcp-test--with-temp-org-files
   ((test-file "*bold* text\ntoken found here\n* Real Heading\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (equal (alist-get 'headline_path (car groups)) nil))
     (should (= (length (alist-get 'matches (car groups))) 1))
     (should (= (alist-get 'line (car (alist-get 'matches (car groups))))
                2)))))

(ert-deftest org-mcp-test-grep-column-zero-star-in-body-not-a-boundary ()
  "Pin that a column-0 `*bold*' line inside a section body does not
truncate that section.  Matches after it (before the next real
heading) must still appear in the same section group.

Regression for the `^\\*' vs `^\\*+ ' boundary-regex bug: the bare
`^\\*' matched `*bold*' and set section-end there, dropping the
subsequent match."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\n*bold* line\ntoken found here\n* Next\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (equal (alist-get 'headline_path (car groups)) '("Section")))
     (should (= (length (alist-get 'matches (car groups))) 1)))))

(ert-deftest org-mcp-test-grep-pattern-is-literal-not-regex ()
  "Pin that pattern is a literal substring, not a regex.
A pattern containing regex metacharacters (`.', `*', `[', etc.)
must match only lines that contain the literal characters."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nno dots here\nfoo.bar on this line\n"))
   (let* ((result (org-mcp-test--call-grep "foo.bar" test-file))
          (groups (alist-get 'groups result)))
     ;; The regex `.` would match any char; the literal should match only foo.bar
     (should (= (length groups) 1))
     (should (= (length (alist-get 'matches (car groups))) 1))
     (should (string-match-p "foo\\.bar"
                             (alist-get 'text (car (alist-get 'matches (car groups)))))))))

(ert-deftest org-mcp-test-grep-one-match-per-line ()
  "Pin that org-grep reports one entry per source line even when
the pattern appears multiple times on the same line."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\ntoken token on one line\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (= (length (alist-get 'matches (car groups))) 1)))))

(ert-deftest org-mcp-test-grep-match-in-heading-title ()
  "Pin that a match on the heading title line itself is reported
in that heading's own group."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Contains the token here\nbody text\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (let ((match (car (alist-get 'matches (car groups)))))
       (should (= (alist-get 'line match) 1))
       (should (string-match-p "token" (alist-get 'text match)))))))

(ert-deftest org-mcp-test-grep-case-sensitive-string-false ()
  "Pin that case_sensitive:\"false\" (LLM-client typo) is treated as false.
Mirrors the same normalisation done for `replace_all'."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nHello World content\n"))
   (let* ((params `((pattern . "hello") (file . ,test-file)
                    (case_sensitive . "false")))
          (result-json (mcp-server-lib-ert-call-tool "org-grep" params))
          (result (json-parse-string result-json
                                     :object-type 'alist
                                     :array-type 'list))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1)))))

(ert-deftest org-mcp-test-grep-no-allowed-files-returns-empty ()
  "Pin that org-grep returns {\"groups\":[]} with no allowed files and no file."
  (let ((org-mcp-allowed-files nil))
    (org-mcp-test--with-enabled
      (let* ((params '((pattern . "anything")))
             (result-json (mcp-server-lib-ert-call-tool "org-grep" params))
             (result (json-parse-string result-json
                                        :object-type 'alist
                                        :array-type 'list)))
        (should (equal (alist-get 'groups result) nil))))))

(ert-deftest org-mcp-test-grep-case-sensitive-json-false ()
  "Pin that case_sensitive:false (JSON boolean) is treated as false.
`:json-false' is truthy in Elisp but must be normalised to nil so
the search is case-insensitive."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nHello World content\n"))
   (let* ((params `((pattern . "hello") (file . ,test-file)
                    (case_sensitive . :json-false)))
          (result-json (mcp-server-lib-ert-call-tool "org-grep" params))
          (result (json-parse-string result-json
                                     :object-type 'alist
                                     :array-type 'list))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1)))))

(ert-deftest org-mcp-test-grep-multi-file-no-file-param ()
  "Pin that org-grep searches all allowed files when file is omitted."
  (org-mcp-test--with-temp-org-files
   ((file-a "* A\ntoken in file A\n")
    (file-b "* B\ntoken in file B\n"))
   (let* ((result (org-mcp-test--call-grep "token"))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 2)))))

(ert-deftest org-mcp-test-grep-file-param-limits-search ()
  "Pin that org-grep only searches the given file when file is provided."
  (org-mcp-test--with-temp-org-files
   ((file-a "* A\ntoken in file A\n")
    (file-b "* B\ntoken in file B\n"))
   (let* ((result (org-mcp-test--call-grep "token" file-a))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (string= (alist-get 'file (car groups)) file-a)))))

(ert-deftest org-mcp-test-grep-missing-allowed-file-skipped ()
  "Pin that a configured-but-missing allowed file is skipped in multi-file
search rather than aborting; matches from present files are still returned.

Regression for the missing `file-exists-p' guard in the multi-file branch
of `org-mcp--tool-grep'."
  (org-mcp-test--with-temp-org-files
   ((present-file "* Section\ntoken found here\n"))
   (let* ((missing-file (make-temp-name
                         (concat temporary-file-directory
                                 "org-mcp-test-missing-")))
          (org-mcp-allowed-files (list present-file missing-file)))
     (let* ((result (org-mcp-test--call-grep "token"))
            (groups (alist-get 'groups result)))
       (should (= (length groups) 1))
       (should (string= (alist-get 'file (car groups)) present-file))))))

(ert-deftest org-mcp-test-grep-uri-prefers-org-id-when-available ()
  "Pin that org-grep returns org-id:// URI when section has an ID."
  (org-mcp-test--with-temp-org-files
   ((test-file (format "* Heading\n:PROPERTIES:\n:ID: %s\n:END:\nbody here\n"
                       org-mcp-test--content-with-id-id)))
   (let* ((result (org-mcp-test--call-grep "body" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (string= (alist-get 'uri (car groups))
                      org-mcp-test--content-with-id-uri)))))

(ert-deftest org-mcp-test-grep-uri-org-headline-when-no-id ()
  "Pin that org-grep returns a fully URL-encoded org-headline:// URI.
Spaces must appear as `%20' and `/' in titles as `%2F', so the URI
round-trips through `url-unhex-string' back to the original title."
  (org-mcp-test--with-temp-org-files
   ((test-file "* No ID Heading\nbody here\n"))
   (let* ((result (org-mcp-test--call-grep "body" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (string-prefix-p "org-headline://" (alist-get 'uri (car groups))))
     (should (string-match-p "No%20ID%20Heading"
                             (alist-get 'uri (car groups)))))))

(ert-deftest org-mcp-test-grep-uri-percent-in-title-round-trips ()
  "Pin that a title containing `%' is fully encoded so it round-trips.
`%20' in a title must become `%2520' in the URI (not stay as `%20'),
otherwise `url-unhex-string' in the read path would corrupt it."
  (org-mcp-test--with-temp-org-files
   ((test-file "* 50%20done\nbody here\n"))
   (let* ((result (org-mcp-test--call-grep "body" test-file))
          (groups (alist-get 'groups result))
          (uri (alist-get 'uri (car groups))))
     (should (= (length groups) 1))
     (should (string-prefix-p "org-headline://" uri))
     (should (string-match-p "50%2520done" uri)))))

(ert-deftest org-mcp-test-grep-case-insensitive-default ()
  "Pin that org-grep is case-insensitive by default."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nHello World content\n"))
   (let* ((result (org-mcp-test--call-grep "hello" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1)))))

(ert-deftest org-mcp-test-grep-case-sensitive-opt-in ()
  "Pin that case_sensitive:true rejects case mismatches."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nHello World content\n"))
   (let* ((result (org-mcp-test--call-grep "hello" test-file t))
          (groups (alist-get 'groups result)))
     (should (equal groups nil)))))

(ert-deftest org-mcp-test-grep-different-sections-separate-groups ()
  "Pin that matches in different sections produce separate groups."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section A\ntoken here\n* Section B\ntoken here\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 2))
     (should (equal (alist-get 'headline_path (car groups)) '("Section A")))
     (should (equal (alist-get 'headline_path (cadr groups))
                    '("Section B"))))))

(ert-deftest org-mcp-test-grep-multiple-matches-one-section ()
  "Pin that consecutive matches in the same section form one group."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nfirst match here\nsecond match here\n"))
   (let* ((result (org-mcp-test--call-grep "match" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (= (length (alist-get 'matches (car groups))) 2)))))

(ert-deftest org-mcp-test-grep-multi-match-line-numbers ()
  "Pin that each match in a group reports its own 1-based line number.
Regression: the prev-pos/count-lines accumulator advanced prev-pos
past the matched line, making every match after the first report the
first match's line number instead of its own."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Section\nfirst match here\nsecond match here\n"))
   (let* ((result (org-mcp-test--call-grep "match" test-file))
          (matches (alist-get 'matches (car (alist-get 'groups result)))))
     (should (= (alist-get 'line (nth 0 matches)) 2))
     (should (= (alist-get 'line (nth 1 matches)) 3)))))

(ert-deftest org-mcp-test-grep-gapped-match-line-numbers ()
  "Pin that line numbers are absolute even when matches are not adjacent.
Locks in that the fix uses true per-match line lookup, not a relative
delta that would drift differently for non-consecutive matches."
  (org-mcp-test--with-temp-org-files
   ((test-file
     "* Section\nfirst match here\nskip this\nskip this too\nfifth match\n"))
   (let* ((result (org-mcp-test--call-grep "match" test-file))
          (matches (alist-get 'matches (car (alist-get 'groups result)))))
     (should (= (alist-get 'line (nth 0 matches)) 2))
     (should (= (alist-get 'line (nth 1 matches)) 5)))))

(ert-deftest org-mcp-test-grep-nested-headline-path ()
  "Pin that org-grep returns the full ancestor path in headline_path.
A match in a nested section lists all ancestor titles in order."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Parent\n** Child\ndeep content here\n"))
   (let* ((result (org-mcp-test--call-grep "deep" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (equal (alist-get 'headline_path (car groups))
                    '("Parent" "Child"))))))

(ert-deftest org-mcp-test-grep-match-in-pre-heading-content ()
  "Pin that org-grep matches lines before the first heading.
Such matches appear in a group with headline_path [] and a
file-level URI (the trailing-slash org-headline:// form)."
  (org-mcp-test--with-temp-org-files
   ((test-file "This is pre-heading content\n* My Heading\n"))
   (let* ((result (org-mcp-test--call-grep "pre-heading" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (let ((group (car groups)))
       (should (equal (alist-get 'headline_path group) nil))
       (should (string-prefix-p "org-headline://" (alist-get 'uri group)))
       (should (string-suffix-p "/" (alist-get 'uri group)))
       (let ((matches (alist-get 'matches group)))
         (should (= (length matches) 1))
         (should (= (alist-get 'line (car matches)) 1)))))))

(ert-deftest org-mcp-test-grep-match-in-section-body ()
  "Pin that org-grep finds a match in a heading's body and returns
correct headline_path and match text."
  (org-mcp-test--with-temp-org-files
   ((test-file "* My Heading\nThis is some body text\n"))
   (let* ((result (org-mcp-test--call-grep "body" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (let ((group (car groups)))
       (should (equal (alist-get 'headline_path group) '("My Heading")))
       (let ((matches (alist-get 'matches group)))
         (should (= (length matches) 1))
         (should (= (alist-get 'line (car matches)) 2))
         (should (string= (alist-get 'text (car matches))
                          "This is some body text")))))))

(ert-deftest org-mcp-test-grep-no-matches ()
  "Pin that org-grep returns {\"groups\":[]} when pattern is absent."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Heading\nSome body text\n"))
   (let ((result (org-mcp-test--call-grep "XYZZY_NOT_IN_FILE" test-file)))
     (should (equal (alist-get 'groups result) nil)))))

(ert-deftest org-mcp-test-grep-file-not-in-allowed-list ()
  "Pin that a `file' not in `org-mcp-allowed-files' is rejected.
The file-access check fires before any search I/O, so the pattern
and the file contents are irrelevant."
  (org-mcp-test--assert-tool-error-message-regex
   "org-grep" '((pattern . "x") (file . "/not/allowed.org"))
   "not in allowed"))

(ert-deftest org-mcp-test-grep-non-string-file ()
  "Pin that non-string `file' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any file-access
or search logic runs."
  (org-mcp-test--assert-tool-error-message-regex
   "org-grep" '((pattern . "x") (file . 42))
   (org-mcp-test--field-non-string-regex "file" 42)))

(ert-deftest org-mcp-test-grep-non-string-pattern ()
  "Pin that non-string `pattern' is rejected at the tool boundary.
`org-mcp--validate-string-field' fires before any downstream
search or file-access logic runs."
  (org-mcp-test--assert-tool-error-message-regex
   "org-grep" '((pattern . 42))
   (org-mcp-test--field-non-string-regex "pattern" 42)))

(ert-deftest org-mcp-test-grep-empty-pattern ()
  "Pin that an empty `pattern' is rejected at the tool boundary.
A non-empty pattern is required; an empty string cannot meaningfully
narrow a search and would return every line in the file."
  (org-mcp-test--assert-tool-error-message-regex
   "org-grep" '((pattern . ""))
   "pattern.*non-empty"))

(ert-deftest org-mcp-test-grep-newline-in-pattern ()
  "Pin that a `pattern' containing a newline is rejected at the tool boundary.
A newline in pattern causes re-search-forward to match across two
physical lines, reporting only the end line and skipping occurrences
on the second matched line -- violating the one-match-per-source-line
contract.  Reject at the boundary to avoid silent wrong output."
  (org-mcp-test--assert-tool-error-message-regex
   "org-grep" '((pattern . "foo\nbar"))
   "single line"))

(ert-deftest org-mcp-test-grep-file-field-is-canonical-for-relative-arg ()
  "Pin that the file field is the canonical absolute path even when
a relative `file' arg resolves to an allowed file.
The validation accepts a relative path (truename comparison), but the
emitted `file' field and URI must use the canonical allowed-list form
so follow-up reads (which require absolute paths) succeed."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Heading\nbody token here\n"))
   (let* ((default-directory (file-name-directory test-file))
          (relative-file (file-name-nondirectory test-file))
          (result (org-mcp-test--call-grep "token" relative-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (string= (alist-get 'file (car groups)) test-file)))))

(ert-deftest org-mcp-test-grep-headline-path-raw-for-stats-cookie ()
  "Pin that headline_path contains the raw title including statistics cookies.
`org-get-outline-path' strips `[n/m]' and `[n%]' cookies; the grep
path must use the same raw title `org-mcp--navigate-to-headline' matches
on, so the emitted URI round-trips through `org-read-headline'."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Tasks [1/3]\nbody token here\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (equal (alist-get 'headline_path (car groups))
                    '("Tasks [1/3]"))))))

(ert-deftest org-mcp-test-grep-headline-path-raw-for-bracket-link ()
  "Pin that headline_path contains the raw bracket-link markup.
`org-get-outline-path' replaces `[[target][desc]]' with `desc';
the grep path must preserve raw markup so it matches
`org-mcp--navigate-to-headline' which uses `org-get-heading t t t t'."
  (org-mcp-test--with-temp-org-files
   ((test-file "* See [[https://example.com][docs]]\nbody token here\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result)))
     (should (= (length groups) 1))
     (should (equal (alist-get 'headline_path (car groups))
                    '("See [[https://example.com][docs]]"))))))

(ert-deftest org-mcp-test-grep-uri-encodes-raw-title-with-stats-cookie ()
  "Pin that the org-headline:// URI fragment encodes the raw cookie title.
The fragment must contain `Tasks%20%5B1%2F3%5D' (raw) not `Tasks'
(stripped), so it round-trips through `org-read-headline'."
  (org-mcp-test--with-temp-org-files
   ((test-file "* Tasks [1/3]\nbody token here\n"))
   (let* ((result (org-mcp-test--call-grep "token" test-file))
          (groups (alist-get 'groups result))
          (uri (alist-get 'uri (car groups))))
     (should (= (length groups) 1))
     (should (string-prefix-p "org-headline://" uri))
     (should (string-match-p "Tasks%20%5B1%2F3%5D" uri)))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
