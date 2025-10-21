;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

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

(defconst org-mcp-test--content-parent-child
  "* Parent Task
Some content here.
** Another Task
More content."
  "Parent task with a child task.")

(defconst org-mcp-test--content-nested-siblings
  "* Parent Task
:PROPERTIES:
:ID:       nested-siblings-parent-id-002
:END:
** First Child
First child content.
It spans multiple lines.
** Second Child
:PROPERTIES:
:ID:       nested-siblings-second-child-id-001
:END:
Second child content.
** Third Child
Third child content."
  "Parent with multiple child tasks.")

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

(defconst org-mcp-test--content-simple-todo
  "* TODO Original Task
First line of body.
Second line of body.
Third line of body."
  "Simple TODO task with three-line body.")

(defconst org-mcp-test--content-headline-no-todo
  "* Regular Headline
Some content."
  "Regular headline without TODO state.")

(defconst org-mcp-test--content-with-id-id
  "550e8400-e29b-41d4-a716-446655440000"
  "ID value for org-mcp-test--content-with-id.")

(defconst org-mcp-test--content-with-id
  (format
   "* Task with ID
:PROPERTIES:
:ID:       %s
:END:
First line of content.
Second line of content.
Third line of content."
   org-mcp-test--content-with-id-id)
  "Task with an Org ID property and multiline content.")

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

(defconst org-mcp-test--content-with-id-uri
  (format "org-id://%s" org-mcp-test--content-with-id-id)
  "URI for org-mcp-test--content-with-id.")

(defconst org-mcp-test--expected-regex-renamed-task-with-id
  (format
   (concat
    "\\`\\* Renamed Task with ID\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "First line of content\\.\n"
    "Second line of content\\.\n"
    "Third line of content\\.\\'")
   org-mcp-test--content-with-id-id)
  "Regex matching complete buffer after renaming Task with ID.")

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

(defconst org-mcp-test--content-slash-in-headline
  "* Project A/B Testing
This is a headline with a slash in it.
* Other Task
Some other content."
  "Headlines containing slash characters.")

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

(defconst org-mcp-test--content-nested-targets
  "* First Section
** Target
Some content.
* Second Section
** Other Item
Other content.
** Target
This Target is under Second Section, not First Section."
  "Multiple targets in different sections.")

(defconst org-mcp-test--content-nested-path-navigation
  "* Parent One
** Other Heading
Some other content.
* Parent Two
** Another Heading
More content.
* Parent Three
** Child
This Child is under Parent Three, not Parent Two."
  "Nested structure for testing path navigation correctness.")

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

(defconst org-mcp-test--expected-timestamp-id-done-regex
  (concat
   "\\`\\* DONE Task with timestamp ID"
   "\\(?:\n:PROPERTIES:\n:ID: +[A-Fa-f0-9-]+\n:END:\\)?"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer after updating timestamp ID task to DONE.")

(defconst org-mcp-test--expected-task-one-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task One"
   "\\(?:\n:PROPERTIES:\n:ID: +[A-Fa-f0-9-]+\n:END:\\)?"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task One in IN-PROGRESS state.")

(defconst org-mcp-test--expected-task-with-id-in-progress-regex
  (concat
   "\\`\\* IN-PROGRESS Task with ID"
   "\\(?:\n:PROPERTIES:\n:ID: +[A-Fa-f0-9-]+\n:END:\\)?"
   "\\(?:.\\|\n\\)*\\'")
  "Regex matching complete buffer with Task with ID in IN-PROGRESS state.")

(defconst org-mcp-test--expected-regex-top-level-with-header
  (concat
   "\\`#\\+TITLE: My Org Document\n"
   "#\\+AUTHOR: Test Author\n"
   "#\\+DATE: 2024-01-01\n"
   "#\\+OPTIONS: toc:nil\n"
   "\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?"
   "\n?"
   "\\* Existing Task\n"
   "Some content here\\.\\'")
  "Regex matching complete buffer after adding top-level TODO with headers.")

(defconst org-mcp-test--content-title-header-only
  "#+TITLE: Test Org File

"
  "Org file with only a title header.")

(defconst org-mcp-test--body-text-multiline
  (concat
   "This is the body text.\n"
   "It has multiple lines.\n"
   "With some content.")
  "Multi-line body text for testing TODO items with content.")

(defconst org-mcp-test--content-parent-child-siblings
  "* Parent Task
** Child One
** Child Two
* Other Parent
** Other Child"
  "Parent tasks with children for testing sibling relationships.")

(defconst org-mcp-test--other-child-id "A1B2C3D4-E5F6-7890-ABCD-EF1234567890"
  "ID value for Other Child in afterUri-not-sibling test.")

(defconst org-mcp-test--content-parent-child-siblings-with-other-child-id
  (format
   "* Parent Task
** Child One
** Child Two
* Other Parent
** Other Child
:PROPERTIES:
:ID:       %s
:END:
"
   org-mcp-test--other-child-id)
  "Parent-child-siblings content with ID for Other Child.")

(defconst org-mcp-test--content-parent-task-simple
  "* Parent Task
Some parent content.
* Another Task"
  "Simple parent task with sibling.")

(defconst org-mcp-test--content-headers-with-task
  "#+TITLE: My Org Document
#+AUTHOR: Test Author
#+DATE: 2024-01-01
#+OPTIONS: toc:nil

* Existing Task
Some content here."
  "Org file with headers and existing task.")

(defconst org-mcp-test--content-siblings-after-test
  "* Parent Task
** First Child
First child content.
** Second Child
Second child content.
** Third Child
Third child content."
  "Parent with children for testing after-sibling insertion.")

(defconst org-mcp-test--content-headlines-with-hash
  "* Task #1
First task
* Task #2
Second task"
  "Headlines containing # character in titles.")

(defconst org-mcp-test--expected-first-section-from-nested-targets
  "* First Section
** Target
Some content."
  "Expected content when extracting First Section from nested-targets.")


;; Regex patterns for validation
;;
;; Note on property drawer patterns: The patterns use ` *` (zero or more
;; spaces) before :PROPERTIES:, :ID:, and :END: lines to maintain compatibility
;; across Emacs versions. Emacs 27.2 indents property drawers with 3 spaces,
;; while Emacs 28+ does not add indentation.

(defconst org-mcp-test--regex-todo-after-headers
  (concat
   "^#\\+TITLE: My Org Document\n"
   "#\\+AUTHOR: Test Author\n"
   "#\\+DATE: 2024-01-01\n"
   "#\\+OPTIONS: toc:nil\n"
   "\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   "\\(?: *:PROPERTIES:\n"
   " *:ID: +[^\n]+\n"
   " *:END:\n\\)?"
   "\n?"
   "\\* Existing Task\n"
   "Some content here\\.")
  "Pattern for TODO added after org headers.")

(defconst org-mcp-test--regex-child-under-parent
  (concat
   "^\\* Parent Task\n"
   "Some content here\\.\n"
   "\\*\\* Another Task\n"
   "More content\\.\n"
   "\\*\\* TODO Child Task +.*:work:.*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?")
  "Pattern for child TODO (level 2) added under parent (level 1) with existing child (level 2).")

(defconst org-mcp-test--parent-with-one-child-parent-project-id
  "parent-project-test-id-001"
  "ID for Parent Project in parent-with-one-child.")

(defconst org-mcp-test--parent-with-one-child-first-child-id
  "first-child-test-id-002"
  "ID for First Child in parent-with-one-child.")

(defconst org-mcp-test--content-parent-with-one-child
  (format
   "* TODO Parent Project                                                     :work:
:PROPERTIES:
:ID:       %s
:END:
** TODO Parent Task                                                       :work:
*** TODO First Child                                                      :work:
:PROPERTIES:
:ID:       %s
:END:"
   org-mcp-test--parent-with-one-child-parent-project-id
   org-mcp-test--parent-with-one-child-first-child-id)
  "Parent at level 2 with one child at level 3.")

(defconst org-mcp-test--regex-second-child-same-level
  (concat
   "\\`\\* TODO Parent Project +.*:work:.*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"  ; Parent Project may have ID
   "\\*\\* TODO Parent Task +.*:work:.*\n"
   "\\*\\*\\* TODO First Child +.*:work:.*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"  ; First Child may have ID
   "\\*\\*\\* TODO Second Child +.*:work:.*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?\\'")  ; Second Child may have ID
  "Pattern for second child (level 3) added at same level as first child (level 3) under parent (level 2).")

(defconst org-mcp-test--regex-todo-with-body
  (concat
   "^\\* TODO Task with Body +:[^\n]*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?" ; Optional properties
   "This is the body text\\.\n"
   "It has multiple lines\\.\n"
   "With some content\\.\n?$")
  "Pattern for TODO with body text.")

(defconst org-mcp-test--regex-todo-after-sibling
  (concat
   "^\\* Parent Task\n"
   "\\*\\* First Child\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   "First child content\\.\n"
   "\\*\\* Second Child\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   "Second child content\\.\n\n?"
   "\\*\\* TODO New Task After Second +:[^\n]*\n"
   "\\(?: *:PROPERTIES:\n *:ID: +[^\n]+\n *:END:\n\\)?"
   "\\*\\* Third Child\n"
   "Third child content\\.")
  "Pattern for TODO added after specific sibling.")

(defconst org-mcp-test--regex-todo-without-tags
  (concat
   "^\\* TODO Task Without Tags *\n" ; No tags, optional spaces
   "\\(?: *:PROPERTIES:\n" " *:ID: +[^\n]+\n" " *:END:\n\\)?$")
  "Pattern for TODO item without any tags.")

(defconst org-mcp-test--pattern-renamed-simple-todo
  (concat
   "^\\* TODO Updated Task\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-F0-9-]+\n"
   " *:END:\n"
   "First line of body\\.$")
  "Pattern for renamed simple TODO with generated ID.")

(defconst org-mcp-test--content-todo-with-tags
  "* TODO Task with Tags :work:urgent:\nTask description."
  "TODO task with tags and body.")

(defconst org-mcp-test--pattern-renamed-todo-with-tags
  (concat
   "^\\* TODO Renamed Task[ \t]+:work:urgent:\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-F0-9-]+\n"
   " *:END:\n"
   "Task description\\.$")
  "Pattern for renamed TODO task preserving tags.")

(defconst org-mcp-test--pattern-renamed-headline-no-todo
  (concat
   "^\\* Updated Headline\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-F0-9-]+\n"
   " *:END:\n"
   "Some content\\.$")
  "Pattern for renamed headline without TODO state.")

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

;; Expected patterns for edit-body tests

(defconst org-mcp-test--pattern-edit-body-single-line
  (format (concat
           "\\`\\* Task with ID\n"
           ":PROPERTIES:\n"
           ":ID: +%s\n"
           ":END:\n"
           "Updated first line\\.\n"
           "Second line of content\\.\n"
           "Third line of content\\.\n"
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
  (concat
   "\\`\\* Parent Task\n"
   "Updated parent content\\.\n" ; No ID when using org-headline://
   "\\*\\* Another Task\n"
   "\\(?: *:PROPERTIES:\n" ; Child may get an ID
   " *:ID: +[A-F0-9-]+\n" " *:END:\n\\)?" "More content\\.\n" "?\\'")
  "Pattern for nested headlines edit-body test result.")


(defconst org-mcp-test--pattern-edit-body-empty
  (concat
   "\\* Another TaskNew content added\\.\n"
   " *:PROPERTIES:\n"
   " *:ID: +[A-F0-9-]+\n"
   " *:END:")
  "Pattern for edit-body test with empty body adding content.")

(defconst org-mcp-test--pattern-edit-body-empty-with-props
  (format (concat
           " *:PROPERTIES:\n"
           " *:ID: +[A-F0-9-]+\n"
           " *:END:\n"
           " *:PROPERTIES:\n"
           " *:ID: +%s\n"
           " *:END:Content added after properties\\.")
          org-mcp-test--timestamp-id)
  "Pattern for edit-body with existing properties adding content.")

(defconst org-mcp-test--pattern-edit-body-accept-lower-level
  (concat
   "\\* Task with ID\n"
   " *:PROPERTIES:\n"
   " *:ID: +"
   org-mcp-test--content-with-id-id
   "\n"
   " *:END:\n"
   "some text\n"
   "\\*\\* Subheading content\n"
   "\\(?: *:PROPERTIES:\n" ; Subheading gets ID
   " *:ID: +[A-F0-9-]+\n"
   " *:END:\n\\)?"
   "Second line of content\\.\n"
   "Third line of content\\.")
  "Pattern for edit-body accepting lower-level headlines.")

;; Expected patterns for tool tests

(defconst org-mcp-test--pattern-tool-read-headline-single
  (concat
   "\\`\\* Project A/B Testing\n"
   "This is a headline with a slash in it\\.\n"
   "?\\'")
  "Pattern for org-read-headline tool single-level path result.")

(defconst org-mcp-test--pattern-tool-read-headline-nested
  (concat
   "\\`\\*\\* First Child\n"
   "First child content\\.\n"
   "It spans multiple lines\\.\n"
   "?\\'")
  "Pattern for org-read-headline tool nested path result.")

(defconst org-mcp-test--pattern-tool-read-by-id
  (format
   (concat
    "\\`\\* Task with ID\n"
    ":PROPERTIES:\n"
    ":ID: +%s\n"
    ":END:\n"
    "First line of content\\.\n"
    "Second line of content\\.\n"
    "Third line of content\\.\n"
    "?\\'")
   org-mcp-test--content-with-id-id)
  "Pattern for org-read-by-id tool result.")


;; Helper functions for calling MCP tools

(defun org-mcp-test--call-get-todo-config ()
  "Call org-get-todo-config tool via JSON-RPC and return the result."
  (let ((result
         (mcp-server-lib-ert-call-tool "org-get-todo-config" nil)))
    ;; Parse the JSON string result
    (json-read-from-string result)))

(defun org-mcp-test--call-get-tag-config ()
  "Call org-get-tag-config tool via JSON-RPC and return the result."
  (let ((result
         (mcp-server-lib-ert-call-tool "org-get-tag-config" nil)))
    ;; Parse the JSON string result
    (json-read-from-string result)))

(defun org-mcp-test--call-get-allowed-files ()
  "Call org-get-allowed-files tool via JSON-RPC and return the result."
  (let ((result
         (mcp-server-lib-ert-call-tool "org-get-allowed-files" nil)))
    ;; Parse the JSON string result
    (json-read-from-string result)))

;; Test helper macros

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (mcp-server-lib-ert-with-server :tools t :resources t ,@body)
       (org-mcp-disable))))

(defmacro org-mcp-test--with-id-setup (id-locations &rest body)
  "Set up org-id tracking with ID-LOCATIONS and run BODY.
ID-LOCATIONS is a list of (ID . FILE) cons cells to register.
Sets up `org-id-track-globally' and `org-id-locations-file',
then registers each ID location and enables MCP for BODY."
  (declare (indent 1) (debug t))
  `(let ((org-id-track-globally t)
         (org-id-locations-file nil)) ; Prevent saving to disk
     (dolist (id-loc ,id-locations)
       (org-id-add-location (car id-loc) (cdr id-loc)))
     (org-mcp-test--with-enabled
       ,@body)))

(defmacro org-mcp-test--with-temp-org-file (var content &rest args)
  "Create a temporary Org file, execute BODY, and ensure cleanup.
VAR is the variable to bind the temp file path to.
CONTENT is the initial content to write to the file.
If first element of ARGS is a string, it's used as the filename prefix.
Remaining elements are the body."
  (declare (indent 2))
  (let* ((filename
          (or (when (stringp (car args))
                (pop args))
              "org-mcp-test"))
         (body args))
    `(let (,var)
       (unwind-protect
           (progn
             (setq ,var
                   (make-temp-file ,filename nil ".org" ,content))
             (let ((org-mcp-allowed-files (list ,var)))
               ,@body))
         (when ,var
           (delete-file ,var))))))

(defun org-mcp-test--check-sequence
    (seq expected-type expected-keywords)
  "Check sequence SEQ has EXPECTED-TYPE and EXPECTED-KEYWORDS."
  (should (= (length seq) 2))
  (should (equal (alist-get 'type seq) expected-type))
  (should (equal (alist-get 'keywords seq) expected-keywords)))

(defun org-mcp-test--check-semantic
    (sem expected-state expected-final expected-type)
  "Check semantic SEM properties.
EXPECTED-STATE is the TODO keyword.
EXPECTED-FINAL is whether it's a final state.
EXPECTED-TYPE is the sequence type."
  (should (= (length sem) 3))
  (should (equal (alist-get 'state sem) expected-state))
  (should (equal (alist-get 'isFinal sem) expected-final))
  (should (equal (alist-get 'sequenceType sem) expected-type)))

(defmacro org-mcp-test--with-config (keywords &rest body)
  "Run BODY with `org-todo-keywords' set to KEYWORDS."
  (declare (indent 1) (debug t))
  `(let ((org-todo-keywords ,keywords))
     (org-mcp-test--with-enabled
       (let ((result (org-mcp-test--call-get-todo-config)))
         (should (= (length result) 2))
         (let ((sequences (cdr (assoc 'sequences result)))
               (semantics (cdr (assoc 'semantics result))))
           ,@body)))))

(defun org-mcp-test--verify-file-content (file-path expected-regexp)
  "Check that FILE-PATH contain text matching EXPECTED-REGEXP."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (should (re-search-forward expected-regexp nil t))))

(defun org-mcp-test--test-headline-resource-with-extension
    (extension &optional file-path)
  "Test headline resource with file having EXTENSION.
EXTENSION can be a string like \".txt\" or nil for no extension.
FILE-PATH if provided uses this exact file path instead of creating temp file."
  (let ((test-file file-path))
    (unwind-protect
        (progn
          (unless test-file
            ;; Create temp file with extension
            (setq test-file
                  (make-temp-file "org-mcp-test" nil extension))
            (with-temp-file test-file
              (insert org-mcp-test--content-nested-targets)))
          (let ((org-mcp-allowed-files (list test-file))
                (uri
                 (format "org-headline://%s#First%%20Section"
                         test-file)))
            (org-mcp-test--with-enabled
              ;; Try to access headline in file
              (mcp-server-lib-ert-verify-resource-read
               uri
               `((uri . ,uri)
                 (text
                  .
                  ,org-mcp-test--expected-first-section-from-nested-targets)
                 (mimeType . "text/plain"))))))
      ;; Only cleanup if we created the file
      (unless file-path
        (delete-file test-file)))))

(defun org-mcp-test--call-add-todo
    (title todoState tags body parentUri &optional afterUri)
  "Call org-add-todo tool via JSON-RPC and return the result.
TITLE is the headline text.
TODOSTATE is the TODO state.
TAGS is a single tag string or list of tag strings.
BODY is optional body text.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after."
  (let* ((params
          `((title . ,title)
            (todo_state . ,todoState)
            (tags . ,tags)
            (body . ,body)
            (parent_uri . ,parentUri)
            (after_uri . ,afterUri)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-add-todo" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id)))
    (mcp-server-lib-ert-process-tool-response response)))

(defun org-mcp-test--call-update-todo-state-expecting-error
    (resource-uri current-state new-state)
  "Call org-update-todo-state tool via JSON-RPC expecting an error.
RESOURCE-URI is the URI to update.
CURRENT-STATE is the current TODO state.
NEW-STATE is the new TODO state to set."
  (let* ((request
          (mcp-server-lib-create-tools-call-request
           "org-update-todo-state" 1
           `((uri . ,resource-uri)
             (current_state . ,current-state)
             (new_state . ,new-state))))
         (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
         (result (mcp-server-lib-ert-process-tool-response response)))
    ;; If we get here, the tool succeeded when we expected failure
    (error "Expected error but got success: %s" result)))

(defun org-mcp-test--call-read-headline-expecting-error (file headline-path)
  "Call org-read-headline tool via JSON-RPC expecting an error.
FILE is the path to the Org file.
HEADLINE-PATH is the headline path string."
  (let* ((request
          (mcp-server-lib-create-tools-call-request
           "org-read-headline" 1
           `((file . ,file)
             (headline_path . ,headline-path))))
         (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
         (result (mcp-server-lib-ert-process-tool-response response)))
    ;; If we get here, the tool succeeded when we expected failure
    (error "Expected error but got success: %s" result)))

(defun org-mcp-test--call-rename-headline-expecting-error
    (resource-uri current-title new-title)
  "Call org-rename-headline tool via JSON-RPC expecting an error.
RESOURCE-URI is the URI to rename.
CURRENT-TITLE is the current title for validation.
NEW-TITLE is the new title to set."
  (let* ((params
          `((uri . ,resource-uri)
            (current_title . ,current-title)
            (new_title . ,new-title)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-rename-headline" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
         (result (mcp-server-lib-ert-process-tool-response response)))
    ;; If we get here, the tool succeeded when we expected failure
    (error "Expected error but got success: %s" result)))

(defun org-mcp-test--call-add-todo-expecting-error
    (title todoState tags body parentUri &optional afterUri)
  "Call org-add-todo tool via JSON-RPC expecting an error.
TITLE is the headline text.
TODOSTATE is the TODO state.
TAGS is a single tag string or list of tag strings.
BODY is optional body text.
PARENTURI is the URI of the parent item.
AFTERURI is optional URI of sibling to insert after."
  (let* ((params
          `((title . ,title)
            (todo_state . ,todoState)
            (tags . ,tags)
            (body . ,body)
            (parent_uri . ,parentUri)
            (after_uri . ,afterUri)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-add-todo" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
         (result (mcp-server-lib-ert-process-tool-response response)))
    ;; If we get here, the tool succeeded when we expected failure
    (error "Expected error but got success: %s" result)))

(defun org-mcp-test--update-and-verify-todo
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
          (mcp-server-lib-ert-call-tool
           "org-update-todo-state" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 4))
    (should (equal (alist-get 'success result) t))
    (should (equal (alist-get 'previous_state result) old-state))
    (should (equal (alist-get 'new_state result) new-state))
    (should (stringp (alist-get 'uri result)))
    (should (string-prefix-p "org-id://" (alist-get 'uri result)))
    (org-mcp-test--verify-file-matches test-file expected-content-regex)
    result))

(defun org-mcp-test--check-add-todo-result
    (result
     expected-title basename test-file &optional expected-pattern)
  "Check that add-todo RESULT has the correct structure and file content.
RESULT is the return value from `org-add-todo' tool.
EXPECTED-TITLE is the title that should be in the result.
BASENAME is the expected file basename.
TEST-FILE is the path to the file to check.
EXPECTED-PATTERN if provided, is a regexp that the file content should match."
  ;; Check result structure
  (should (= (length result) 4))
  (should (equal (alist-get 'success result) t))
  (should (string-prefix-p "org-id://" (alist-get 'uri result)))
  (should (equal (alist-get 'file result) basename))
  (should (equal (alist-get 'title result) expected-title))
  (when expected-pattern
    (org-mcp-test--verify-file-matches test-file expected-pattern)))

(defun org-mcp-test--assert-error-and-file (test-file error-form)
  "Assert that ERROR-FORM throws an error and TEST-FILE remains unchanged.
ERROR-FORM should be a form that is expected to signal an error.
The file content is saved before the error test and verified to be
unchanged after."
  (let ((original-content
         (with-temp-buffer
           (insert-file-contents test-file)
           (buffer-string))))
    (should-error (eval error-form) :type 'mcp-server-lib-tool-error)
    (org-mcp-test--verify-file-eq test-file original-content)))

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

(ert-deftest org-mcp-test-tool-get-todo-config-empty ()
  "Test org-get-todo-config with empty `org-todo-keywords'."
  (org-mcp-test--with-config nil
    (should (assoc 'sequences result))
    (should (assoc 'semantics result))
    (should (equal sequences []))
    (should (equal semantics []))))

(ert-deftest org-mcp-test-tool-get-todo-config-default ()
  "Test org-get-todo-config with default `org-todo-keywords'."
  (org-mcp-test--with-config '((sequence "TODO(t!)" "DONE(d!)"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0) "sequence" ["TODO(t!)" "|" "DONE(d!)"])
    (should (= (length semantics) 2))
    (org-mcp-test--check-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-semantic
     (aref semantics 1) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-single-keyword ()
  "Test org-get-todo-config with single keyword."
  (org-mcp-test--with-config '((sequence "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0) "sequence" ["|" "DONE"])
    (should (= (length semantics) 1))
    (org-mcp-test--check-semantic
     (aref semantics 0) "DONE" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-explicit-bar ()
  "Test org-get-todo-config with explicit | and multiple states."
  (org-mcp-test--with-config '((sequence
                                "TODO" "NEXT" "|" "DONE" "CANCELLED"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0)
     "sequence"
     ["TODO" "NEXT" "|" "DONE" "CANCELLED"])
    (should (= (length semantics) 4))
    (org-mcp-test--check-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-semantic
     (aref semantics 1) "NEXT" nil "sequence")
    (org-mcp-test--check-semantic
     (aref semantics 2) "DONE" t "sequence")
    (org-mcp-test--check-semantic
     (aref semantics 3) "CANCELLED" t "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type ()
  "Test org-get-todo-config with type keywords."
  (org-mcp-test--with-config '((type "Fred" "Sara" "Lucy" "|" "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0) "type" ["Fred" "Sara" "Lucy" "|" "DONE"])
    (should (= (length semantics) 4))
    (org-mcp-test--check-semantic
     (aref semantics 0) "Fred" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 1) "Sara" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 2) "Lucy" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 3) "DONE" t "type")))

(ert-deftest org-mcp-test-tool-get-todo-config-multiple-sequences ()
  "Test org-get-todo-config with multiple sequences."
  (org-mcp-test--with-config '((sequence "TODO" "|" "DONE")
                               (type "BUG" "FEATURE" "|" "FIXED"))
    (should (= (length sequences) 2))
    ;; First sequence
    (org-mcp-test--check-sequence
     (aref sequences 0) "sequence" ["TODO" "|" "DONE"])
    ;; Second sequence
    (org-mcp-test--check-sequence
     (aref sequences 1) "type" ["BUG" "FEATURE" "|" "FIXED"])
    (should (= (length semantics) 5))
    ;; Semantics from first sequence
    (org-mcp-test--check-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-semantic
     (aref semantics 1) "DONE" t "sequence")
    ;; Semantics from second sequence
    (org-mcp-test--check-semantic (aref semantics 2) "BUG" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 3) "FEATURE" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 4) "FIXED" t "type")))

(ert-deftest org-mcp-test-tool-get-todo-config-no-done-states ()
  "Test org-get-todo-config with no done states."
  (org-mcp-test--with-config '((sequence "TODO" "NEXT" "|"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0) "sequence" ["TODO" "NEXT" "|"])
    (should (= (length semantics) 2))
    (org-mcp-test--check-semantic
     (aref semantics 0) "TODO" nil "sequence")
    (org-mcp-test--check-semantic
     (aref semantics 1) "NEXT" nil "sequence")))

(ert-deftest org-mcp-test-tool-get-todo-config-type-no-separator ()
  "Test org-get-todo-config with type keywords and no separator."
  (org-mcp-test--with-config '((type "BUG" "FEATURE" "ENHANCEMENT"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0) "type" ["BUG" "FEATURE" "|" "ENHANCEMENT"])
    (should (= (length semantics) 3))
    (org-mcp-test--check-semantic (aref semantics 0) "BUG" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 1) "FEATURE" nil "type")
    (org-mcp-test--check-semantic
     (aref semantics 2) "ENHANCEMENT" t "type")))

(ert-deftest org-mcp-test-tool-get-tag-config-empty ()
  "Test org-get-tag-config with empty `org-tag-alist'."
  (let ((org-tag-alist nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should (= (length result) 4))
        (should
         (equal (alist-get 'org-use-tag-inheritance result) "t"))
        (should
         (equal
          (alist-get
           'org-tags-exclude-from-inheritance result)
          "nil"))
        (should (equal (alist-get 'org-tag-alist result) "nil"))
        (should
         (equal
          (alist-get 'org-tag-persistent-alist result) "nil"))))))

(ert-deftest org-mcp-test-tool-get-tag-config-simple ()
  "Test org-get-tag-config with simple tags."
  (let ((org-tag-alist '("work" "personal" "urgent"))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal
          (alist-get 'org-tag-alist result)
          "(\"work\" \"personal\" \"urgent\")"))
        (should
         (equal (alist-get 'org-tag-persistent-alist result) "nil"))
        (should
         (equal (alist-get 'org-use-tag-inheritance result) "t"))))))

(ert-deftest org-mcp-test-tool-get-tag-config-with-keys ()
  "Test org-get-tag-config with fast selection keys."
  (let ((org-tag-alist
         '(("work" . ?w) ("personal" . ?p) "urgent" ("@home" . ?h)))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal
          (alist-get 'org-tag-alist result)
          (concat
           "((\"work\" . 119) (\"personal\" . 112) "
           "\"urgent\" (\"@home\" . 104))")))))))

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
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        ;; Just check that the literal string is returned correctly
        (should (stringp (alist-get 'org-tag-alist result)))
        (should
         (string-match-p
          ":startgroup" (alist-get 'org-tag-alist result)))
        (should
         (string-match-p
          ":endgroup" (alist-get 'org-tag-alist result)))
        (should
         (string-match-p
          ":startgrouptag" (alist-get 'org-tag-alist result)))
        (should
         (string-match-p
          ":endgrouptag" (alist-get 'org-tag-alist result)))))))

(ert-deftest org-mcp-test-tool-get-tag-config-persistent ()
  "Test org-get-tag-config with persistent tags."
  (let ((org-tag-alist '(("work" . ?w)))
        (org-tag-persistent-alist '(("important" . ?i) "recurring"))
        (org-tags-exclude-from-inheritance nil))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal
          (alist-get 'org-tag-alist result) "((\"work\" . 119))"))
        (should
         (equal
          (alist-get 'org-tag-persistent-alist result)
          "((\"important\" . 105) \"recurring\")"))))))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-enabled ()
  "Test org-get-tag-config with inheritance enabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal (alist-get 'org-use-tag-inheritance result) "t"))))))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-disabled ()
  "Test org-get-tag-config with inheritance disabled."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance nil))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal
          (alist-get 'org-use-tag-inheritance result) "nil"))))))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance-selective ()
  "Test org-get-tag-config with selective inheritance (list)."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance '("work")))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal
          (alist-get 'org-use-tag-inheritance result)
          "(\"work\")"))))))

(ert-deftest org-mcp-test-tool-get-allowed-files-empty ()
  "Test org-get-allowed-files with empty configuration."
  (let ((org-mcp-allowed-files nil))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-allowed-files)))
        (should (= (length result) 1))
        (let ((files (cdr (assoc 'files result))))
          (should (vectorp files))
          (should (= (length files) 0)))))))

(ert-deftest org-mcp-test-tool-get-allowed-files-single ()
  "Test org-get-allowed-files with single file."
  (let ((org-mcp-allowed-files '("/home/user/tasks.org")))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-allowed-files)))
        (should (= (length result) 1))
        (let ((files (cdr (assoc 'files result))))
          (should (vectorp files))
          (should (= (length files) 1))
          (should (string= (aref files 0) "/home/user/tasks.org")))))))

(ert-deftest org-mcp-test-tool-get-allowed-files-multiple ()
  "Test org-get-allowed-files with multiple files."
  (let ((org-mcp-allowed-files
         '("/home/user/tasks.org"
           "/home/user/projects.org"
           "/home/user/notes.org")))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-allowed-files)))
        (should (= (length result) 1))
        (let ((files (cdr (assoc 'files result))))
          (should (vectorp files))
          (should (= (length files) 3))
          (should (string= (aref files 0) "/home/user/tasks.org"))
          (should (string= (aref files 1) "/home/user/projects.org"))
          (should (string= (aref files 2) "/home/user/notes.org")))))))

(defmacro org-mcp-test--with-add-todo-setup
    (file-var initial-content &rest body)
  "Helper for org-add-todo test.
Sets up FILE-VAR with INITIAL-CONTENT and standard org configuration.
Executes BODY with org-mcp enabled and standard variables set."
  (declare (indent 2))
  `(org-mcp-test--with-temp-org-file ,file-var ,initial-content
     (let ((org-mcp-allowed-files (list ,file-var))
           (org-todo-keywords
            '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
           (org-tag-alist '("work" "personal" "urgent"))
           (org-id-locations-file nil))
       (org-mcp-test--with-enabled
         ,@body))))

(defun org-mcp-test--assert-add-todo-rejects-body-headline
    (initial-content parent-headline body-with-headline)
  "Test that adding TODO with BODY-WITH-HEADLINE is rejected.
INITIAL-CONTENT is the initial file content.
PARENT-HEADLINE is the parent headline path (empty string for top-level).
BODY-WITH-HEADLINE is the body containing invalid headline."
  (org-mcp-test--with-add-todo-setup test-file initial-content
    (let ((parent-uri
           (format "org-headline://%s#%s" test-file parent-headline)))
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         "Test Task" "TODO" '("work") ,body-with-headline ,parent-uri)))))

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

(defun org-mcp-test--assert-add-todo-invalid-title (invalid-title)
  "Assert that adding TODO with INVALID-TITLE throws an error.
Tests that the given title is rejected when creating a TODO."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "org-headline://%s#" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         ,invalid-title "TODO" nil nil ,parent-uri)))))

(defun org-mcp-test--assert-rename-headline-rejected
    (initial-content headline-title new-title)
  "Assert renaming headline to NEW-TITLE is rejected.
INITIAL-CONTENT is the Org content to test with.
HEADLINE-TITLE is the current headline to rename.
NEW-TITLE is the invalid new title that should be rejected."
  (org-mcp-test--with-temp-org-file test-file initial-content
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        ;; Try to rename - should fail
        (let* ((resource-uri
                (format "org-headline://%s#%s"
                        test-file
                        (url-hexify-string headline-title)))
               (request
                (mcp-server-lib-create-tools-call-request
                 "org-rename-headline" 1
                 `((uri . ,resource-uri)
                   (current_title . ,headline-title)
                   (new_title . ,new-title))))
               (response
                (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id)))
          (should-error
           (mcp-server-lib-ert-process-tool-response response)
           :type 'mcp-server-lib-tool-error))
        ;; Verify that file wasn't changed
        (with-temp-buffer
          (insert-file-contents test-file)
          (should (string= (buffer-string) initial-content)))))))

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
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          (let ((uri (format "org://%s" test-file)))
            (mcp-server-lib-ert-verify-resource-read
             uri
             `((uri . ,uri)
               (text . ,test-content)
               (mimeType . "text/plain")))))))))

(ert-deftest org-mcp-test-outline-resource-returns-structure ()
  "Test that outline resource returns document structure."
  (let ((test-content
         "* First Section
Some content here.
** Subsection 1.1
More content.
** Subsection 1.2
Even more content.
* Second Section
Content of second section.
*** Deep subsection
Very deep content."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
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
            ;; Check if we have an error instead of result
            (when (alist-get 'error response)
              (error
               "Resource request failed: %s"
               (alist-get 'message (alist-get 'error response))))
            (let* ((outline-json (alist-get 'text (aref contents 0)))
                   (outline
                    (json-parse-string outline-json
                                       :object-type 'alist))
                   (headings (alist-get 'headings outline)))
              ;; Check we have the right number of top-level headings
              (should (= (length headings) 2))
              ;; Check first heading
              (let ((first (aref headings 0)))
                (should
                 (equal (alist-get 'title first) "First Section"))
                (should (= (alist-get 'level first) 1))
                ;; Check children of first heading
                (let ((children (alist-get 'children first)))
                  (should (= (length children) 2))
                  (should
                   (equal
                    (alist-get 'title (aref children 0))
                    "Subsection 1.1"))
                  (should
                   (equal
                    (alist-get 'title (aref children 1))
                    "Subsection 1.2"))))
              ;; Check second heading
              (let ((second (aref headings 1)))
                (should
                 (equal (alist-get 'title second) "Second Section"))
                (should (= (alist-get 'level second) 1))
                ;; Deep subsection is empty (level 3 under level 1)
                (should
                 (= (length (alist-get 'children second)) 0))))))))))

(ert-deftest org-mcp-test-file-not-in-allowed-list-returns-error ()
  "Test that reading a file not in allowed list returns an error."
  (org-mcp-test--with-temp-org-file allowed-file "Allowed content"
    (org-mcp-test--with-temp-org-file forbidden-file
        "Forbidden content"
      (let ((org-mcp-allowed-files (list allowed-file)))
        (org-mcp-test--with-enabled
          ;; Try to read the forbidden file
          (let ((uri (format "org://%s" forbidden-file)))
            (org-mcp-test--read-resource-expecting-error
             uri
             (format "'%s': the referenced file not in allowed list" forbidden-file))))))))

(ert-deftest org-mcp-test-headline-resource-returns-content ()
  "Test that headline resource returns specific headline content."
  (let ((test-content
         "* First Section
Some content in first section.
** Subsection 1.1
Content of subsection 1.1.
** Subsection 1.2
Content of subsection 1.2.
* Second Section
Content in second section.
** Subsection 2.1
Content of subsection 2.1."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; Test getting a top-level headline
          (let ((uri
                 (format "org-headline://%s#First%%20Section"
                         test-file)))
            (mcp-server-lib-ert-verify-resource-read
             uri
             `((uri . ,uri)
               (text
                .
                ,(concat
                  "* First Section\n"
                  "Some content in first section.\n"
                  "** Subsection 1.1\n"
                  "Content of subsection 1.1.\n"
                  "** Subsection 1.2\n"
                  "Content of subsection 1.2."))
               (mimeType . "text/plain"))))
          ;; Test getting a nested headline
          (let ((uri
                 (format (concat
                          "org-headline://%s#"
                          "First%%20Section/Subsection%%201.1")
                         test-file)))
            (mcp-server-lib-ert-verify-resource-read
             uri
             `((uri . ,uri)
               (text
                .
                ,(concat
                  "** Subsection 1.1\n" "Content of subsection 1.1."))
               (mimeType . "text/plain")))))))))

(ert-deftest org-mcp-test-headline-resource-not-found ()
  "Test headline resource error for non-existent headline."
  (let ((test-content "* Existing Section\nSome content."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          (let ((uri
                 (format "org-headline://%s#Nonexistent" test-file)))
            (org-mcp-test--read-resource-expecting-error
             uri "Cannot find headline: 'Nonexistent'")))))))

(ert-deftest org-mcp-test-headline-resource-file-with-hash ()
  "Test headline resource with # in filename."
  (org-mcp-test--with-temp-org-file file
      org-mcp-test--content-headline-no-todo
    "org-mcp-test-file#"
    (org-mcp-test--with-enabled
      ;; Test accessing the file with # encoded as %23
      (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
             (uri
              (format "org-headline://%s#Regular%%20Headline"
                      encoded-path)))
        (mcp-server-lib-ert-verify-resource-read
         uri
         `((uri . ,uri)
           (text . "* Regular Headline\nSome content.")
           (mimeType . "text/plain")))))))

(ert-deftest org-mcp-test-headline-resource-headline-with-hash ()
  "Test headline resource with # in headline title."
  (let ((test-content org-mcp-test--content-headlines-with-hash))
    (org-mcp-test--with-temp-org-file file test-content
      (let ((org-mcp-allowed-files (list file)))
        (org-mcp-test--with-enabled
          ;; Test accessing headline with # encoded as %23
          (let ((uri (format "org-headline://%s#Task%%20%%231" file)))
            (mcp-server-lib-ert-verify-resource-read
             uri
             `((uri . ,uri)
               (text . "* Task #1\nFirst task")
               (mimeType . "text/plain")))))))))

(ert-deftest
    org-mcp-test-headline-resource-file-and-headline-with-hash
    ()
  "Test headline resource with # in both filename and headline."
  (org-mcp-test--with-temp-org-file file
      org-mcp-test--content-headlines-with-hash
    "org-mcp-test-file#"
    (org-mcp-test--with-enabled
      ;; Test with both file and headline containing #
      (let* ((encoded-path (replace-regexp-in-string "#" "%23" file))
             (uri
              (format "org-headline://%s#Task%%20%%231"
                      encoded-path)))
        (mcp-server-lib-ert-verify-resource-read
         uri
         `((uri . ,uri)
           (text . "* Task #1\nFirst task")
           (mimeType . "text/plain")))))))

(ert-deftest org-mcp-test-headline-resource-txt-extension ()
  "Test that headline resource works with .txt files, not just .org files."
  (org-mcp-test--test-headline-resource-with-extension ".txt"))

(ert-deftest org-mcp-test-headline-resource-no-extension ()
  "Test that headline resource works with files having no extension."
  (org-mcp-test--test-headline-resource-with-extension nil))

(ert-deftest org-mcp-test-headline-resource-path-traversal ()
  "Test that path traversal with ../ in org-headline URIs is rejected."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-parent-task-simple
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        ;; Test with ../ in the filename part
        (let ((uri
               (format "org-headline://../%s#Parent%%20Task"
                       (file-name-nondirectory test-file))))
          (org-mcp-test--read-resource-expecting-error
           uri
           (format "Path must be absolute: ../%s"
                   (file-name-nondirectory test-file))))))))

(ert-deftest org-mcp-test-headline-resource-encoded-path-traversal ()
  "Test that URL-encoded path traversal in org-headline URIs is rejected."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-parent-task-simple
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        ;; Test with URL-encoded ../ (%2E%2E%2F) in the filename part
        ;; The encoding is NOT decoded, so %2E%2E%2F remains literal
        (let ((uri
               (format "org-headline://%%2E%%2E%%2F%s#Parent%%20Task"
                       (file-name-nondirectory test-file))))
          (org-mcp-test--read-resource-expecting-error
           uri
           (format "Path must be absolute: %%2E%%2E%%2F%s"
                   (file-name-nondirectory test-file))))))))

(ert-deftest org-mcp-test-headline-resource-navigation ()
  "Test that headline navigation respects structure.
This test verifies that `org-mcp--get-headline-content'
properly checks parent-child relationships and levels."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-wrong-levels
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
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
           "Cannot find headline: 'First Parent/Target Headline'"))))))


(ert-deftest org-mcp-test-id-resource-returns-content ()
  "Test that ID resource returns content for valid ID."
  (let ((test-content
         (concat
          "* Section with ID\n"
          ":PROPERTIES:\n"
          ":ID: 12345678-abcd-efgh-ijkl-1234567890ab\n"
          ":END:\n"
          "Content of section with ID.")))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-id-setup
            `(("12345678-abcd-efgh-ijkl-1234567890ab" . ,test-file))
          (let ((uri "org-id://12345678-abcd-efgh-ijkl-1234567890ab"))
            (mcp-server-lib-ert-verify-resource-read
             uri
             `((uri . ,uri)
               (text . ,test-content)
               (mimeType . "text/plain")))))))))

(ert-deftest org-mcp-test-id-resource-not-found ()
  "Test ID resource error for non-existent ID."
  (let ((test-content "* Section without ID\nNo ID here."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-locations-file nil)
            (org-id-locations nil)) ; Make sure no IDs are registered
        (org-mcp-test--with-enabled
          (let ((uri "org-id://nonexistent-id-12345"))
            (org-mcp-test--read-resource-expecting-error
             uri "Cannot find ID: 'nonexistent-id-12345'")))))))

(ert-deftest org-mcp-test-id-resource-file-not-allowed ()
  "Test ID resource validates file is in allowed list."
  ;; Create two files - one allowed, one not
  (org-mcp-test--with-temp-org-file allowed-file "* Allowed\n"
    (org-mcp-test--with-temp-org-file other-file
        (concat
         "* Section with ID\n"
         ":PROPERTIES:\n"
         ":ID: test-id-789\n"
         ":END:\n"
         "This file is not in allowed list.")
      (let ((org-mcp-allowed-files (list allowed-file)))
        (org-mcp-test--with-id-setup `(("test-id-789" . ,other-file))
          (let ((uri "org-id://test-id-789"))
            ;; Should get an error for file not allowed
            (org-mcp-test--read-resource-expecting-error
             uri
             (format "'%s': the referenced file not in allowed list" "test-id-789"))))))))

(ert-deftest org-mcp-test-update-todo-state-success ()
  "Test successful TODO state update."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO(t!)" "IN-PROGRESS(i!)" "|" "DONE(d!)"))))
        (org-mcp-test--with-enabled
          ;; Update TODO to IN-PROGRESS
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20One" test-file)))
            (org-mcp-test--update-and-verify-todo
             resource-uri "TODO" "IN-PROGRESS"
             test-file org-mcp-test--expected-task-one-in-progress-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-mismatch ()
  "Test TODO state update fails on state mismatch."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Try to update with wrong current state
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20One" test-file)))
            (org-mcp-test--assert-error-and-file
             test-file
             `(org-mcp-test--call-update-todo-state-expecting-error
               ,resource-uri "IN-PROGRESS" "DONE"))))))))

(ert-deftest org-mcp-test-update-todo-with-timestamp-id ()
  "Test updating TODO state using timestamp-format ID (not UUID)."
  (let ((test-content org-mcp-test--content-timestamp-id))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE"))))
        (org-mcp-test--with-id-setup `(("20240101T120000"
                                        .
                                        ,test-file))
          (let ((uri "org-id://20240101T120000"))
            (org-mcp-test--update-and-verify-todo
             uri "TODO" "DONE"
             test-file
             org-mcp-test--expected-timestamp-id-done-regex)))))))

(ert-deftest org-mcp-test-update-todo-state-empty-newstate-invalid ()
  "Test that empty string for newState is rejected."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Try to set empty state
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20One" test-file)))
            (org-mcp-test--assert-error-and-file
             test-file
             `(org-mcp-test--call-update-todo-state-expecting-error
               ,resource-uri "TODO" ""))))))))

(ert-deftest org-mcp-test-update-todo-state-invalid ()
  "Test TODO state update fails for invalid new state."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Try to update to invalid state
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20One" test-file)))
            (org-mcp-test--assert-error-and-file
             test-file
             `(org-mcp-test--call-update-todo-state-expecting-error
               ,resource-uri "TODO" "INVALID-STATE"))))))))

(ert-deftest org-mcp-test-update-todo-state-with-open-buffer ()
  "Test TODO state update works when file is open in another buffer."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Open the file in a buffer
        (let ((buffer (find-file-noselect test-file)))
          (unwind-protect
              (org-mcp-test--with-enabled
                ;; Update TODO state while buffer is open
                (let ((resource-uri
                       (format "org-headline://%s#Task%%20One"
                               test-file)))
                  (org-mcp-test--update-and-verify-todo
                   resource-uri "TODO" "IN-PROGRESS"
                   test-file org-mcp-test--expected-task-one-in-progress-regex)
                  ;; Verify the buffer was also updated
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (should
                     (re-search-forward "^\\* IN-PROGRESS Task One"
                                        nil t)))))
            ;; Clean up: kill the buffer
            (kill-buffer buffer)))))))

(ert-deftest org-mcp-test-update-todo-state-with-modified-buffer ()
  "Test TODO state update fails when buffer has unsaved changes."
  (let ((test-content
         "* TODO Task One
Task description.
* TODO Task Two
Another task description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        ;; Open the file in a buffer and modify it elsewhere
        (let ((buffer (find-file-noselect test-file)))
          (unwind-protect
              (progn
                ;; Make a modification at an unrelated location
                (with-current-buffer buffer
                  (goto-char (point-max))
                  (insert "\n* TODO Task Three\nAdded in buffer.")
                  ;; Buffer is now modified but not saved
                  (should (buffer-modified-p)))

                (org-mcp-test--with-enabled
                  ;; Try to update while buffer has unsaved changes
                  (let ((resource-uri
                         (format "org-headline://%s#Task%%20One"
                                 test-file)))
                    (org-mcp-test--assert-error-and-file
                     test-file
                     `(org-mcp-test--call-update-todo-state-expecting-error
                       ,resource-uri "TODO" "IN-PROGRESS"))
                    ;; Verify buffer still has unsaved changes
                    (with-current-buffer buffer
                      (should (buffer-modified-p))
                      ;; Task One should still be TODO
                      (goto-char (point-min))
                      (should
                       (re-search-forward "^\\* TODO Task One" nil t))
                      ;; Task Three should still be there
                      (goto-char (point-min))
                      (should
                       (re-search-forward "^\\* TODO Task Three"
                                          nil t))))))
            ;; Clean up: kill the buffer
            (kill-buffer buffer)))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-id ()
  "Test TODO state update fails for non-existent UUID."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
            (org-id-locations-file nil)
            (org-id-locations nil)) ; Make sure no IDs are registered
        (org-mcp-test--with-enabled
          ;; Try to update a non-existent ID
          (let ((resource-uri "org-id://nonexistent-uuid-12345"))
            (should-error
             (org-mcp-test--call-update-todo-state-expecting-error
              resource-uri "TODO" "IN-PROGRESS")
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was NOT modified
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task One")))))))

(ert-deftest org-mcp-test-update-todo-state-by-id ()
  "Test updating TODO state using org-id:// URI."
  (let ((test-content org-mcp-test--content-with-id-todo))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-id-setup
            `((,org-mcp-test--content-with-id-id . ,test-file))
          (let ((result
                 (org-mcp-test--update-and-verify-todo
                  org-mcp-test--content-with-id-uri "TODO" "IN-PROGRESS"
                  test-file
                  org-mcp-test--expected-task-with-id-in-progress-regex)))
            (should
             (equal
              (alist-get 'uri result)
              org-mcp-test--content-with-id-uri))
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                (should
                 (string-match
                  org-mcp-test--expected-regex-todo-to-in-progress-with-id
                  content))))))))))

(ert-deftest org-mcp-test-update-todo-state-nonexistent-headline ()
  "Test TODO state update fails for non-existent headline path."
  (let ((test-content
         "* TODO Task One
Task description.
* TODO Task Two
Another task."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Try to update a non-existent headline
          (let ((resource-uri
                 (format "org-headline://%s#Nonexistent%%20Task"
                         test-file)))
            (org-mcp-test--assert-error-and-file
             test-file
             `(org-mcp-test--call-update-todo-state-expecting-error
               ,resource-uri "TODO" "IN-PROGRESS"))))))))

(ert-deftest org-mcp-test-add-todo-top-level ()
  "Test adding a top-level TODO item."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (result
            (org-mcp-test--call-add-todo
             "New Task"
             "TODO"
             '("work" "urgent")
             nil ; no body
             parent-uri
             nil))) ; no afterUri
      (org-mcp-test--check-add-todo-result
       result "New Task" (file-name-nondirectory test-file) test-file
       (concat
        "^\\* TODO New Task +:.*work.*urgent.*:\n"
        "\\(?: *:PROPERTIES:\n"
        " *:ID: +[^\n]+\n"
        " *:END:\n\\)?$")))))

(ert-deftest org-mcp-test-add-todo-top-level-with-header ()
  "Test adding top-level TODO after header comments."
  (let ((initial-content org-mcp-test--content-headers-with-task))
    (org-mcp-test--with-add-todo-setup test-file initial-content
      (let* ((parent-uri (format "org-headline://%s#" test-file))
             (result
              (org-mcp-test--call-add-todo
               "New Top Task"
               "TODO"
               '("urgent")
               nil ; no body
               parent-uri
               nil))) ; no afterUri
        (org-mcp-test--check-add-todo-result
         result
         "New Top Task"
         (file-name-nondirectory test-file)
         test-file)
        ;; Verify complete buffer content
        (with-temp-buffer
          (insert-file-contents test-file)
          (should
           (string-match-p
            org-mcp-test--expected-regex-top-level-with-header
            (buffer-string))))))))

(ert-deftest org-mcp-test-add-todo-invalid-state ()
  "Test that adding TODO with invalid state throws error."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "org-headline://%s#" test-file)))
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         "New Task"
         "INVALID-STATE" ; Not in org-todo-keywords
         '("work")
         nil
         ,parent-uri)))))

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
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "org-headline://%s#" test-file)))
      ;; Should reject tags not in org-tag-alist
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         "Task" "TODO" '("invalid") nil ,parent-uri)))))

(ert-deftest org-mcp-test-add-todo-tag-accept-valid-with-alist ()
  "Test that tags in `org-tag-alist' are accepted."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "org-headline://%s#" test-file)))
      ;; Should accept tags in org-tag-alist (work, personal, urgent)
      (let ((result
             (org-mcp-test--call-add-todo
              "ValidTask" "TODO" '("work") nil parent-uri
              nil)))
        ;; Check result structure - should have exactly 4 fields
        (org-mcp-test--check-add-todo-result
         result
         "ValidTask"
         (file-name-nondirectory test-file)
         test-file
         (concat
          "^\\* TODO ValidTask +:work:\n"
          "\\(?: *:PROPERTIES:\n"
          " *:ID: +[^\n]+\n"
          " *:END:\n\\)?$"))))))

(ert-deftest org-mcp-test-add-todo-tag-validation-without-alist ()
  "Test valid tag names are accepted when `org-tag-alist' is empty."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-uri (format "org-headline://%s#" test-file)))
        ;; Should accept valid tag names (alphanumeric, _, @)
        (let ((result
               (org-mcp-test--call-add-todo
                "Task1"
                "TODO"
                '("validtag" "tag123" "my_tag" "@home")
                nil
                parent-uri
                nil)))
          ;; Check result structure - should have exactly 4 fields
          (org-mcp-test--check-add-todo-result
           result "Task1" (file-name-nondirectory test-file) test-file
           (concat
            "^\\* TODO Task1 +:"
            ".*validtag.*tag123.*my_tag.*@home.*:\n"
            "\\(?: *:PROPERTIES:\n"
            " *:ID: +[^\n]+\n"
            " *:END:\n\\)?$")))))))

(ert-deftest org-mcp-test-add-todo-tag-invalid-characters ()
  "Test that tags with invalid characters are rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((org-tag-alist nil)
          (org-tag-persistent-alist nil))
      (let ((parent-uri (format "org-headline://%s#" test-file)))
        ;; Should reject tags with special characters
        (org-mcp-test--assert-error-and-file
         test-file
         `(org-mcp-test--call-add-todo-expecting-error
           "Task" "TODO" '("invalid-tag!") nil ,parent-uri))
        (org-mcp-test--assert-error-and-file
         test-file
         `(org-mcp-test--call-add-todo-expecting-error
           "Task" "TODO" '("tag-with-dash") nil ,parent-uri))
        (org-mcp-test--assert-error-and-file
         test-file
         `(org-mcp-test--call-add-todo-expecting-error
           "Task" "TODO" '("tag#hash") nil ,parent-uri))))))

(ert-deftest org-mcp-test-add-todo-child-under-parent ()
  "Test adding a child TODO under an existing parent."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-parent-child
    (let* ((parent-uri
            (format "org-headline://%s#Parent%%20Task" test-file))
           (result
            (org-mcp-test--call-add-todo "Child Task" "TODO" '("work")
                                         nil ; no body
                                         parent-uri
                                         nil))) ; no afterUri
      ;; Check result structure - should have exactly 4 fields
      (org-mcp-test--check-add-todo-result
       result
       "Child Task"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-add-todo-child-empty-after-uri ()
  "Test adding a child TODO with empty string for after_uri.
Empty string should be treated as nil - append as last child."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-parent-child
    (let* ((parent-uri
            (format "org-headline://%s#Parent%%20Task" test-file))
           (result
            (org-mcp-test--call-add-todo "Child Task" "TODO" '("work")
                                         nil ; no body
                                         parent-uri
                                         ""))) ; empty string after_uri
      ;; Check result structure - should have exactly 4 fields
      (org-mcp-test--check-add-todo-result
       result
       "Child Task"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-child-under-parent))))

(ert-deftest org-mcp-test-add-todo-second-child-same-level ()
  "Test that adding a second child creates it at the same level as first child.
This tests the bug where the second child was created at level 4 instead of level 3."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-parent-with-one-child
    (let* ((parent-uri
            (format "org-headline://%s#Parent%%20Project/Parent%%20Task"
                    test-file))
           (result
            (org-mcp-test--call-add-todo
             "Second Child" "TODO" '("work")
             nil  ; no body
             parent-uri
             nil))) ; no after_uri
      ;; Check result structure
      (org-mcp-test--check-add-todo-result
       result
       "Second Child"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-second-child-same-level))))

(ert-deftest org-mcp-test-add-todo-with-after-uri ()
  "Test adding TODO after a sibling using after_uri.
Tests that adding after a level 3 sibling correctly creates level 3 (not level 1).
Reproduces the emacs.org scenario: level 2 parent (via path), level 3 sibling (via ID)."
  (let ((initial-content org-mcp-test--content-level2-parent-level3-children))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-tag-alist '("internet")))
        (org-mcp-test--with-id-setup
            `((,org-mcp-test--level2-parent-level3-sibling-id . ,test-file))
          (let* ((parent-uri
                  (format "org-headline://%s#Top%%20Level/Review%%20the%%20package"
                          test-file))
                 (after-uri (format "org-id://%s"
                                    org-mcp-test--level2-parent-level3-sibling-id))
                 (result
                  (org-mcp-test--call-add-todo
                   "Review org-mcp-test.el" "TODO" '("internet")
                   nil
                   parent-uri
                   after-uri)))
            ;; BUG: org-insert-heading creates level 1 (*) instead of level 3 (***)
            (org-mcp-test--check-add-todo-result
             result
             "Review org-mcp-test.el"
             (file-name-nondirectory test-file)
             test-file
             org-mcp-test--regex-after-sibling-level3)))))))

(ert-deftest org-mcp-test-add-todo-with-body ()
  "Test adding TODO with body text."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (body-text org-mcp-test--body-text-multiline)
           (result
            (org-mcp-test--call-add-todo
             "Task with Body" "TODO" '("work") body-text parent-uri)))
      ;; Check result structure - should have exactly 4 fields
      (org-mcp-test--check-add-todo-result
       result
       "Task with Body"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-with-body))))

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
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (body-with-asterisk "Some initial text.\n*")
           (result
            (org-mcp-test--call-add-todo
             "Task" "TODO" '("work") body-with-asterisk parent-uri
             nil)))
      ;; Should succeed since * without space is not a headline
      (should result)
      (should (alist-get 'success result)))))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-block ()
  "Test that adding TODO with body containing unbalanced block is rejected.
Unbalanced blocks like #+BEGIN_EXAMPLE without #+END_EXAMPLE should be
rejected in TODO body content."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let
        ((parent-uri (format "org-headline://%s#" test-file))
         (body-with-unbalanced-block
          "Here's an example:\n#+BEGIN_EXAMPLE\nsome code\nMore text after block"))
      ;; Should reject unbalanced blocks
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         "Task with unbalanced block"
         "TODO"
         '("work")
         ,body-with-unbalanced-block
         ,parent-uri)))))

(ert-deftest org-mcp-test-add-todo-body-with-unbalanced-end-block ()
  "Test that adding TODO with body containing unbalanced END block is rejected.
An #+END_EXAMPLE without matching #+BEGIN_EXAMPLE should be rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "org-headline://%s#" test-file))
          (body-with-unbalanced-end
           "Some text before\n#+END_EXAMPLE\nMore text after"))
      ;; Should reject unbalanced END blocks
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         "Task with unbalanced END block"
         "TODO"
         '("work")
         ,body-with-unbalanced-end
         ,parent-uri)))))

(ert-deftest org-mcp-test-add-todo-body-with-literal-block-end ()
  "Test that TODO body with END_SRC inside EXAMPLE block is accepted.
#+END_SRC inside an EXAMPLE block is literal text, not a block delimiter.
This is valid Org-mode syntax and should be allowed."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let*
        ((parent-uri (format "org-headline://%s#" test-file))
         (body-with-literal-end
          "Example of source block:\n#+BEGIN_EXAMPLE\n#+END_SRC\n#+END_EXAMPLE\nText after.")
         (result
          (org-mcp-test--call-add-todo "Task with literal END_SRC"
                                       "TODO"
                                       '("work")
                                       body-with-literal-end
                                       parent-uri
                                       nil)))
      ;; Should succeed - #+END_SRC is just literal text inside EXAMPLE block
      (should result)
      (should (alist-get 'success result))
      ;; Verify the content was added correctly
      (with-current-buffer (find-file-noselect test-file)
        (goto-char (point-min))
        (should (search-forward "#+BEGIN_EXAMPLE" nil t))
        (should (search-forward "#+END_SRC" nil t))
        (should (search-forward "#+END_EXAMPLE" nil t))))))

(ert-deftest org-mcp-test-add-todo-after-sibling ()
  "Test adding TODO after a specific sibling."
  (let ((initial-content org-mcp-test--content-siblings-after-test))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-tag-alist '("work")))
        ;; First add IDs to existing items so we can reference them
        (let ((second-id nil))
          (with-temp-buffer
            (set-visited-file-name test-file t)
            (insert-file-contents test-file)
            (org-mode)
            (goto-char (point-min))
            ;; Add ID to Second Child
            (re-search-forward "^\\*\\* Second Child")
            (org-id-get-create)
            (setq second-id (org-id-get))
            (write-region (point-min) (point-max) test-file))
          ;; Kill any buffer visiting the test file
          (let ((buf (find-buffer-visiting test-file)))
            (when buf
              (kill-buffer buf)))

          (org-mcp-test--with-id-setup `((,second-id . ,test-file))
            (let* ((parent-uri
                    (format "org-headline://%s#Parent%%20Task"
                            test-file))
                   (after-uri (format "org-id://%s" second-id))
                   (result
                    (org-mcp-test--call-add-todo
                     "New Task After Second"
                     "TODO"
                     '("work")
                     nil
                     parent-uri
                     after-uri)))
              ;; Check result has exactly 4 fields
              (org-mcp-test--check-add-todo-result
               result
               "New Task After Second"
               (file-name-nondirectory test-file)
               test-file
               org-mcp-test--regex-todo-after-sibling))))))))


(ert-deftest org-mcp-test-add-todo-afterUri-not-sibling ()
  "Test error when afterUri is not a child of parentUri."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-parent-child-siblings-with-other-child-id
    (let ((org-mcp-allowed-files (list test-file))
          (org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-tag-alist '("work")))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--other-child-id . ,test-file))
        (let* ((parent-uri
                (format "org-headline://%s#Parent%%20Task"
                        test-file))
               (after-uri
                (format "org-id://%s" org-mcp-test--other-child-id)))
          ;; Error: Other Child is not a child of Parent Task
          (should-error
           (org-mcp-test--call-add-todo
            "New Task" "TODO" '("work") nil parent-uri
            after-uri)
           :type 'mcp-server-lib-tool-error)
          ;; Verify file was NOT modified
          (with-temp-buffer
            (insert-file-contents test-file)
            (should
             (string=
              (buffer-string)
              org-mcp-test--content-parent-child-siblings-with-other-child-id))))))))

(ert-deftest org-mcp-test-add-todo-parent-id-uri ()
  "Test adding TODO with parent specified as org-id:// URI."
  (let ((initial-content org-mcp-test--content-parent-task-simple))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)")))
            (org-tag-alist '("work"))
            (org-id-locations-file nil))
        ;; Add ID to Parent Task
        (let ((parent-id nil))
          (with-temp-buffer
            (set-visited-file-name test-file t)
            (insert-file-contents test-file)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* Parent Task")
            (org-id-get-create)
            (setq parent-id (org-id-get))
            (write-region (point-min) (point-max) test-file))
          ;; Kill any buffer visiting the test file
          (let ((buf (find-buffer-visiting test-file)))
            (when buf
              (kill-buffer buf)))

          (org-mcp-test--with-enabled
            ;; Use org-id:// for parent instead of org-headline://
            (let* ((parent-uri (format "org-id://%s" parent-id))
                   (result
                    (org-mcp-test--call-add-todo
                     "Child via ID" "TODO" '("work") nil parent-uri
                     nil)))
              ;; Check result has exactly 4 fields
              (org-mcp-test--check-add-todo-result
               result
               "Child via ID"
               (file-name-nondirectory test-file)
               test-file
               (concat
                "^\\* Parent Task\n"
                "\\(?: *:PROPERTIES:\n"
                " *:ID: +[^\n]+\n"
                " *:END:\n\\)?"
                "Some parent content\\.\n\n?"
                "\\*\\* TODO Child via ID +:work:\n"
                "\\(?: *:PROPERTIES:\n"
                " *:ID: +[^\n]+\n"
                " *:END:\n\\)?\n?"
                "\\* Another Task")))))))))


(ert-deftest org-mcp-test-add-todo-mutex-tags-error ()
  "Test that mutually exclusive tags are rejected."
  (let ((initial-content "#+TITLE: Test Org File\n\n"))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-track-globally nil)
            (org-id-locations-file nil)
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            ;; Configure mutex tag groups
            (org-tag-alist
             '(("work" . ?w)
               :startgroup
               ("@office" . ?o)
               ("@home" . ?h)
               :endgroup)))
        (org-mcp-test--with-enabled
          ;; Try to add TODO with conflicting tags - should error
          (let ((parent-uri (format "org-headline://%s#" test-file)))
            ;; This should throw an error
            (should-error
             (org-mcp-test--call-add-todo
              "Test Task"
              "TODO"
              ["work" "@office" "@home"] ; conflicting tags
              nil
              parent-uri
              nil)
             :type 'mcp-server-lib-tool-error)))))))

(ert-deftest org-mcp-test-add-todo-mutex-tags-valid ()
  "Test that non-conflicting tags from mutex groups are accepted."
  (let ((initial-content "#+TITLE: Test Org File\n\n"))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-track-globally nil)
            (org-id-locations-file nil)
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            ;; Configure mutex tag groups
            (org-tag-alist
             '(("work" . ?w)
               :startgroup
               ("@office" . ?o)
               ("@home" . ?h)
               :endgroup ("project" . ?p))))
        (org-mcp-test--with-enabled
          ;; Add TODO with non-conflicting tags
          (let* ((parent-uri (format "org-headline://%s#" test-file))
                 (result
                  (org-mcp-test--call-add-todo
                   "Test Task"
                   "TODO"
                   ["work" "@office" "project"] ; no conflict
                   nil
                   parent-uri
                   nil)))
            ;; Should succeed
            (should (assoc 'success result))
            (should (equal (alist-get 'success result) t))
            ;; Verify tags were added correctly
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                ;; Check task was added
                (should (string-match-p "\\* TODO Test Task" content))
                ;; Check all tags are present (order doesn't matter)
                (should (string-match-p ":work:" content))
                (should (string-match-p ":@office:" content))
                (should (string-match-p ":project:" content))))))))))

(ert-deftest org-mcp-test-add-todo-nil-tags ()
  "Test that adding TODO with nil tags creates headline without tags."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (result
            (org-mcp-test--call-add-todo "Task Without Tags" "TODO"
                                         nil ; nil for tags
                                         nil ; no body
                                         parent-uri
                                         nil))) ; no afterUri
      (org-mcp-test--check-add-todo-result
       result
       "Task Without Tags"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-without-tags))))

(ert-deftest org-mcp-test-add-todo-empty-list-tags ()
  "Test that adding TODO with empty list tags creates headline without tags."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (result
            (org-mcp-test--call-add-todo "Task Without Tags" "TODO"
                                         '() ; empty list for tags
                                         nil ; no body
                                         parent-uri
                                         nil))) ; no afterUri
      (org-mcp-test--check-add-todo-result
       result
       "Task Without Tags"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-without-tags))))

(ert-deftest org-mcp-test-rename-headline-simple ()
  "Test renaming a simple TODO headline."
  (let ((initial-content org-mcp-test--content-simple-todo))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Rename the headline
          (let* ((resource-uri
                  (format "org-headline://%s#Original%%20Task"
                          test-file))
                 (params
                  `((uri . ,resource-uri)
                    (current_title . "Original Task")
                    (new_title . "Updated Task")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result structure
            (should (= (length result) 4))
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previous_title result) "Original Task"))
            (should
             (equal (alist-get 'new_title result) "Updated Task"))
            ;; Should return an org-id:// URI
            (should
             (string-match "^org-id://" (alist-get 'uri result)))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              (should
               (string-match-p
                org-mcp-test--pattern-renamed-simple-todo
                (buffer-string))))))))))

(ert-deftest org-mcp-test-rename-headline-title-mismatch ()
  "Test that rename fails when current title doesn't match."
  (org-mcp-test--with-temp-org-file test-file org-mcp-test--content-simple-todo
    (let ((org-mcp-allowed-files (list test-file))
          (org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-mcp-test--with-enabled
        ;; Try to rename with wrong current title
        (let* ((resource-uri
                (format "org-headline://%s#Original%%20Task"
                        test-file)))
          (should-error
           (org-mcp-test--call-rename-headline-expecting-error
            resource-uri "Wrong Title" "Updated Task")
           :type 'mcp-server-lib-tool-error)
          ;; Verify file was not modified
          (org-mcp-test--verify-file-eq
           test-file org-mcp-test--content-simple-todo))))))

(ert-deftest org-mcp-test-rename-headline-preserve-tags ()
  "Test that renaming preserves tags."
  (org-mcp-test--with-temp-org-file test-file org-mcp-test--content-todo-with-tags
    (let ((org-mcp-allowed-files (list test-file))
          (org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-tag-alist '("work" "urgent" "personal")))
      (org-mcp-test--with-enabled
        ;; Rename the headline
        (let* ((resource-uri
                (format "org-headline://%s#Task%%20with%%20Tags"
                        test-file))
               (params
                `((uri . ,resource-uri)
                  (current_title . "Task with Tags")
                  (new_title . "Renamed Task")))
               (result-text
                (mcp-server-lib-ert-call-tool
                 "org-rename-headline" params))
               (result (json-read-from-string result-text)))
          ;; Check result
          (should (equal (alist-get 'success result) t))
          (should
           (equal
            (alist-get 'previous_title result) "Task with Tags"))
          (should
           (equal (alist-get 'new_title result) "Renamed Task"))
          ;; Verify file content - tags should be preserved
          ;; org-edit-headline may add spaces for tag alignment
          (org-mcp-test--verify-file-matches
           test-file
           org-mcp-test--pattern-renamed-todo-with-tags))))))

(ert-deftest org-mcp-test-rename-headline-no-todo ()
  "Test renaming a regular headline without TODO state."
  (org-mcp-test--with-temp-org-file test-file org-mcp-test--content-headline-no-todo
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        ;; Rename the headline
        (let* ((resource-uri
                (format "org-headline://%s#Regular%%20Headline"
                        test-file))
               (params
                `((uri . ,resource-uri)
                  (current_title . "Regular Headline")
                  (new_title . "Updated Headline")))
               (result-text
                (mcp-server-lib-ert-call-tool
                 "org-rename-headline" params))
               (result (json-read-from-string result-text)))
          ;; Check result
          (should (equal (alist-get 'success result) t))
          ;; Verify file content
          (org-mcp-test--verify-file-matches
           test-file
           org-mcp-test--pattern-renamed-headline-no-todo))))))

(ert-deftest org-mcp-test-rename-headline-nested-path-navigation ()
  "Test correct headline path navigation in nested structures.
Verifies that the implementation correctly navigates nested headline
paths and only matches headlines at the appropriate hierarchy level."
  (let ((initial-content org-mcp-test--content-nested-path-navigation))
    (org-mcp-test--with-temp-org-file
     test-file initial-content
     (let ((org-mcp-allowed-files (list test-file)))
       (org-mcp-test--with-enabled
        ;; Try to rename "Parent Two/Child"
        ;; But there's no Child under Parent Two!
        ;; The function should fail, but it might incorrectly
        ;; find Parent Three's Child
        (let* ((resource-uri
                (format "org-headline://%s#Parent%%20Two/Child"
                        test-file)))
          ;; This should throw an error because Parent Two has no Child
          (let* ((request
                   (mcp-server-lib-create-tools-call-request
                    "org-rename-headline" 1
                    `((uri . ,resource-uri)
                      (current_title . "Child")
                      (new_title . "Renamed Child"))))
                 (response
                  (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id)))
            (should-error
             (mcp-server-lib-ert-process-tool-response response)
             :type 'mcp-server-lib-tool-error))))))))

(ert-deftest org-mcp-test-rename-headline-by-id ()
  "Test renaming a headline accessed by org-id URI."
  (let ((initial-content org-mcp-test--content-with-id))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-track-globally t)
            (org-id-locations-file
             (make-temp-file "org-id-locations")))
        ;; Manually add the ID to org-id locations
        (org-id-update-id-locations (list test-file))
        (org-mcp-test--with-enabled
          ;; Rename using ID-based URI
          (let* ((params
                  `((uri . ,org-mcp-test--content-with-id-uri)
                    (current_title . "Task with ID")
                    (new_title . "Renamed Task with ID")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal (alist-get 'previous_title result) "Task with ID"))
            (should
             (equal
              (alist-get 'new_title result) "Renamed Task with ID"))
            ;; URI should remain ID-based (not converted to path-based)
            (should
             (equal
              (alist-get 'uri result)
              org-mcp-test--content-with-id-uri))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                (should
                 (string-match
                  org-mcp-test--expected-regex-renamed-task-with-id
                  content))))))))))

(ert-deftest org-mcp-test-rename-headline-id-not-found ()
  "Test error when ID doesn't exist."
  (let ((initial-content "* Some Task\nContent here."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-track-globally nil)
            (org-id-locations-file nil))
        (org-mcp-test--with-enabled
          ;; Try to rename non-existent ID
          (let* ((resource-uri "org-id://non-existent-id-12345")
                 (request
                  (mcp-server-lib-create-tools-call-request
                   "org-rename-headline" 1
                   `((uri . ,resource-uri)
                     (current_title . "Whatever")
                     (new_title . "Should Fail"))))
                 (response
                  (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id)))
            (should-error
             (mcp-server-lib-ert-process-tool-response response)
             :type 'mcp-server-lib-tool-error)))))))

(ert-deftest org-mcp-test-rename-headline-with-slash ()
  "Test renaming a headline containing a slash character.
Slashes must be properly URL-encoded to avoid path confusion."
  (let ((initial-content
         "* Project A/B Testing
This is a headline with a slash in it.
* Other Task
Some other content."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; The slash should be encoded as %2F in the URI
          (let* ((resource-uri
                  (format
                   "org-headline://%s#Project%%20A%%2FB%%20Testing"
                   test-file))
                 (params
                  `((uri . ,resource-uri)
                    (current_title . "Project A/B Testing")
                    (new_title . "Project A/B Experiments")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previous_title result)
              "Project A/B Testing"))
            (should
             (equal
              (alist-get 'new_title result) "Project A/B Experiments"))
            ;; Should return an org-id:// URI
            (should
             (string-match "^org-id://" (alist-get 'uri result)))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              ;; Check complete buffer with single regex
              (should
               (string-match-p
                "^\\* Project A/B Experiments\n *:PROPERTIES:\n *:ID: +[A-F0-9-]+\n *:END:\nThis is a headline with a slash in it\\.\n\\* Other Task\nSome other content\\.$"
                (buffer-string))))))))))

(ert-deftest org-mcp-test-rename-headline-slash-not-nested ()
  "Test that headline with slash is not treated as nested path.
Verifies that 'Parent/Child' is treated as a single headline,
not as Child under Parent."
  (let ((initial-content
         "* Parent
** Real Child
Content here.
* Parent/Child
This is a single headline with a slash, not nested under Parent."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; Try to rename the "Parent/Child" headline
          (let* ((resource-uri
                  ;; Slash encoded as %2F to indicate single headline
                  (format "org-headline://%s#Parent%%2FChild"
                          test-file))
                 (params
                  `((uri . ,resource-uri)
                    (current_title . "Parent/Child")
                    (new_title . "Parent-Child Renamed")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Should succeed
            (should (equal (alist-get 'success result) t))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                ;; The single headline with slash should be renamed
                (should
                 (string-match-p
                  "^\\* Parent-Child Renamed$" content))
                ;; The actual Parent headline should be unchanged
                (should (string-match-p "^\\* Parent$" content))
                ;; The Real Child should still be under Parent
                (should
                 (string-match-p
                  "^\\*\\* Real Child$" content))))))))))

(ert-deftest org-mcp-test-rename-headline-with-percent ()
  "Test renaming a headline containing a percent sign.
Percent signs must be properly URL-encoded to avoid double-encoding issues."
  (let ((initial-content
         "* 50% Complete
This task is half done.
* Use %20 for spaces
Documentation about URL encoding."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; The percent should be encoded as %25 in the URI
          (let* ((resource-uri
                  (format "org-headline://%s#50%%25%%20Complete"
                          test-file))
                 (params
                  `((uri . ,resource-uri)
                    (current_title . "50% Complete")
                    (new_title . "75% Complete")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal (alist-get 'previous_title result) "50% Complete"))
            (should
             (equal (alist-get 'new_title result) "75% Complete"))
            ;; Should return an org-id:// URI
            (should
             (string-match "^org-id://" (alist-get 'uri result)))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              ;; Check complete buffer with single regex
              (should
               (string-match-p
                "^\\* 75% Complete\n *:PROPERTIES:\n *:ID: +[A-F0-9-]+\n *:END:\nThis task is half done\\.\n\\* Use %20 for spaces\nDocumentation about URL encoding\\.$"
                (buffer-string))))))))))

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
   org-mcp-test--content-headline-no-todo
   "Regular Headline"
   "First Line\nSecond Line"))

(ert-deftest org-mcp-test-rename-headline-duplicate-first-match ()
  "Test that when multiple headlines have the same name, first match is renamed.
This test documents the first-match behavior when duplicate headlines exist."
  (let ((initial-content
         "* Team Updates
** Project Review
First review content.
* Development Tasks
** Project Review
Second review content.
* Planning
** Project Review
Third review content."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; Use headline:// URI with ambiguous path
          (let ((resource-uri
                 (format "org-headline://%s#Project%%20Review"
                         test-file)))
            ;; Should succeed - renames first match
            (org-mcp--tool-rename-headline
             resource-uri "Project Review" "Q1 Review"))

          ;; Verify only first occurrence was renamed
          (with-temp-buffer
            (insert-file-contents test-file)
            (let ((content (buffer-string)))
              ;; Check entire structure: first renamed with ID, others unchanged
              (should
               (string-match-p
                (concat
                 "\\`\\* Team Updates\n"
                 "\\*\\* Q1 Review\n" ; First renamed
                 " *:PROPERTIES:\n"
                 " *:ID: +[A-F0-9-]+\n" ; ID added
                 " *:END:\n"
                 "First review content\\.\n"
                 "\\* Development Tasks\n"
                 "\\*\\* Project Review\n" ; Second unchanged
                 "Second review content\\.\n"
                 "\\* Planning\n"
                 "\\*\\* Project Review\n" ; Third unchanged
                 "Third review content\\.\\'")
                content)))))))))

(ert-deftest org-mcp-test-rename-headline-creates-id ()
  "Test that renaming a headline creates an Org ID and returns it."
  (let ((initial-content
         "* Headline Without ID
Content here."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-track-globally t)
            (org-id-locations-file (make-temp-file "test-org-id")))
        (org-mcp-test--with-enabled
          ;; Rename headline using path-based URI
          (let* ((resource-uri
                  (format
                   "org-headline://%s#Headline%%20Without%%20ID"
                   test-file))
                 (params
                  `((uri . ,resource-uri)
                    (current_title . "Headline Without ID")
                    (new_title . "Renamed Headline")))
                 (request
                  (mcp-server-lib-create-tools-call-request
                   "org-rename-headline" 1 params))
                 (response
                  (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id))
                 (result
                  (mcp-server-lib-ert-process-tool-response
                   response)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previous_title result)
              "Headline Without ID"))
            (should
             (equal (alist-get 'new_title result) "Renamed Headline"))
            ;; The returned URI should now be an org-id:// URI
            (let ((returned-uri (alist-get 'uri result)))
              (should (string-match "^org-id://" returned-uri))
              ;; Extract the ID from the URI
              (let ((id
                     (replace-regexp-in-string
                      "^org-id://" "" returned-uri)))
                ;; Verify the ID was added to the file
                (with-temp-buffer
                  (insert-file-contents test-file)
                  (let ((content (buffer-string)))
                    ;; Title should be renamed
                    (should
                     (string-match-p
                      "^\\* Renamed Headline$" content))
                    ;; ID property should exist
                    (should
                     (string-match-p
                      (format ":ID:\\s-+%s" id) content))))))))))))


(ert-deftest org-mcp-test-rename-headline-hierarchy ()
  "Test that headline hierarchy is correctly navigated.
Ensures that when searching for nested headlines, the function
correctly restricts search to the parent's subtree."
  (let ((initial-content
         "* First Section
** Target
Some content.
* Second Section
** Other Item
More content.
** Target
This Target is under Second Section, not First Section."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; Try to access "First Section/Target"
          ;; But after finding "First Section", the search continues
          ;; and might find "Second Section/Target" instead
          (let* ((resource-uri
                  (format "org-headline://%s#Second%%20Section/Target"
                          test-file))
                 (params
                  `((uri . ,resource-uri)
                    (current_title . "Target")
                    (new_title . "Renamed Target")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Should succeed and rename the correct Target
            (should (equal (alist-get 'success result) t))
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                ;; First Section's Target should NOT be renamed
                (should
                 (string-match-p
                  "\\* First Section\n\\*\\* Target" content))
                ;; Second Section's Target SHOULD be renamed
                (should
                 (string-match-p
                  "\\* Second Section\n\\*\\* Other Item\n[^\n]*\n\\*\\* Renamed Target"
                  content))))))))))

(ert-deftest org-mcp-test-rename-headline-with-todo-keyword ()
  "Test that headlines with TODO keywords can be renamed.
The navigation function should find headlines even when they have TODO keywords."
  (let ((initial-content
         "* Project Management
** TODO Review Documents
This task needs to be renamed
** DONE Review Code
This is already done"))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; Try to rename using the headline title without TODO keyword
          (let
              ((resource-uri
                (format
                 "org-headline://%s#Project%%20Management/Review%%20Documents"
                 test-file)))
            ;; This should work - finding "Review Documents" even though
            ;; the actual headline is "TODO Review Documents"
            (org-mcp--tool-rename-headline
             resource-uri "Review Documents" "Q1 Planning Review"))

          ;; Verify the headline was renamed correctly
          (with-temp-buffer
            (insert-file-contents test-file)
            (let ((content (buffer-string)))
              ;; The TODO keyword should be preserved
              (should
               (string-match-p
                "^\\*\\* TODO Q1 Planning Review$" content))
              ;; The DONE headline should be unchanged
              (should
               (string-match-p
                "^\\*\\* DONE Review Code$" content)))))))))

;;; org-edit-body tests

(defun org-mcp-test--call-edit-body
    (resource-uri old-body new-body &optional replace-all)
  "Call org-edit-body tool via JSON-RPC and return the result.
RESOURCE-URI is the URI of the node to edit.
OLD-BODY is the substring to search for within the node's body.
NEW-BODY is the replacement text.
REPLACE-ALL if true, replace all occurrences (default: nil)."
  (let* ((params
          `((resource_uri . ,resource-uri)
            (old_body . ,old-body)
            (new_body . ,new-body)
            (replace_all . ,replace-all)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-edit-body" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request mcp-server-lib-ert-server-id)))
    (mcp-server-lib-ert-process-tool-response response)))

(defun org-mcp-test--verify-file-eq (test-file expected-content)
  "Verify TEST-FILE containing exactly EXPECTED-CONTENT."
  (with-temp-buffer
    (insert-file-contents test-file)
    (should (string= (buffer-string) expected-content))))

(defun org-mcp-test--verify-file-matches (test-file expected-pattern)
  "Verify TEST-FILE content matches EXPECTED-PATTERN regexp."
  (with-temp-buffer
    (insert-file-contents test-file)
    (should (string-match-p expected-pattern (buffer-string)))))

(defmacro org-mcp-test--assert-error-and-unchanged
    (error-form test-file expected-content)
  "Assert ERROR-FORM throws an error and TEST-FILE remains unchanged.
ERROR-FORM should be a form that is expected to signal an error.
TEST-FILE is the file to verify.
EXPECTED-CONTENT is the exact content the file should have."
  `(progn
     (should-error ,error-form :type 'mcp-server-lib-tool-error)
     (org-mcp-test--verify-file-eq ,test-file ,expected-content)))

(defun org-mcp-test--check-edit-body-result
    (result test-file expected-pattern &optional expected-id)
  "Check edit-body RESULT structure and file content.
RESULT is the return value from `org-edit-body` tool.
TEST-FILE is the path to the file to check.
EXPECTED-PATTERN is a regexp that the file content should match.
EXPECTED-ID if provided, check the returned URI has this exact ID."
  (should (= (length result) 2))
  (should (equal (alist-get 'success result) t))
  (let ((uri (alist-get 'uri result)))
    (if expected-id
        (should (equal uri (concat "org-id://" expected-id)))
      (should (string-prefix-p "org-id://" uri))))
  (org-mcp-test--verify-file-matches test-file expected-pattern))

(ert-deftest org-mcp-test-edit-body-single-line ()
  "Test org-edit-body tool for single-line replacement."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (let ((result
               (org-mcp-test--call-edit-body
                org-mcp-test--content-with-id-uri
                "First line of content."
                "Updated first line."
                nil)))
          (org-mcp-test--check-edit-body-result
           result
           test-file
           org-mcp-test--pattern-edit-body-single-line
           org-mcp-test--content-with-id-id))))))

(ert-deftest org-mcp-test-edit-body-multiline ()
  "Test org-edit-body tool for multi-line replacement."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id-todo
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (let ((result
               (org-mcp-test--call-edit-body
                org-mcp-test--content-with-id-uri
                "Second line of content."
                "This has been replaced
with new multiline
content here."
                nil)))
          (org-mcp-test--check-edit-body-result
           result test-file org-mcp-test--pattern-edit-body-multiline
           org-mcp-test--content-with-id-id))))))

(ert-deftest org-mcp-test-edit-body-multiple-without-replaceall ()
  "Test error for multiple occurrences without replaceAll."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id-repeated-text
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup `(("test-id" . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          "org-id://test-id" "occurrence of pattern" "REPLACED" nil)
         test-file org-mcp-test--content-with-id-repeated-text)))))

(ert-deftest org-mcp-test-edit-body-replace-all ()
  "Test org-edit-body tool with replaceAll functionality."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id-repeated-text
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup `(("test-id" . ,test-file))
        (let ((result
               (org-mcp-test--call-edit-body "org-id://test-id"
                                             "occurrence of pattern"
                                             "REPLACED"
                                             t))) ; replaceAll = true
          (org-mcp-test--check-edit-body-result
           result
           test-file
           org-mcp-test--pattern-edit-body-replace-all))))))

(ert-deftest org-mcp-test-edit-body-replace-all-explicit-false ()
  "Test that explicit replace_all=false triggers error on multiple matches."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id-repeated-text
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup `(("test-id" . ,test-file))
        ;; Should error because multiple occurrences exist
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp-test--call-edit-body "org-id://test-id"
                                       "occurrence of pattern"
                                       "REPLACED"
                                       :false) ; :false = JSON false
         test-file
         org-mcp-test--content-with-id-repeated-text)))))

(ert-deftest org-mcp-test-edit-body-not-found ()
  "Test org-edit-body tool error when text is not found."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri
          "nonexistent text"
          "replacement"
          nil)
         test-file org-mcp-test--content-with-id)))))

(ert-deftest org-mcp-test-edit-body-empty ()
  "Test org-edit-body tool can add content to empty body."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-parent-task-simple
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (let* ((resource-uri
                (format "org-headline://%s#Another%%20Task"
                        test-file))
               (result
                (org-mcp-test--call-edit-body
                 resource-uri "" "New content added."
                 nil)))
          (org-mcp-test--check-edit-body-result
           result
           test-file
           org-mcp-test--pattern-edit-body-empty))))))

(ert-deftest org-mcp-test-edit-body-empty-old-non-empty-body ()
  "Test error when oldBody is empty but body has content."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri
          "" ; Empty oldBody
          "replacement" nil)
         test-file org-mcp-test--content-with-id)))))

(ert-deftest org-mcp-test-edit-body-empty-with-properties ()
  "Test adding content to empty body with properties drawer."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id-no-body
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup `((,org-mcp-test--timestamp-id
                                      . ,test-file))
        (let ((result
               (org-mcp-test--call-edit-body
                (format "org-id://%s" org-mcp-test--timestamp-id)
                ""
                "Content added after properties."
                nil)))
          (org-mcp-test--check-edit-body-result
           result
           test-file
           org-mcp-test--pattern-edit-body-empty-with-props))))))

(ert-deftest org-mcp-test-edit-body-nested-headlines ()
  "Test org-edit-body preserves nested headlines."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-parent-child
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (let ((result
               (org-mcp-test--call-edit-body
                (format "org-headline://%s#Parent%%20Task" test-file)
                "Some content here"
                "Updated parent content"
                nil)))
          (org-mcp-test--check-edit-body-result
           result
           test-file
           org-mcp-test--pattern-edit-body-nested-headlines))))))

(ert-deftest org-mcp-test-edit-body-reject-headline-in-middle ()
  "Test org-edit-body rejects newBody with headline marker in middle."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri "First line of content."
          "replacement text
* This would become a headline"
          nil)
         test-file org-mcp-test--content-with-id)))))

(ert-deftest org-mcp-test-edit-body-accept-lower-level-headline ()
  "Test org-edit-body accepts newBody with lower-level headline."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (let ((result
               (org-mcp-test--call-edit-body
                org-mcp-test--content-with-id-uri
                "First line of content."
                "some text
** Subheading content"
                nil)))
          (org-mcp-test--check-edit-body-result
           result
           test-file
           org-mcp-test--pattern-edit-body-accept-lower-level))))))

(ert-deftest org-mcp-test-edit-body-reject-higher-level-headline ()
  "Test org-edit-body rejects newBody with higher-level headline.
When editing a level 2 node, level 1 headlines should be rejected."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-parent-child
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          (format "org-headline://%s#Parent%%20Task/Another%%20Task"
                  test-file)
          "More content."
          "New text
* Top level heading"
          nil)
         test-file org-mcp-test--content-parent-child)))))

(ert-deftest org-mcp-test-edit-body-reject-headline-at-start ()
  "Test org-edit-body rejects newBody with headline at beginning."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri
          "First line of content."
          "* Heading at start"
          nil)
         test-file org-mcp-test--content-with-id)))))

(ert-deftest org-mcp-test-edit-body-reject-unbalanced-begin-block ()
  "Test org-edit-body rejects newBody with unbalanced BEGIN block."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri "First line of content."
          "Some text
#+BEGIN_EXAMPLE
Code without END_EXAMPLE"
          nil)
         test-file org-mcp-test--content-with-id)))))

(ert-deftest org-mcp-test-edit-body-reject-orphaned-end-block ()
  "Test org-edit-body rejects newBody with orphaned END block."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri "First line of content."
          "Some text
#+END_SRC
Without BEGIN_SRC"
          nil)
         test-file org-mcp-test--content-with-id)))))

(ert-deftest org-mcp-test-edit-body-reject-mismatched-blocks ()
  "Test org-edit-body rejects newBody with mismatched blocks."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (org-mcp-test--assert-error-and-unchanged
         (org-mcp--tool-edit-body
          org-mcp-test--content-with-id-uri "First line of content."
          "Text here
#+BEGIN_QUOTE
Some quote
#+END_EXAMPLE"
          nil)
         test-file org-mcp-test--content-with-id)))))

;;; Resource template workaround tool tests

(ert-deftest org-mcp-test-tool-read-file ()
  "Test org-read-file tool returns same content as file resource."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-headline-no-todo
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (let* ((params `((file . ,test-file)))
               (result-text
                (mcp-server-lib-ert-call-tool "org-read-file" params)))
          (should (string= result-text org-mcp-test--content-headline-no-todo)))))))

(ert-deftest org-mcp-test-tool-read-outline ()
  "Test org-read-outline tool returns valid JSON outline structure."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-nested-siblings
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (let* ((params `((file . ,test-file)))
               (result-json
                (mcp-server-lib-ert-call-tool "org-read-outline" params))
               (result (json-parse-string result-json :object-type 'alist))
               (headings (alist-get 'headings result)))
          (should (= (length headings) 1))
          (should (string= (alist-get 'title (aref headings 0)) "Parent Task")))))))

(ert-deftest org-mcp-test-tool-read-headline-empty-path ()
  "Test org-read-headline with empty headline_path signals validation error."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-headline-no-todo
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (should-error
         (org-mcp-test--call-read-headline-expecting-error test-file "")
         :type 'mcp-server-lib-tool-error)))))

(ert-deftest org-mcp-test-tool-read-headline-single-level ()
  "Test org-read-headline with single-level path."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-slash-in-headline
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (let* ((params `((file . ,test-file)
                         (headline_path . "Project%20A%2FB%20Testing")))
               (result-text
                (mcp-server-lib-ert-call-tool "org-read-headline" params)))
          (should
           (string-match-p
            org-mcp-test--pattern-tool-read-headline-single
            result-text)))))))

(ert-deftest org-mcp-test-tool-read-headline-nested ()
  "Test org-read-headline with nested path."
  (org-mcp-test--with-temp-org-file test-file
      org-mcp-test--content-nested-siblings
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-enabled
        (let* ((params `((file . ,test-file)
                         (headline_path . "Parent%20Task/First%20Child")))
               (result-text
                (mcp-server-lib-ert-call-tool "org-read-headline" params)))
          (should
           (string-match-p
            org-mcp-test--pattern-tool-read-headline-nested
            result-text)))))))

(ert-deftest org-mcp-test-tool-read-by-id ()
  "Test org-read-by-id tool returns headline content by ID."
  (org-mcp-test--with-temp-org-file test-file org-mcp-test--content-with-id
    (let ((org-mcp-allowed-files (list test-file)))
      (org-mcp-test--with-id-setup
          `((,org-mcp-test--content-with-id-id . ,test-file))
        (let* ((params `((uuid . ,org-mcp-test--content-with-id-id)))
               (result-text
                (mcp-server-lib-ert-call-tool "org-read-by-id" params)))
          (should
           (string-match-p
            org-mcp-test--pattern-tool-read-by-id
            result-text)))))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
