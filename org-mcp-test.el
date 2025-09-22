;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

(require 'ert)
(require 'org-mcp)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-ert)
(require 'json)

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
** First Child
First child content.
** Second Child
Second child content.
** Third Child
Third child content."
  "Parent with multiple child tasks.")

(defconst org-mcp-test--content-simple-todo
  "* TODO Original Task
Task description."
  "Simple TODO task.")

(defconst org-mcp-test--content-todo-with-tags
  "* TODO Task with Tags :work:urgent:
Task description."
  "TODO task with tags.")

(defconst org-mcp-test--content-headline-no-todo
  "* Regular Headline
Some content."
  "Regular headline without TODO state.")

(defconst org-mcp-test--content-with-id
  "* Task with ID
:PROPERTIES:
:ID:       550e8400-e29b-41d4-a716-446655440000
:END:
This is a task with an ID property."
  "Task with an Org ID property.")

(defconst org-mcp-test--content-slash-in-headline
  "* Project A/B Testing
This is a headline with a slash in it.
* Other Task
Some other content."
  "Headlines containing slash characters.")

(defconst org-mcp-test--content-parent-child-slash
  "* Parent
** Real Child
Content here.
* Parent/Child
This is a single headline with a slash, not nested under Parent."
  "Mixed nested and slash-containing headlines.")

(defconst org-mcp-test--content-percent-in-headline
  "* 50% Complete
This task is half done.
* Use %20 for spaces
Documentation about URL encoding."
  "Headlines containing percent signs.")

(defconst org-mcp-test--content-duplicate-headlines
  "* Team Updates
** Project Review
First review content.
* Development Tasks
** Project Review
Second review content.
* Quality Assurance
** Project Review
Third review content.
** Code Review
Code review content."
  "Multiple headlines with the same name.")

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

(defconst org-mcp-test--regex-todo-with-tags
  "^\\* TODO New Task +.*:work:urgent:
\\(?::PROPERTIES:
:ID: +[^\n]+
:END:
\\)?$"
  "Pattern for TODO with work and urgent tags.")

(defconst org-mcp-test--regex-todo-after-headers
  (concat
   "^#\\+TITLE: My Org Document\n"
   "#\\+AUTHOR: Test Author\n"
   "#\\+DATE: 2024-01-01\n"
   "#\\+OPTIONS: toc:nil\n"
   "\n"
   "\\* TODO New Top Task +.*:urgent:\n"
   "\\(?::PROPERTIES:\n"
   ":ID: +[^\n]+\n"
   ":END:\n\\)?"
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
   "\\*\\*\\* TODO Child Task +.*:work:.*\n"
   "\\(?::PROPERTIES:\n:ID: +[^\n]+\n:END:\n\\)?")
  "Pattern for child TODO added under parent with existing child.")

(defconst org-mcp-test--regex-todo-with-body
  (concat
   "^\\* TODO Task with Body +:[^\n]*\n"
   "\\(?::PROPERTIES:\n:ID: +[^\n]+\n:END:\n\\)?" ; Optional properties
   "This is the body text\\.\n"
   "It has multiple lines\\.\n"
   "With some content\\.\n?$")
  "Pattern for TODO with body text.")

(defconst org-mcp-test--regex-todo-after-sibling
  (concat
   "^\\* Parent Task\n"
   "\\*\\* First Child\n"
   "\\(?::PROPERTIES:\n:ID: +[^\n]+\n:END:\n\\)?"
   "First child content\\.\n"
   "\\*\\* Second Child\n"
   "\\(?::PROPERTIES:\n:ID: +[^\n]+\n:END:\n\\)?"
   "Second child content\\.\n\n?"
   "\\*\\* TODO New Task After Second +:[^\n]*\n"
   "\\(?::PROPERTIES:\n:ID: +[^\n]+\n:END:\n\\)?"
   "\\*\\* Third Child\n"
   "Third child content\\.")
  "Pattern for TODO added after specific sibling.")

(defconst org-mcp-test--regex-todo-without-tags
  (concat
   "^\\* TODO Task Without Tags *\n" ; No tags, optional spaces
   "\\(?::PROPERTIES:\n" ":ID: +[^\n]+\n" ":END:\n\\)?$")
  "Pattern for TODO item without any tags.")

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

;; Test helper macros

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (mcp-server-lib-ert-with-server :tools t :resources t ,@body)
       (org-mcp-disable))))

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
            (todoState . ,todoState)
            (tags . ,tags)
            (body . ,body)
            (parentUri . ,parentUri)
            (afterUri . ,afterUri)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-add-todo" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request)))
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
             (currentState . ,current-state)
             (newState . ,new-state))))
         (response (mcp-server-lib-process-jsonrpc-parsed request))
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
            (currentTitle . ,current-title)
            (newTitle . ,new-title)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-rename-headline" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request))
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
            (todoState . ,todoState)
            (tags . ,tags)
            (body . ,body)
            (parentUri . ,parentUri)
            (afterUri . ,afterUri)))
         (request
          (mcp-server-lib-create-tools-call-request
           "org-add-todo" 1 params))
         (response (mcp-server-lib-process-jsonrpc-parsed request))
         (result (mcp-server-lib-ert-process-tool-response response)))
    ;; If we get here, the tool succeeded when we expected failure
    (error "Expected error but got success: %s" result)))

(defun org-mcp-test--update-and-verify-todo
    (resource-uri
     old-state new-state &optional test-file expected-content)
  "Update TODO state and verify the result via MCP JSON-RPC.
RESOURCE-URI is the URI to update.
OLD-STATE is the current TODO state to update from.
NEW-STATE is the new TODO state to update to.
TEST-FILE if provided, verify file content after update.
EXPECTED-CONTENT if provided with TEST-FILE, verify file contains this
exact content."
  (let* ((params
          `((uri . ,resource-uri)
            (currentState . ,old-state)
            (newState . ,new-state)))
         (result-text
          (mcp-server-lib-ert-call-tool
           "org-update-todo-state" params))
         (result (json-read-from-string result-text)))
    (should (= (length result) 4))
    (should (equal (alist-get 'success result) t))
    (should (equal (alist-get 'previousState result) old-state))
    (should (equal (alist-get 'newState result) new-state))
    (should (stringp (alist-get 'uri result)))
    (should (string-prefix-p "org-id://" (alist-get 'uri result)))
    ;; Verify file content if test-file provided
    (when test-file
      (when expected-content
        (with-temp-buffer
          (insert-file-contents test-file)
          (let ((buffer-content (buffer-string)))
            ;; Build a regex that matches the expected content
            ;; The expected-content should be a headline like "* IN-PROGRESS Task One"
            ;; We need to match: headline, optional PROPERTIES drawer with ID, then rest of content
            (let*
                ((expected-regex
                  (concat
                   ;; Match start of buffer
                   "\\`"
                   ;; Match the headline exactly as provided
                   (regexp-quote expected-content)
                   ;; Optional newline and PROPERTIES drawer with ID
                   "\\(?:\n:PROPERTIES:\n:ID: +[A-Fa-f0-9-]+\n:END:\\)?"
                   ;; Match any remaining content (body text, etc.) to end of buffer
                   "\\(?:.\\|\n\\)*"
                   ;; Match end of buffer
                   "\\'")))
              (should
               (string-match expected-regex buffer-content)))))))
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
  ;; Check file content if pattern provided
  (when expected-pattern
    (with-temp-buffer
      (insert-file-contents test-file)
      (should (string-match-p expected-pattern (buffer-string))))))

(defun org-mcp-test--assert-error-and-file (test-file error-form)
  "Assert that ERROR-FORM throws an error and TEST-FILE remains unchanged.
ERROR-FORM should be a form that is expected to signal an error.
The file content is saved before the error test and verified to be
unchanged after."
  ;; Save original content before test
  (let ((original-content
         (with-temp-buffer
           (insert-file-contents test-file)
           (buffer-string))))
    (should-error (eval error-form) :type 'mcp-server-lib-tool-error)
    ;; Verify file has not changed
    (with-temp-buffer
      (insert-file-contents test-file)
      (should (string= (buffer-string) original-content)))))

(ert-deftest org-mcp-test-tool-get-todo-config-empty ()
  "Test org-get-todo-config with empty `org-todo-keywords'."
  (org-mcp-test--with-config nil
    (should (assoc 'sequences result))
    (should (assoc 'semantics result))
    (should (equal sequences []))
    (should (equal semantics []))))

(ert-deftest org-mcp-test-tool-get-todo-config-default ()
  "Test org-get-todo-config with default `org-todo-keywords'."
  (org-mcp-test--with-config '((sequence "TODO" "DONE"))
    (should (= (length sequences) 1))
    (org-mcp-test--check-sequence
     (aref sequences 0) "sequence" ["TODO" "|" "DONE"])
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
        (should (= (length result) 5))
        (should
         (equal (alist-get 'org-use-tag-inheritance result) "t"))
        (should
         (equal
          (alist-get
           'org-tags-exclude-from-inheritance result)
          "nil"))
        (should
         (equal (alist-get 'org-tags-sort-function result) "nil"))
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
        (org-tags-sort-function nil)
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
        (org-tags-sort-function nil)
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
        (org-tags-sort-function nil)
        (org-use-tag-inheritance '("work")))
    (org-mcp-test--with-enabled
      (let ((result (org-mcp-test--call-get-tag-config)))
        (should
         (equal
          (alist-get 'org-use-tag-inheritance result)
          "(\"work\")"))))))

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
         ,invalid-title "TODO" nil nil ,parent-uri
         nil)))))

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
                   (currentTitle . ,headline-title)
                   (newTitle . ,new-title))))
               (response
                (mcp-server-lib-process-jsonrpc-parsed request)))
          (should-error
           (mcp-server-lib-ert-process-tool-response response)
           :type 'mcp-server-lib-tool-error))
        ;; Verify that file wasn't changed
        (with-temp-buffer
          (insert-file-contents test-file)
          (let ((content (buffer-string)))
            ;; Headline should be unchanged
            (should
             (string-match-p
              (format "^\\* %s$"
                      (regexp-quote headline-title))
              content))))))))

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
                  (mcp-server-lib-process-jsonrpc request))
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
          (let* ((uri (format "org://%s" forbidden-file))
                 (request
                  (mcp-server-lib-create-resources-read-request uri))
                 (response-json
                  (mcp-server-lib-process-jsonrpc request))
                 (response
                  (json-parse-string response-json
                                     :object-type 'alist)))
            ;; Should get an error response
            (mcp-server-lib-ert-check-error-object
             response mcp-server-lib-jsonrpc-error-invalid-params
             (format "File not in allowed list: %s"
                     forbidden-file))))))))

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
          (let* ((uri
                  (format "org-headline://%s#Nonexistent" test-file))
                 (request
                  (mcp-server-lib-create-resources-read-request uri))
                 (response-json
                  (mcp-server-lib-process-jsonrpc request))
                 (response
                  (json-parse-string response-json
                                     :object-type 'alist)))
            ;; Should get an error response
            (mcp-server-lib-ert-check-error-object
             response
             mcp-server-lib-jsonrpc-error-invalid-params
             "Headline not found: Nonexistent")))))))

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
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-locations-file nil)) ; Prevent saving ID locations
        ;; Manually register the ID location
        (setq org-id-locations (make-hash-table :test 'equal))
        (puthash
         "12345678-abcd-efgh-ijkl-1234567890ab"
         test-file
         org-id-locations)
        (org-mcp-test--with-enabled
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
          (let* ((uri "org-id://nonexistent-id-12345")
                 (request
                  (mcp-server-lib-create-resources-read-request uri))
                 (response-json
                  (mcp-server-lib-process-jsonrpc request))
                 (response
                  (json-parse-string response-json
                                     :object-type 'alist)))
            ;; Should get an error response
            (mcp-server-lib-ert-check-error-object
             response
             mcp-server-lib-jsonrpc-error-invalid-params
             "ID not found: nonexistent-id-12345")))))))

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
      (let ((org-mcp-allowed-files (list allowed-file))
            (org-id-locations-file nil)) ; Prevent saving ID locations
        ;; Manually register the ID location
        (setq org-id-locations (make-hash-table :test 'equal))
        (puthash "test-id-789" other-file org-id-locations)
        (org-mcp-test--with-enabled
          (let* ((uri "org-id://test-id-789")
                 (request
                  (mcp-server-lib-create-resources-read-request uri))
                 (response-json
                  (mcp-server-lib-process-jsonrpc request))
                 (response
                  (json-parse-string response-json
                                     :object-type 'alist)))
            ;; Should get an error for file not allowed
            (mcp-server-lib-ert-check-error-object
             response
             mcp-server-lib-jsonrpc-error-invalid-params
             (format "File not in allowed list: %s" other-file))))))))

(ert-deftest org-mcp-test-update-todo-state-success ()
  "Test successful TODO state update."
  (let ((test-content "* TODO Task One\nTask description."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Update TODO to IN-PROGRESS
          (let ((resource-uri
                 (format "org-headline://%s#Task%%20One" test-file)))
            (org-mcp-test--update-and-verify-todo
             resource-uri "TODO" "IN-PROGRESS"
             test-file "* IN-PROGRESS Task One")))))))

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
                   test-file "* IN-PROGRESS Task One")
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
  (let ((test-content
         "* TODO Task with ID
:PROPERTIES:
:ID:       550e8400-e29b-41d4-a716-446655440000
:END:
This is a task with an ID property."))
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords
             '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
            (org-id-track-globally t)
            (org-id-locations-file
             (make-temp-file "org-id-locations")))
        ;; Initialize org-id system with the file
        (with-temp-buffer
          (insert-file-contents test-file)
          (org-mode)
          (org-id-update-id-locations (list test-file)))
        (org-mcp-test--with-enabled
          ;; Update using ID-based URI
          (let* ((resource-uri
                  "org-id://550e8400-e29b-41d4-a716-446655440000")
                 (result
                  (org-mcp-test--update-and-verify-todo
                   resource-uri "TODO" "IN-PROGRESS"
                   test-file "* IN-PROGRESS Task with ID")))
            ;; Verify the returned URI is the same ID
            (should
             (equal
              (alist-get 'uri result)
              "org-id://550e8400-e29b-41d4-a716-446655440000"))
            ;; Verify file content with a single regex matching the whole buffer
            (with-temp-buffer
              (insert-file-contents test-file)
              (let*
                  ((content (buffer-string))
                   (expected-regex
                    (concat
                     ;; Match start of buffer
                     "\\`"
                     ;; Match the updated headline
                     "\\* IN-PROGRESS Task with ID\n"
                     ;; Match the PROPERTIES drawer with the specific ID
                     ":PROPERTIES:\n"
                     ":ID: +550e8400-e29b-41d4-a716-446655440000\n"
                     ":END:\n"
                     ;; Match the body text
                     "This is a task with an ID property\\."
                     ;; Match end of buffer
                     "\\'")))
                (should (string-match expected-regex content))))))))))

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
        "\\(?::PROPERTIES:\n"
        ":ID: +[^\n]+\n"
        ":END:\n\\)?$")))))

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
        ;; Need extra validation for headers
        (with-temp-buffer
          (insert-file-contents test-file)
          (let ((content (buffer-string)))
            ;; Check that headers are still at the top
            (should (string-match-p "^#\\+TITLE:" content))
            ;; Check that the new task comes after headers but before existing
            (should
             (string-match-p
              (concat
               "^#\\+TITLE: My Org Document\n"
               "#\\+AUTHOR: Test Author\n"
               "#\\+DATE: 2024-01-01\n"
               "#\\+OPTIONS: toc:nil\n"
               "\n"
               "\\* TODO New Top Task +.*:urgent:\n"
               "\\(?::PROPERTIES:\n"
               ":ID: +[^\n]+\n"
               ":END:\n\\)?"
               "\n?"
               "\\* Existing Task\n"
               "Some content here\\.")
              content))))))))

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
         ,parent-uri
         nil)))))

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

(ert-deftest org-mcp-test-add-todo-tag-reject-invalid-with-alist ()
  "Test that tags not in `org-tag-alist' are rejected."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let ((parent-uri (format "org-headline://%s#" test-file)))
      ;; Should reject tags not in org-tag-alist
      (org-mcp-test--assert-error-and-file
       test-file
       `(org-mcp-test--call-add-todo-expecting-error
         "Task" "TODO" '("invalid") nil ,parent-uri
         nil)))))

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
          "\\(?::PROPERTIES:\n"
          ":ID: +[^\n]+\n"
          ":END:\n\\)?$"))))))

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
            "\\(?::PROPERTIES:\n"
            ":ID: +[^\n]+\n"
            ":END:\n\\)?$")))))))

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
           "Task" "TODO" '("invalid-tag!") nil ,parent-uri
           nil))
        (org-mcp-test--assert-error-and-file
         test-file
         `(org-mcp-test--call-add-todo-expecting-error
           "Task" "TODO" '("tag-with-dash") nil ,parent-uri
           nil))
        (org-mcp-test--assert-error-and-file
         test-file
         `(org-mcp-test--call-add-todo-expecting-error
           "Task" "TODO" '("tag#hash") nil ,parent-uri
           nil))))))

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

(ert-deftest org-mcp-test-add-todo-with-body ()
  "Test adding TODO with body text."
  (org-mcp-test--with-add-todo-setup test-file
      org-mcp-test--content-empty
    (let* ((parent-uri (format "org-headline://%s#" test-file))
           (body-text org-mcp-test--body-text-multiline)
           (result
            (org-mcp-test--call-add-todo
             "Task with Body" "TODO" '("work") body-text parent-uri
             nil)))
      ;; Check result structure - should have exactly 4 fields
      (org-mcp-test--check-add-todo-result
       result
       "Task with Body"
       (file-name-nondirectory test-file)
       test-file
       org-mcp-test--regex-todo-with-body))))

(ert-deftest org-mcp-test-add-todo-after-sibling ()
  "Test adding TODO after a specific sibling."
  (let ((initial-content org-mcp-test--content-siblings-after-test))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-tag-alist '("work"))
            (org-id-locations-file nil))
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
          ;; Register the ID location
          (setq org-id-locations (make-hash-table :test 'equal))
          (puthash second-id test-file org-id-locations)

          (org-mcp-test--with-enabled
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
  (let ((initial-content org-mcp-test--content-parent-child-siblings))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
            (org-tag-alist '("work"))
            (org-id-locations-file nil))
        ;; Add ID to "Other Child" - not a child of Parent Task
        (with-temp-buffer
          (set-visited-file-name test-file t)
          (insert-file-contents test-file)
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^\\*\\* Other Child")
          (org-id-get-create)
          (let ((other-id (org-id-get)))
            (write-region (point-min) (point-max) test-file)
            ;; Kill any buffer visiting the test file
            (let ((buf (find-buffer-visiting test-file)))
              (when buf
                (kill-buffer buf)))

            ;; Register the ID location manually
            (setq org-id-locations (make-hash-table :test 'equal))
            (puthash other-id test-file org-id-locations)

            (org-mcp-test--with-enabled
              (let* ((parent-uri
                      (format "org-headline://%s#Parent%%20Task"
                              test-file))
                     (after-uri (format "org-id://%s" other-id)))
                ;; Error: Other Child is not a child of Parent Task
                (should-error
                 (org-mcp-test--call-add-todo
                  "New Task" "TODO" '("work") nil parent-uri
                  after-uri)
                 :type 'mcp-server-lib-tool-error)
                ;; Verify file was NOT modified from its state with IDs
                (with-temp-buffer
                  (insert-file-contents test-file)
                  ;; Should still have the original structure with IDs added
                  (should
                   (string-match-p
                    (concat
                     "^\\* Parent Task\n"
                     "\\*\\* Child One\n"
                     "\\*\\* Child Two\n"
                     "\\* Other Parent\n"
                     "\\*\\* Other Child\n"
                     "\\(?::PROPERTIES:\n"
                     ":ID: +[^\n]+\n"
                     ":END:\n\\)?")
                    (buffer-string)))
                  ;; But should NOT have "New Task"
                  (should-not
                   (string-match-p
                    "New Task" (buffer-string))))))))))))

(ert-deftest org-mcp-test-add-todo-parent-id-uri ()
  "Test adding TODO with parent specified as org-id:// URI."
  (let ((initial-content org-mcp-test--content-parent-task-simple))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE")))
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
                "\\(?::PROPERTIES:\n"
                ":ID: +[^\n]+\n"
                ":END:\n\\)?"
                "Some parent content\\.\n\n?"
                "\\*\\* TODO Child via ID +:work:\n"
                "\\(?::PROPERTIES:\n"
                ":ID: +[^\n]+\n"
                ":END:\n\\)?\n?"
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
                    (currentTitle . "Original Task")
                    (newTitle . "Updated Task")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result structure
            (should (= (length result) 4))
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previousTitle result) "Original Task"))
            (should
             (equal (alist-get 'newTitle result) "Updated Task"))
            ;; Should return an org-id:// URI
            (should
             (string-match "^org-id://" (alist-get 'uri result)))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              (should
               (string-match-p
                "^\\* TODO Updated Task\n:PROPERTIES:\n:ID: +[A-F0-9-]+\n:END:\nTask description\\.$"
                (buffer-string))))))))))

(ert-deftest org-mcp-test-rename-headline-title-mismatch ()
  "Test that rename fails when current title doesn't match."
  (let ((initial-content "* TODO Actual Task\nTask description."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-todo-keywords '((sequence "TODO" "|" "DONE"))))
        (org-mcp-test--with-enabled
          ;; Try to rename with wrong current title
          (let* ((resource-uri
                  (format "org-headline://%s#Actual%%20Task"
                          test-file)))
            (should-error
             (org-mcp-test--call-rename-headline-expecting-error
              resource-uri "Wrong Title" "Updated Task")
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was not modified
            (with-temp-buffer
              (insert-file-contents test-file)
              (should
               (string= (buffer-string) initial-content)))))))))

(ert-deftest org-mcp-test-rename-headline-preserve-tags ()
  "Test that renaming preserves tags."
  (let ((initial-content
         "* TODO Task with Tags :work:urgent:\nTask description."))
    (org-mcp-test--with-temp-org-file test-file initial-content
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
                    (currentTitle . "Task with Tags")
                    (newTitle . "Renamed Task")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previousTitle result) "Task with Tags"))
            (should
             (equal (alist-get 'newTitle result) "Renamed Task"))
            ;; Verify file content - tags should be preserved
            (with-temp-buffer
              (insert-file-contents test-file)
              ;; org-edit-headline may add spaces for tag alignment
              (should
               (string-match-p
                "^\\* TODO Renamed Task[ \t]+:work:urgent:\n:PROPERTIES:\n:ID: +[A-F0-9-]+\n:END:\nTask description\\.$"
                (buffer-string))))))))))

(ert-deftest org-mcp-test-rename-headline-no-todo ()
  "Test renaming a regular headline without TODO state."
  (let ((initial-content "* Regular Headline\nSome content."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          ;; Rename the headline
          (let* ((resource-uri
                  (format "org-headline://%s#Regular%%20Headline"
                          test-file))
                 (params
                  `((uri . ,resource-uri)
                    (currentTitle . "Regular Headline")
                    (newTitle . "Updated Headline")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              (should
               (string-match-p
                "^\\* Updated Headline\n:PROPERTIES:\n:ID: +[A-F0-9-]+\n:END:\nSome content\\.$"
                (buffer-string))))))))))

(ert-deftest org-mcp-test-rename-headline-nested-path-navigation ()
  "Test correct headline path navigation in nested structures.
Verifies that the implementation correctly navigates nested headline
paths and only matches headlines at the appropriate hierarchy level."
  (let ((initial-content
         "* Parent One
** Other Heading
Some other content.
* Parent Two
** Another Heading
More content.
* Parent Three
** Child
This Child is under Parent Three, not Parent Two."))
    (org-mcp-test--with-temp-org-file test-file initial-content
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
                       (currentTitle . "Child")
                       (newTitle . "Renamed Child"))))
                   (response
                    (mcp-server-lib-process-jsonrpc-parsed request)))
              (should-error
               (mcp-server-lib-ert-process-tool-response response)
               :type 'mcp-server-lib-tool-error))))))))


(ert-deftest org-mcp-test-rename-headline-by-id ()
  "Test renaming a headline accessed by org-id URI."
  (let ((initial-content
         "* Task with ID
:PROPERTIES:
:ID:       550e8400-e29b-41d4-a716-446655440000
:END:
This is a task with an ID property."))
    (org-mcp-test--with-temp-org-file test-file initial-content
      (let ((org-mcp-allowed-files (list test-file))
            (org-id-track-globally t)
            (org-id-locations-file
             (make-temp-file "org-id-locations")))
        ;; Manually add the ID to org-id locations
        (org-id-update-id-locations (list test-file))
        (org-mcp-test--with-enabled
          ;; Rename using ID-based URI
          (let* ((resource-uri
                  "org-id://550e8400-e29b-41d4-a716-446655440000")
                 (params
                  `((uri . ,resource-uri)
                    (currentTitle . "Task with ID")
                    (newTitle . "Renamed Task with ID")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal (alist-get 'previousTitle result) "Task with ID"))
            (should
             (equal
              (alist-get 'newTitle result) "Renamed Task with ID"))
            ;; URI should remain ID-based (not converted to path-based)
            (should
             (equal
              (alist-get 'uri result)
              "org-id://550e8400-e29b-41d4-a716-446655440000"))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              (let ((content (buffer-string)))
                ;; Title should be renamed
                (should
                 (string-match-p
                  "^\\* Renamed Task with ID$" content))
                ;; ID should be unchanged
                (should
                 (string-match-p
                  ":ID:       550e8400-e29b-41d4-a716-446655440000"
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
                     (currentTitle . "Whatever")
                     (newTitle . "Should Fail"))))
                 (response
                  (mcp-server-lib-process-jsonrpc-parsed request)))
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
                    (currentTitle . "Project A/B Testing")
                    (newTitle . "Project A/B Experiments")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previousTitle result)
              "Project A/B Testing"))
            (should
             (equal
              (alist-get 'newTitle result) "Project A/B Experiments"))
            ;; Should return an org-id:// URI
            (should
             (string-match "^org-id://" (alist-get 'uri result)))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              ;; Check complete buffer with single regex
              (should
               (string-match-p
                "^\\* Project A/B Experiments\n:PROPERTIES:\n:ID: +[A-F0-9-]+\n:END:\nThis is a headline with a slash in it\\.\n\\* Other Task\nSome other content\\.$"
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
                    (currentTitle . "Parent/Child")
                    (newTitle . "Parent-Child Renamed")))
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
                    (currentTitle . "50% Complete")
                    (newTitle . "75% Complete")))
                 (result-text
                  (mcp-server-lib-ert-call-tool
                   "org-rename-headline" params))
                 (result (json-read-from-string result-text)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal (alist-get 'previousTitle result) "50% Complete"))
            (should
             (equal (alist-get 'newTitle result) "75% Complete"))
            ;; Should return an org-id:// URI
            (should
             (string-match "^org-id://" (alist-get 'uri result)))
            ;; Verify file content
            (with-temp-buffer
              (insert-file-contents test-file)
              ;; Check complete buffer with single regex
              (should
               (string-match-p
                "^\\* 75% Complete\n:PROPERTIES:\n:ID: +[A-F0-9-]+\n:END:\nThis task is half done\\.\n\\* Use %20 for spaces\nDocumentation about URL encoding\\.$"
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
                 ":PROPERTIES:\n"
                 ":ID: +[A-F0-9-]+\n" ; ID added
                 ":END:\n"
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
                    (currentTitle . "Headline Without ID")
                    (newTitle . "Renamed Headline")))
                 (request
                  (mcp-server-lib-create-tools-call-request
                   "org-rename-headline" 1 params))
                 (response
                  (mcp-server-lib-process-jsonrpc-parsed request))
                 (result
                  (mcp-server-lib-ert-process-tool-response
                   response)))
            ;; Check result
            (should (equal (alist-get 'success result) t))
            (should
             (equal
              (alist-get 'previousTitle result)
              "Headline Without ID"))
            (should
             (equal (alist-get 'newTitle result) "Renamed Headline"))
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
                    (currentTitle . "Target")
                    (newTitle . "Renamed Target")))
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

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
