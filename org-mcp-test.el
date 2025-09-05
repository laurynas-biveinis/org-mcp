;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

(require 'ert)
(require 'org-mcp)
(require 'mcp-server-lib-commands)
(require 'mcp-server-lib-ert)
(require 'json)

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (mcp-server-lib-ert-with-server :tools t :resources t ,@body)
       (org-mcp-disable))))

(defmacro org-mcp-test--with-temp-org-file (var content &rest body)
  "Create a temporary Org file, execute BODY, and ensure cleanup.
VAR is the variable to bind the temp file path to.
CONTENT is the initial content to write to the file."
  (declare (indent 2) (debug (symbolp form body)))
  `(let (,var)
     (unwind-protect
         (progn
           (setq ,var
                 (make-temp-file "org-mcp-test" nil ".org" ,content))
           ,@body)
       (when ,var
         (delete-file ,var)))))

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
     (let ((result (org-mcp--tool-get-todo-config)))
       (should (= (length result) 2))
       (let ((sequences (cdr (assoc 'sequences result)))
             (semantics (cdr (assoc 'semantics result))))
         ,@body))))

(defun org-mcp-test--verify-file-content (file-path expected-regexp)
  "Check that FILE-PATH contain text matching EXPECTED-REGEXP."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (should (re-search-forward expected-regexp nil t))))

(defun org-mcp-test--update-and-verify-todo
    (resource-uri
     current-state new-state expected-previous expected-new)
  "Update TODO state and verify the result.
RESOURCE-URI is the URI to update.
CURRENT-STATE is the current TODO state.
NEW-STATE is the new TODO state.
EXPECTED-PREVIOUS is the expected previous state in result.
EXPECTED-NEW is the expected new state in result."
  (let ((result
         (org-mcp--tool-update-todo-state
          resource-uri current-state new-state)))
    ;; Check result
    (should (equal (alist-get 'success result) t))
    (should
     (equal (alist-get 'previousState result) expected-previous))
    (should (equal (alist-get 'newState result) expected-new))
    result))

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
    (let ((result (org-mcp--tool-get-tag-config)))
      (should (= (length result) 5))
      (should (equal (alist-get 'org-use-tag-inheritance result) "t"))
      (should
       (equal
        (alist-get 'org-tags-exclude-from-inheritance result) "nil"))
      (should
       (equal (alist-get 'org-tags-sort-function result) "nil"))
      (should (equal (alist-get 'org-tag-alist result) "nil"))
      (should
       (equal (alist-get 'org-tag-persistent-alist result) "nil")))))

(ert-deftest org-mcp-test-tool-get-tag-config-simple ()
  "Test org-get-tag-config with simple tags."
  (let ((org-tag-alist '("work" "personal" "urgent"))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t)
        (org-tags-exclude-from-inheritance nil))
    (let ((result (org-mcp--tool-get-tag-config)))
      (should
       (equal
        (alist-get 'org-tag-alist result)
        "(\"work\" \"personal\" \"urgent\")"))
      (should
       (equal (alist-get 'org-tag-persistent-alist result) "nil"))
      (should
       (equal (alist-get 'org-use-tag-inheritance result) "t")))))

(ert-deftest org-mcp-test-tool-get-tag-config-with-keys ()
  "Test org-get-tag-config with fast selection keys."
  (let ((org-tag-alist
         '(("work" . ?w) ("personal" . ?p) "urgent" ("@home" . ?h)))
        (org-tag-persistent-alist nil)
        (org-use-tag-inheritance t))
    (let ((result (org-mcp--tool-get-tag-config)))
      (should
       (equal
        (alist-get 'org-tag-alist result)
        "((\"work\" . 119) (\"personal\" . 112) \"urgent\" (\"@home\" . 104))")))))

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
    (let ((result (org-mcp--tool-get-tag-config)))
      ;; Just check that the literal string is returned correctly
      (should (stringp (alist-get 'org-tag-alist result)))
      (should
       (string-match-p
        ":startgroup" (alist-get 'org-tag-alist result)))
      (should
       (string-match-p ":endgroup" (alist-get 'org-tag-alist result)))
      (should
       (string-match-p
        ":startgrouptag" (alist-get 'org-tag-alist result)))
      (should
       (string-match-p
        ":endgrouptag" (alist-get 'org-tag-alist result))))))

(ert-deftest org-mcp-test-tool-get-tag-config-persistent ()
  "Test org-get-tag-config with persistent tags."
  (let ((org-tag-alist '(("work" . ?w)))
        (org-tag-persistent-alist '(("important" . ?i) "recurring"))
        (org-tags-exclude-from-inheritance nil))
    (let ((result (org-mcp--tool-get-tag-config)))
      (should
       (equal (alist-get 'org-tag-alist result) "((\"work\" . 119))"))
      (should
       (equal
        (alist-get 'org-tag-persistent-alist result)
        "((\"important\" . 105) \"recurring\")")))))

(ert-deftest org-mcp-test-tool-get-tag-config-inheritance ()
  "Test org-get-tag-config with different inheritance settings."
  (let ((org-tag-alist '("work" "personal"))
        (org-tags-exclude-from-inheritance nil)
        (org-tag-persistent-alist nil)
        (org-tags-sort-function nil))
    ;; Test with inheritance enabled
    (let ((org-use-tag-inheritance t))
      (let ((result (org-mcp--tool-get-tag-config)))
        (should
         (equal (alist-get 'org-use-tag-inheritance result) "t"))))
    ;; Test with inheritance disabled
    (let ((org-use-tag-inheritance nil))
      (let ((result (org-mcp--tool-get-tag-config)))
        (should
         (equal (alist-get 'org-use-tag-inheritance result) "nil"))))
    ;; Test with selective inheritance (list)
    (let ((org-use-tag-inheritance '("work")))
      (let ((result (org-mcp--tool-get-tag-config)))
        (should
         (equal
          (alist-get 'org-use-tag-inheritance result)
          "(\"work\")"))))))


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
    (org-mcp-test--with-temp-org-file test-file test-content
      (let ((org-mcp-allowed-files (list test-file)))
        (org-mcp-test--with-enabled
          (let* ((basename (file-name-nondirectory test-file))
                 (uri (format "org://%s" basename)))
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
          (let* ((basename (file-name-nondirectory test-file))
                 (uri (format "org-outline://%s" basename))
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
          (let* ((basename (file-name-nondirectory forbidden-file))
                 (uri (format "org://%s" basename))
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
             (format "File not in allowed list: %s" basename))))))))

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
          (let* ((basename (file-name-nondirectory test-file))
                 (uri
                  (format "org-headline://%s/First%%20Section"
                          basename)))
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
          (let* ((basename (file-name-nondirectory test-file))
                 (uri
                  (format (concat
                           "org-headline://%s/"
                           "First%%20Section/Subsection%%201.1")
                          basename)))
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
          (let* ((basename (file-name-nondirectory test-file))
                 (uri
                  (format "org-headline://%s/Nonexistent" basename))
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
             response mcp-server-lib-jsonrpc-error-invalid-params
             (format "File not in allowed list: %s"
                     (file-name-nondirectory other-file)))))))))

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
                 (format "org-headline://%s/Task%%20One"
                         (file-name-nondirectory test-file))))
            (org-mcp-test--update-and-verify-todo
             resource-uri "TODO" "IN-PROGRESS" "TODO" "IN-PROGRESS")
            ;; Verify file was actually updated
            (org-mcp-test--verify-file-content
             test-file "^\\* IN-PROGRESS Task One")))))))

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
                 (format "org-headline://%s/Task%%20One"
                         (file-name-nondirectory test-file))))
            (should-error
             (org-mcp--tool-update-todo-state
              resource-uri "IN-PROGRESS" "DONE") ; Wrong state
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was NOT updated
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task One")))))))

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
                 (format "org-headline://%s/Task%%20One"
                         (file-name-nondirectory test-file))))
            (should-error
             (org-mcp--tool-update-todo-state
              resource-uri "TODO" "") ; Empty state should fail
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was NOT updated
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task One")))))))

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
                 (format "org-headline://%s/Task%%20One"
                         (file-name-nondirectory test-file))))
            (should-error
             (org-mcp--tool-update-todo-state
              resource-uri "TODO" "INVALID-STATE") ; Invalid state
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was NOT updated
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task One")))))))

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
                       (format "org-headline://%s/Task%%20One"
                               (file-name-nondirectory test-file))))
                  (org-mcp-test--update-and-verify-todo
                   resource-uri
                   "TODO"
                   "IN-PROGRESS"
                   "TODO"
                   "IN-PROGRESS")
                  ;; Verify file was actually updated on disk
                  (org-mcp-test--verify-file-content
                   test-file "^\\* IN-PROGRESS Task One")
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
                         (format "org-headline://%s/Task%%20One"
                                 (file-name-nondirectory test-file))))
                    (should-error
                     (org-mcp--tool-update-todo-state
                      resource-uri "TODO" "IN-PROGRESS")
                     :type 'mcp-server-lib-tool-error)
                    ;; Verify file was NOT modified
                    (org-mcp-test--verify-file-content
                     test-file "^\\* TODO Task One")
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
             (org-mcp--tool-update-todo-state
              resource-uri "TODO" "IN-PROGRESS")
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was NOT modified
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task One")))))))

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
                 (format "org-headline://%s/Nonexistent%%20Task"
                         (file-name-nondirectory test-file))))
            (should-error
             (org-mcp--tool-update-todo-state
              resource-uri "TODO" "IN-PROGRESS")
             :type 'mcp-server-lib-tool-error)
            ;; Verify file was NOT modified
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task One")
            (org-mcp-test--verify-file-content
             test-file "^\\* TODO Task Two")))))))

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
