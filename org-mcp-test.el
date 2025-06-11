;;; org-mcp-test.el --- Tests for org-mcp -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-mcp package.

;;; Code:

(require 'ert)
(require 'org-mcp)

(defmacro org-mcp-test--with-enabled (&rest body)
  "Run BODY with org-mcp enabled, ensuring cleanup."
  (declare (indent defun) (debug t))
  `(progn
     (org-mcp-enable)
     (unwind-protect
         (progn
           ,@body)
       (org-mcp-disable))))

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

(provide 'org-mcp-test)
;;; org-mcp-test.el ends here
