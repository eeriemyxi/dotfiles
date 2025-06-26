;;; kdl-ts-mode.el --- Tree-sitter support for KDL -*- lexical-binding: t; -*-

;; Copyright (C) 2024 dataphract

;; Homepage   : https://github.com/dataphract/kdl-ts-mode
;; Version    : 0.1
;; Author     : dataphract <dataphract@gmail.com>
;; Maintainer : dataphract <dataphract@gmail.com>
;; Created    : January 2024
;; Keywords   : kdl languages tree-sitter
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides a major mode for editing KDL files, powered by tree-sitter.

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

(defcustom kdl-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `kdl-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'kdl)

(defvar kdl-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?=  ".")
    (modify-syntax-entry ?/  ". 124")
    (modify-syntax-entry ?*  ". 23b")
    (modify-syntax-entry ?\n ">")
    table)
  "Syntax table for `kdl-ts-mode'.")

(defvar kdl-ts-mode--indent-rules
  '((kdl
     ((parent-is "source_file") column-0 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((parent-is "node_children") parent-bol kdl-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `kdl-ts-mode'.")

;; Syntax highlighting
(defvar kdl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'kdl
   :feature 'bracket
   '((["(" ")" "{" "}"]) @font-lock-bracket-face)

   :language 'kdl
   :feature 'comment
   '((single_line_comment) @font-lock-comment-face
     (multi_line_comment) @font-lock-comment-face)

   :language 'kdl
   :feature 'constant
   '("null" @font-lock-constant-face
     (boolean) @font-lock-constant-face)

   :language 'kdl
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'kdl
   :feature 'type
   '((type) @font-lock-type-face)

   :language 'kdl
   :feature 'string
   :override t
   '((string) @font-lock-string-face)

   :language 'kdl
   :feature 'escape-sequence
   :override t
   '((escape) @font-lock-escape-face)

   :language 'kdl
   :feature 'node
   :override t
   '((node (identifier) @font-lock-function-call-face))

   :language 'kdl
   :feature 'property
   :override t
   '((prop (identifier) @font-lock-property-use-face))

   :language 'kdl
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)

   :language 'kdl
   :feature 'comment
   :override t
   '((node (node_comment)) @font-lock-comment-face
     (node (node_field (node_field_comment)) @font-lock-comment-face)
     (node_children (node_children_comment)) @font-lock-comment-face))


  "Tree-sitter font-lock settings for `kdl-ts-mode'.")

;;;###autoload
(define-derived-mode kdl-ts-mode prog-mode "KDL"
  "Major mode for editing KDL, powered by tree-sitter."
  :group 'kdl
  :syntax-table kdl-ts-mode--syntax-table

  (when (treesit-ready-p 'kdl)
    (treesit-parser-create 'kdl)

    (setq-local comment-start "//")
    (setq-local comment-end "")

    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules kdl-ts-mode--indent-rules)

    (setq-local treesit-font-lock-settings kdl-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (string type)
                  (constant escape-sequence number node property)
                  (bracket
                                        ;delimiter
                   error
                                        ;misc-punctuation
                   )))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'kdl)
    (add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode)))

(provide 'kdl-ts-mode)

;;; kdl-ts-mode.el ends here
