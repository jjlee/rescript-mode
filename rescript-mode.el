;;; rescript-mode.el --- A major mode for editing ReScript -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Version: 0.1.0
;; Author: John Lee
;; Url: https://github.com/jjlee/rescript-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:
;; This project provides useful functions and helpers for developing code
;; using the ReScript programming language

;;; Code:

(defgroup rescript nil
  "Support for ReScript code."
  :link '(url-link "https://rescript-lang.org/")
  :group 'languages)

(require 'rescript-indent)

(defconst rescript-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

;; Syntax definitions and helpers
(defvar rescript-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "_"  table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defcustom rescript-mode-hook nil
  "Hook called by `rescript-mode'."
  :type 'hook
  :group 'rescript)

;; in rescript-vscode grammar file, what are they?
;; import
;; library
;; export

;; Font-locking definitions and helpers
(defconst rescript-mode-keywords
  '("and" "as" "assert"
    "begin"
    "catch" "finally" "raise"
    "class" "constraint"
    "do" "done" "downto"
    "else"
    "exception" "external"
    "for" "fun" "functor"
    "if" "in" "include" "inherit"
    "lazy" "let"
    "module" "mutable"
    "new" "nonrec"
    "object" "of" "open" "or"
    "pri" "private" "pub"
    "rec"
    "switch"
    "then" "to" "try" "type"
    "val" "virtual"
    "while"))


(defconst rescript-mode-consts
  '("true" "false"))

(defconst rescript-special-types
  '("int" "float" "string" "char"
    "bool" "unit" "list" "array" "exn"
    "option" "ref"))

(defconst rescript-camel-case
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(eval-and-compile
  (defconst rescript--char-literal-rx
    (rx (seq (group "'")
             (or (seq "\\" anything)
                 (not (any "'\\")))
             (group "'")))))

(defun rescript-re-word (inner)
  "Build a word regexp given INNER."
  (concat "\\<" inner "\\>"))

(defun rescript-re-grab (inner)
  "Build a grab regexp given INNER."
  (concat "\\(" inner "\\)"))

(defun rescript-regexp-opt-symbols (words)
  "Like `(regexp-opt words 'symbols)`, but will work on Emacs 23.
See rust-mode PR #42.
Argument WORDS argument to pass to `regexp-opt`."
  (concat "\\_<" (regexp-opt words t) "\\_>"))

;;; Syntax highlighting for Rescript
(defvar rescript-font-lock-keywords
  `((,(rescript-regexp-opt-symbols rescript-mode-keywords) . font-lock-keyword-face)
    (,(rescript-regexp-opt-symbols rescript-special-types) . font-lock-builtin-face)
    (,(rescript-regexp-opt-symbols rescript-mode-consts) . font-lock-constant-face)

    (,rescript-camel-case 1 font-lock-type-face)

    ;; Field names like `foo:`, highlight excluding the :
    (,(concat (rescript-re-grab rescript-re-ident) ":[^:]") 1 font-lock-variable-name-face)
    ;; Module names like `foo::`, highlight including the ::
    (,(rescript-re-grab (concat rescript-re-ident "::")) 1 font-lock-type-face)
    ;; Name punned labeled args like ::foo
    (,(concat "[[:space:]]+" (rescript-re-grab (concat "::" rescript-re-ident))) 1 font-lock-type-face)

    ;; TODO jsx attribs?
    (,
     (concat "<[/]?" (rescript-re-grab rescript-re-ident) "[^>]*" ">")
     1 font-lock-type-face)))

(defvar rescript-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\C-c\C-a" #'rescript-mode-find-alternate-file)
    map))

;;;###autoload
(define-derived-mode rescript-mode prog-mode "ReScript"
  "Major mode for ReScript code.

\\{rescript-mode-map}"
  :keymap rescript-mode-map

  ;; Indentation
  (setq-local indent-line-function 'rescript-indent-line)
  (setq-local comment-start "/* ")
  (setq-local comment-end   " */")
  (setq-local indent-tabs-mode nil)
  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local paragraph-start
              (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local require-final-newline t)
  (setq-local normal-auto-fill-function nil)
  (setq-local comment-multi-line t)
  ;; Fonts
  (setq-local font-lock-defaults '(rescript-font-lock-keywords))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.resi?$" . rescript-mode))

(defun rescript-mode-reload ()
  "Reload Rescript mode."
  (interactive)
  (unload-feature 'rescript-mode)
  (require 'rescript-mode)
  (rescript-mode))

(provide 'rescript-mode)

;;; rescript-mode.el ends here
