;;; rescript-mode.el --- A major mode for editing ReScript -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
;; Copyright (C) 2021 John Lee <jjl@pobox.com>

;; Version: 0.1.0
;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dancol@dancol.org>
;;         John Lee <jjl@pobox.com>
;; Maintainer: John Lee <jjl@pobox.com>
;; Url: https://github.com/jjlee/rescript-mode
;; Keywords: languages, rescript
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This project provides useful functions and helpers for developing code
;; using the ReScript programming language
;;
;; The indentation code comes from js.el.  The rest is based on reason-mode.el.
;;
;; Exported names start with "rescript-"; private names start with
;; "rescript--".

;;; Code:

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(defgroup rescript nil
  "Support for ReScript code."
  :link '(url-link "https://rescript-lang.org/")
  :group 'languages)

(require 'rescript-indent)

(defconst rescript--re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

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

;; in rescript-vscode grammar file, what are they?
;; import
;; library
;; export

;; Font-locking definitions and helpers
(defconst rescript--keywords
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


(defconst rescript--consts
  '("true" "false"))

(defconst rescript--special-types
  '("int" "float" "string" "char"
    "bool" "unit" "list" "array" "exn"
    "option" "ref"))

(defconst rescript--camel-case
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(eval-and-compile
  (defconst rescript--char-literal-rx
    (rx (seq (group "'")
             (or (seq "\\" anything)
                 (not (any "'\\")))
             (group "'")))))

(defun rescript--re-grab (inner)
  "Build a grab regexp given INNER."
  (concat "\\(" inner "\\)"))

;;; Syntax highlighting for Rescript
(defvar rescript--font-lock-keywords
  `((,(regexp-opt rescript--keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt rescript--special-types 'symbols) . font-lock-builtin-face)
    (,(regexp-opt rescript--consts 'symbols) . font-lock-constant-face)

    (,rescript--camel-case 1 font-lock-type-face)

    ;; Field names like `foo:`, highlight excluding the :
    (,(concat (rescript--re-grab rescript--re-ident) ":[^:]") 1 font-lock-variable-name-face)
    ;; Module names like `foo::`, highlight including the ::
    (,(rescript--re-grab (concat rescript--re-ident "::")) 1 font-lock-type-face)
    ;; Name punned labeled args like ::foo
    (,(concat "[[:space:]]+" (rescript--re-grab (concat "::" rescript--re-ident))) 1 font-lock-type-face)

    ;; TODO jsx attribs?
    (,
     (concat "<[/]?" (rescript--re-grab rescript--re-ident) "[^>]*" ">")
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
  (setq-local indent-line-function #'rescript-indent-line)
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
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  ;; Fonts
  (setq-local font-lock-defaults '(rescript--font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.resi?\\'" . rescript-mode))

(provide 'rescript-mode)
;;; rescript-mode.el ends here
