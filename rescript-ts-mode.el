;;; rescript-ts-mode.el --- Tree-sitter based major mode for ReScript -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Manuel Vázquez Acosta

;; Author: Manuel Vázquez Acosta
;; Keywords: languages, rescript
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; A tree-sitter based major mode for ReScript.  Uses the rescript
;; tree-sitter grammar maintained by the ReScript team, which correctly
;; handles regex literals, template strings, and other syntax that is
;; problematic for regexp-based font-locking.

;;; Code:

(require 'treesit)
(require 'prog-mode)

(defgroup rescript-ts nil
  "Major mode for editing ReScript with tree-sitter."
  :prefix "rescript-ts-"
  :group 'languages)

(defcustom rescript-ts-indent-offset 2
  "Number of spaces for each indentation step in `rescript-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'rescript-ts)

(defcustom rescript-ts-grammar-source-url
  "https://github.com/rescript/tree-sitter-rescript"
  "Git repository used by `rescript-ts-install-grammar'."
  :type 'string
  :group 'rescript-ts)

(defcustom rescript-ts-grammar-install-directory nil
  "Directory where `rescript-ts-install-grammar' installs the grammar.

When nil, install to the standard tree-sitter directory under
`user-emacs-directory'."
  :type '(choice (const :tag "Default tree-sitter directory" nil)
                 directory)
  :group 'rescript-ts)

(defcustom rescript-ts-prompt-to-install-grammar t
  "Whether `rescript-ts-mode' should offer to install its grammar.

When non-nil, calling `rescript-ts-mode' interactively prompts to install
the ReScript tree-sitter grammar if it is missing."
  :type 'boolean
  :group 'rescript-ts)

(defvar rescript-ts--indent-rules
  `((rescript
     ((parent-is "source_file") column-0 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((parent-is "block") parent-bol rescript-ts-indent-offset)
     ((parent-is "arguments") parent-bol rescript-ts-indent-offset)
     ((parent-is "formal_parameters") parent-bol rescript-ts-indent-offset)
     ((parent-is "switch_expression") parent-bol rescript-ts-indent-offset)
     ((parent-is "switch_match") parent-bol rescript-ts-indent-offset)
     ((parent-is "sequence_expression") parent-bol 0)
     ((parent-is "record") parent-bol rescript-ts-indent-offset)
     ((parent-is "record_type") parent-bol rescript-ts-indent-offset)
     ((parent-is "array") parent-bol rescript-ts-indent-offset)
     ((parent-is "list") parent-bol rescript-ts-indent-offset)
     ((parent-is "tuple") parent-bol rescript-ts-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol rescript-ts-indent-offset)
     ((parent-is "module_binding") parent-bol rescript-ts-indent-offset)
     ((parent-is "variant_declaration") parent-bol rescript-ts-indent-offset)
     ((parent-is "if_expression") parent-bol rescript-ts-indent-offset)
     ((parent-is "else_clause") parent-bol rescript-ts-indent-offset)
     ((parent-is "try_expression") parent-bol rescript-ts-indent-offset)
     ((parent-is "pipe_expression") parent-bol rescript-ts-indent-offset)
     ((parent-is "ternary_expression") parent-bol rescript-ts-indent-offset)
     ((parent-is "jsx_element") parent-bol rescript-ts-indent-offset)
     ((parent-is "jsx_self_closing_element") parent-bol rescript-ts-indent-offset)
     ((parent-is "jsx_opening_element") parent-bol rescript-ts-indent-offset)
     ;; Fallback
     (no-node parent-bol 0)))
  "Tree-sitter indentation rules for ReScript.")

(defun rescript-ts--fontify-builtin-collection (node override start end &rest _)
  "Fontify collection builtins represented by NODE.

Highlight `list' and `dict' consistently in both type positions and
constructor forms like `list{}' and `dict{}'.  Respect OVERRIDE and only
fontify within START and END."
  (let* ((node-start (treesit-node-start node))
         (node-end (treesit-node-end node))
         (text (buffer-substring-no-properties node-start node-end))
         (limit (cond
                 ((string-prefix-p "list" text) (+ node-start 4))
                 ((string-prefix-p "dict" text) (+ node-start 4)))))
    (when limit
      (treesit-fontify-with-override
       (max node-start start)
       (min limit end)
       'font-lock-builtin-face
       override))))

(defvar rescript-ts--font-lock-settings
  (treesit-font-lock-rules
   ;; Comments
   :feature 'comment
   :language 'rescript
   '((comment) @font-lock-comment-face)

   ;; Strings
   :feature 'string
   :language 'rescript
   '((string) @font-lock-string-face
     (template_string) @font-lock-string-face
     (character) @font-lock-string-face
     (regex) @font-lock-string-face)

   ;; Extension expressions like %re("/regex/") -- the extension_identifier
   ;; gets keyword face, and the string inside keeps its string face from above.
   :feature 'extension
   :language 'rescript
   '((extension_expression
      (extension_identifier) @font-lock-preprocessor-face))

   ;; Builtin collection constructors and types
   :feature 'builtin
   :language 'rescript
   :override t
   '((type_identifier) @rescript-ts--fontify-builtin-collection
     (dict) @rescript-ts--fontify-builtin-collection
     (dict_pattern) @rescript-ts--fontify-builtin-collection
     (list) @rescript-ts--fontify-builtin-collection
     (list_pattern) @rescript-ts--fontify-builtin-collection)

   ;; Numbers
   :feature 'number
   :language 'rescript
   '((number) @font-lock-number-face)

   ;; Keywords
   :feature 'keyword
   :language 'rescript
   '(["let" "type" "module" "open" "include" "external"
      "if" "else" "switch" "when"
      "for" "in" "to" "downto" "while"
      "try" "catch" "as" "exception"
      "export"
      "private" "mutable" "rec" "and"
      "async" "await" "lazy" "assert"
      "constraint" "of" "unpack"
      ] @font-lock-keyword-face)

   ;; Constants
   :feature 'constant
   :language 'rescript
   '((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (unit) @font-lock-constant-face
     (polyvar) @font-lock-constant-face)

   ;; Types
   :feature 'type
   :language 'rescript
   '((type_identifier) @font-lock-type-face
     (module_identifier) @font-lock-type-face)

   ;; Functions
   :feature 'function
   :language 'rescript
   '((let_binding pattern: (value_identifier) @font-lock-function-name-face
                  body: (function))
     (call_expression
      function: (value_identifier) @font-lock-function-call-face)
     (call_expression
      function: (value_identifier_path
                 (value_identifier) @font-lock-function-call-face)))

   ;; Parameters
   :feature 'parameter
   :language 'rescript
   '((parameter (value_identifier) @font-lock-variable-name-face)
     (parameter
      (labeled_parameter
       (value_identifier) @font-lock-variable-name-face)))

   ;; Variant constructors
   :feature 'constructor
   :language 'rescript
   '((variant_identifier) @font-lock-type-face)

   ;; Decorators like @module, @scope, @send, etc.
   :feature 'decorator
   :language 'rescript
   '((decorator (decorator_identifier) @font-lock-preprocessor-face))

   ;; Property/field names
   :feature 'property
   :language 'rescript
   :override t
   '((record_type_field (property_identifier) @font-lock-variable-name-face)
     (record_field (property_identifier) @font-lock-variable-name-face))

   ;; Operators
   :feature 'operator
   :language 'rescript
   '((pipe_expression "->" @font-lock-operator-face))

   ;; Escape sequences in strings
   :feature 'escape-sequence
   :language 'rescript
   :override t
   '((escape_sequence) @font-lock-escape-face))
  "Tree-sitter font-lock settings for ReScript.")

(defvar rescript-ts--font-lock-feature-list
  '((comment string)
    (keyword type builtin constant number)
    (function parameter constructor decorator extension property)
    (operator escape-sequence))
  "Feature list for font-locking in `rescript-ts-mode'.
Each level adds more highlighting on top of the previous.")

(defun rescript-ts-install-grammar (&optional out-dir)
  "Install the ReScript tree-sitter grammar.

If OUT-DIR is non-nil, install the grammar there.  Otherwise install it
into `rescript-ts-grammar-install-directory' or Emacs' standard
`tree-sitter' directory when that variable is nil."
  (interactive
   (list
    (when current-prefix-arg
      (read-directory-name "Install ReScript grammar to: "))))
  (require 'treesit)
  (setf (alist-get 'rescript treesit-language-source-alist)
        rescript-ts-grammar-source-url)
  (treesit-install-language-grammar
   'rescript
   (or out-dir rescript-ts-grammar-install-directory)))

(defun rescript-ts-diagnose-grammar ()
  "Show diagnostic information about the ReScript tree-sitter grammar."
  (interactive)
  (require 'treesit)
  (message
   "rescript available=%S ready=%S extra-load-path=%S override=%S source=%S"
   (treesit-language-available-p 'rescript)
   (treesit-ready-p 'rescript t)
   treesit-extra-load-path
   treesit-load-name-override-list
   (alist-get 'rescript treesit-language-source-alist)))

(defun rescript-ts--ensure-grammar ()
  "Ensure the ReScript tree-sitter grammar is available."
  (or (treesit-ready-p 'rescript)
      (when (and rescript-ts-prompt-to-install-grammar
                 (called-interactively-p 'interactive)
                 (y-or-n-p
                  (concat
                   "ReScript tree-sitter grammar is missing. "
                   "Install it now? ")))
        (condition-case err
            (progn
              (rescript-ts-install-grammar)
              (treesit-ready-p 'rescript))
          (error
           (user-error
            "Failed to install ReScript tree-sitter grammar: %s"
            (error-message-string err)))))))

;;;###autoload
(define-derived-mode rescript-ts-mode prog-mode "ReScript"
  "Major mode for editing ReScript, powered by tree-sitter.

\\{rescript-ts-mode-map}"
  :group 'rescript-ts
  :syntax-table (let ((table (make-syntax-table)))
                  (modify-syntax-entry ?/ ". 124b" table)
                  (modify-syntax-entry ?* ". 23n" table)
                  (modify-syntax-entry ?\n "> b" table)
                  table)
  (unless (rescript-ts--ensure-grammar)
    (error
     (concat
      "Tree-sitter grammar for ReScript is not available. "
      "Run M-x rescript-ts-install-grammar to install it")))

  (treesit-parser-create 'rescript)

  ;; Font-lock
  (setq-local treesit-font-lock-settings rescript-ts--font-lock-settings)
  (setq-local treesit-font-lock-feature-list rescript-ts--font-lock-feature-list)

  ;; Indentation
  (setq-local treesit-simple-indent-rules rescript-ts--indent-rules)
  (setq-local indent-tabs-mode nil)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  (setq-local comment-multi-line t)

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              (rx (or "let_declaration"
                      "type_declaration"
                      "module_declaration"
                      "external_declaration"
                      "exception_declaration")))

  (setq-local require-final-newline t)

  (treesit-major-mode-setup))

;; NOTE: Not auto-enabled yet.
;; Enable manually with M-x rescript-ts-mode, or uncomment
;; the form below if you want to use it for all .res/.resi buffers.
;;
;; ;;;###autoload
;; (if (treesit-ready-p 'rescript t)
;;     (add-to-list 'auto-mode-alist '("\\.resi?\\'" . rescript-ts-mode)))

(provide 'rescript-ts-mode)
;;; rescript-ts-mode.el ends here
