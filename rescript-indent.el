;;; rescript-indent.el --- Indentation functions for ReScript -*-lexical-binding: t-*-

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.

;; Author: Karl Landstrom <karl.landstrom@brgeight.se>
;;         Daniel Colascione <dancol@dancol.org>
;; Maintainer: John Lee <jjl@pobox.com>
;; Version: 1
;; Date: 2021-04-10
;; Keywords: languages, rescript

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


;; Based heavily (in fact entirely, right now) on js.el from GNU Emacs
;; So there is a lot of code in here that makes little sense for ReScript, and
;; sometimes the indentation it supplies will be just plain wrong.

;; The goal for now is just to get vaguely sensible most of the time when you
;; hit return or tab.  To format your ReScript code properly, use bsc -format
;; (which you can access, for example, using lsp-mode's lsp-format-buffer).
;;
;; Exported names start with "rescript-"; private names start with
;; "rescript--".

;;; Code:

(require 'cc-mode)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))
(require 'newcomment)
(require 'imenu)
(require 'moz nil t)
(require 'json)
(require 'prog-mode)

(eval-when-compile
  (require 'cl-lib)
  (require 'ido))

(defvar inferior-moz-buffer)
(defvar moz-repl-name)
(defvar ido-cur-list)
(defvar electric-layout-rules)
(declare-function ido-mode "ido" (&optional arg))
(declare-function inferior-moz-process "ext:mozrepl" ())

;;; Constants

(defconst rescript--name-start-re "[[:alpha:]_$]"
  "Regexp matching the start of a JavaScript identifier, without grouping.")

(defconst rescript--stmt-delim-chars "^;{}?:")

(defconst rescript--name-re (concat rescript--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a JavaScript identifier, without grouping.")

(defconst rescript--objfield-re (concat rescript--name-re ":")
  "Regexp matching the start of a JavaScript object field.")

(defconst rescript--dotted-name-re
  (concat rescript--name-re "\\(?:\\." rescript--name-re "\\)*")
  "Regexp matching a dot-separated sequence of JavaScript names.")

(defconst rescript--cpp-name-re rescript--name-re
  "Regexp matching a C preprocessor name.")

(defconst rescript--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst rescript--plain-method-re
  (concat "^\\s-*?\\(" rescript--dotted-name-re "\\)\\.prototype"
          "\\.\\(" rescript--name-re "\\)\\s-*?=\\s-*?\\(function\\)\\_>")
  "Regexp matching an explicit JavaScript prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the `function' keyword.")

(defconst rescript--plain-class-re
  (concat "^\\s-*\\(" rescript--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching a JavaScript explicit prototype \"class\" declaration.
An example of this is \"Class.prototype = { method1: ...}\".")

;; var NewClass = BaseClass.extend(
(defconst rescript--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" rescript--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" rescript--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$"))

;; var NewClass = Class.create()
(defconst rescript--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" rescript--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()"))

(defconst rescript--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" rescript--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst rescript--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" rescript--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*("))

;; var NewClass = Class.create({
(defconst rescript--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" rescript--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" rescript--dotted-name-re "\\)\\s-*,\\s-*\\)?{?"))

;; Parent class name(s) (yes, multiple inheritance in JavaScript) are
;; matched with dedicated font-lock matchers
(defconst rescript--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" rescript--dotted-name-re "\\)"))

(defconst rescript--extrescript-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" rescript--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" rescript--dotted-name-re "\\)")
  "Regexp matching an ExtJS class declaration (style 1).")

(defconst rescript--extrescript-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" rescript--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" rescript--dotted-name-re "\\)")
  "Regexp matching an ExtJS class declaration (style 2).")

(defconst rescript--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" rescript--dotted-name-re "\\)")
  "Regexp matching a MochiKit class declaration.")

(defconst rescript--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst rescript--class-styles
  `((:name            "Plain"
     :class-decl      ,rescript--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       javascript)

    (:name            "MochiKit"
     :class-decl      ,rescript--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,rescript--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,rescript--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,rescript--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,rescript--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,rescript--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtJS (style 1)"
     :class-decl      ,rescript--extrescript-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "ExtJS (style 2)"
     :class-decl      ,rescript--extrescript-class-decl-re-2
     :contexts        (toplevel)
     :framework       extjs)

    (:name            "Merrill Press"
     :class-decl      ,rescript--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "List of JavaScript class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class.  Its first group must match the name of its class.  If there
is a parent class, the second group should match, and it should be
the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name.  Otherwise, multiple definitions
will create multiple top-level entries.  Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains
")

(defconst rescript--available-frameworks
  (cl-loop for style in rescript--class-styles
           for framework = (plist-get style :framework)
           unless (memq framework available-frameworks)
           collect framework into available-frameworks
           finally return available-frameworks)
  "List of available JavaScript frameworks symbols.")

(defconst rescript--function-heading-1-re
  (concat
   "^\\s-*function\\(?:\\s-\\|\\*\\)+\\(" rescript--name-re "\\)")
  "Regexp matching the start of a JavaScript function header.
Match group 1 is the name of the function.")

(defconst rescript--function-heading-2-re
  (concat
   "^\\s-*\\(" rescript--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst rescript--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" rescript--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the JavaScript form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst rescript--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" rescript--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun rescript--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst rescript--keyword-re
  (rescript--regexp-opt-symbol
   '("abstract" "async" "await" "break" "case" "catch" "class" "const"
     "continue" "debugger" "default" "delete" "do" "else"
     "enum" "export" "extends" "final" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "native" "new" "package"
     "private" "protected" "public" "return" "static"
     "super" "switch" "synchronized" "throw"
     "throws" "transient" "try" "typeof" "var" "void" "let"
     "yield" "volatile" "while" "with"))
  "Regexp matching any JavaScript keyword.")

(defconst rescript--basic-type-re
  (rescript--regexp-opt-symbol
   '("boolean" "byte" "char" "double" "float" "int" "long"
     "short" "void"))
  "Regular expression matching any predefined type in JavaScript.")

(defconst rescript--constant-re
  (rescript--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in JavaScript.")


(defconst rescript--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list rescript--function-heading-1-re 1 font-lock-function-name-face)
   (list rescript--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `rescript-mode'.")

(defconst rescript--font-lock-keywords-2
  (append rescript--font-lock-keywords-1
          (list (list rescript--keyword-re 1 font-lock-keyword-face)
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
                (cons rescript--basic-type-re font-lock-type-face)
                (cons rescript--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `rescript-mode'.")

;; rescript--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; rescript--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; rescript--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, rescript--pstate, is actually a list
;; of all rescript--pitem instances open after the marked character.
;;
;; The text property for b-end, rescript--pend, is simply the
;; rescript--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an rescript--pstate text property. Since no other
;; rescript--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; rescript--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at
;; subsequence parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(cl-defstruct (rescript--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `rescript--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `rescript--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst rescript--initial-pitem
  (make-rescript--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;;; User Customization

(defgroup rescript nil
  "Customization variables for JavaScript mode."
  :tag "JavaScript"
  :group 'languages)

(defcustom rescript-indent-level 4
  "Number of spaces for each indentation step in `rescript-mode'."
  :type 'integer
  :safe 'integerp
  :group 'rescript)

(defcustom rescript-expr-indent-offset 0
  "Number of additional spaces for indenting continued expressions.
The value must be no less than minus `rescript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'rescript)

(defcustom rescript-paren-indent-offset 0
  "Number of additional spaces for indenting expressions in parentheses.
The value must be no less than minus `rescript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'rescript
  :version "24.1")

(defcustom rescript-square-indent-offset 0
  "Number of additional spaces for indenting expressions in square braces.
The value must be no less than minus `rescript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'rescript
  :version "24.1")

(defcustom rescript-curly-indent-offset 0
  "Number of additional spaces for indenting expressions in curly braces.
The value must be no less than minus `rescript-indent-level'."
  :type 'integer
  :safe 'integerp
  :group 'rescript
  :version "24.1")

(defcustom rescript-switch-indent-offset 0
  "Number of additional spaces for indenting the contents of a switch block.
The value must not be negative."
  :type 'integer
  :safe 'integerp
  :group 'rescript
  :version "24.4")

(defcustom rescript-flat-functions nil
  "Treat nested functions as top-level functions in `rescript-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean
  :group 'rescript)

(defcustom rescript-indent-align-list-continuation t
  "Align continuation of non-empty ([{ lines in `rescript-mode'."
  :version "26.1"
  :type 'boolean
  :safe 'booleanp
  :group 'rescript)

(defcustom rescript-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `rescript-mode'."
  :type 'function
  :group 'rescript)

(defcustom rescript-enabled-frameworks rescript--available-frameworks
  "Frameworks recognized by `rescript-mode'.
To improve performance, you may turn off some frameworks you
seldom use, either globally or on a per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           rescript--available-frameworks))
  :group 'rescript)

(defcustom rescript-rescript-switch-tabs
  (and (memq system-type '(darwin)) t)
  "Whether `rescript-mode' should display tabs while selecting them.
This is useful only if the windowing system has a good mechanism
for preventing Firefox from stealing the keyboard focus."
  :type 'boolean
  :group 'rescript)

(defcustom rescript-rescript-tmpdir
  "~/.emacs.d/js/js"
  "Temporary directory used by `rescript-mode' to communicate with Mozilla.
This directory must be readable and writable by both Mozilla and Emacs."
  :type 'directory
  :group 'rescript)

(defcustom rescript-rescript-timeout 5
  "Reply timeout for executing commands in Mozilla via `rescript-mode'.
The value is given in seconds.  Increase this value if you are
getting timeout messages."
  :type 'integer
  :group 'rescript)

(defcustom rescript-indent-first-init nil
  "Non-nil means specially indent the first variable declaration's initializer.
Normally, the first declaration's initializer is unindented, and
subsequent declarations have their identifiers aligned with it:

  var o = {
      foo: 3
  };

  var o = {
      foo: 3
  },
      bar = 2;

If this option has the value t, indent the first declaration's
initializer by an additional level:

  var o = {
          foo: 3
      };

  var o = {
          foo: 3
      },
      bar = 2;

If this option has the value `dynamic', if there is only one declaration,
don't indent the first one's initializer; otherwise, indent it.

  var o = {
      foo: 3
  };

  var o = {
          foo: 3
      },
      bar = 2;"
  :version "25.1"
  :type '(choice (const nil) (const t) (const dynamic))
  :safe 'symbolp
  :group 'rescript)

(defcustom rescript-chain-indent nil
  "Use \"chained\" indentation.
Chained indentation applies when the current line starts with \".\".
If the previous expression also contains a \".\" at the same level,
then the \".\"s will be lined up:

  let x = svg.mumble()
             .chained;
"
  :version "26.1"
  :type 'boolean
  :safe 'booleanp
  :group 'rescript)

(defcustom rescript-jsx-detect-syntax t
  "When non-nil, automatically detect whether JavaScript uses JSX.
`rescript-jsx-syntax' (which see) may be made buffer-local and set to
t.  The detection strategy can be customized by adding elements
to `rescript-jsx-regexps', which see."
  :version "27.1"
  :type 'boolean
  :safe 'booleanp
  :group 'rescript)

(defcustom rescript-jsx-syntax nil
  "When non-nil, parse JavaScript with consideration for JSX syntax.

This enables proper font-locking and indentation of code using
Facebook’s “JSX” syntax extension for JavaScript, for use with
Facebook’s “React” library.  Font-locking is like sgml-mode.
Indentation is also like sgml-mode, although some indentation
behavior may differ slightly to align more closely with the
conventions of the React developer community.

When `rescript-mode' is already enabled, you should call
`rescript-jsx-enable' to set this variable.

It is set to be buffer-local (and t) when in `rescript-jsx-mode'."
  :version "27.1"
  :type 'boolean
  :safe 'booleanp
  :group 'rescript)

(defcustom rescript-jsx-align->-with-< t
  "When non-nil, “>” will be indented to the opening “<” in JSX.

When this is enabled, JSX indentation looks like this:

  <element
    attr=\"\"
  >
  </element>
  <input
  />

When this is disabled, JSX indentation looks like this:

  <element
    attr=\"\"
    >
  </element>
  <input
    />"
  :version "27.1"
  :type 'boolean
  :safe 'booleanp
  :group 'rescript)

(defcustom rescript-jsx-indent-level nil
  "When non-nil, indent JSX by this value, instead of like JS.

Let `rescript-indent-level' be 4.  When this variable is also set to
nil, JSX indentation looks like this (consistent):

  return (
      <element>
          <element>
              Hello World!
          </element>
      </element>
  )

Alternatively, when this variable is also set to 2, JSX
indentation looks like this (different):

  return (
      <element>
        <element>
          Hello World!
        </element>
      </element>
  )"
  :version "27.1"
  :type '(choice integer
                 (const :tag "Not Set" nil))
  :safe (lambda (x) (or (null x) (integerp x)))
  :group 'rescript)
;; This is how indentation behaved out-of-the-box until Emacs 27.  JSX
;; indentation was controlled with `sgml-basic-offset', which defaults
;; to 2, whereas `rescript-indent-level' defaults to 4.  Users who had the
;; same values configured for both their HTML and JS indentation would
;; luckily get consistent JSX indentation; most others were probably
;; unhappy.  I’d be surprised if anyone actually wants different
;; indentation levels, but just in case, here’s a way back to that.

(defcustom rescript-jsx-attribute-offset 0
  "Specifies a delta for JSXAttribute indentation.

Let `rescript-indent-level' be 2.  When this variable is also set to 0,
JSXAttribute indentation looks like this:

  <element
    attribute=\"value\">
  </element>

Alternatively, when this variable is also set to 2, JSXAttribute
indentation looks like this:

  <element
      attribute=\"value\">
  </element>

This variable is like `sgml-attribute-offset'."
  :version "27.1"
  :type 'integer
  :safe 'integerp
  :group 'rescript)

;;; KeyMap

(defvar rescript-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [(control ?c) (meta ?:)] #'rescript-eval)
    (define-key keymap [(control ?c) (control ?j)] #'rescript-set-rescript-context)
    (define-key keymap [(control meta ?x)] #'rescript-eval-defun)
    (define-key keymap [(meta ?.)] #'rescript-find-symbol)
    (easy-menu-define nil keymap "JavaScript Menu"
      '("JavaScript"
        ["Select New Mozilla Context..." rescript-set-rescript-context
         (fboundp #'inferior-moz-process)]
        ["Evaluate Expression in Mozilla Context..." rescript-eval
         (fboundp #'inferior-moz-process)]
        ["Send Current Function to Mozilla..." rescript-eval-defun
         (fboundp #'inferior-moz-process)]))
    keymap)
  "Keymap for `rescript-mode'.")

;;; Syntax table and parsing

(defvar rescript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?` "\"" table)
    table)
  "Syntax table for `rescript-mode'.")

(defvar rescript--quick-match-re nil
  "Autogenerated regexp used by `rescript-mode' to match buffer constructs.")

(defvar rescript--quick-match-re-func nil
  "Autogenerated regexp used by `rescript-mode' to match constructs and functions.")

(make-variable-buffer-local 'rescript--quick-match-re)
(make-variable-buffer-local 'rescript--quick-match-re-func)

(defvar rescript--cache-end 1
  "Last valid buffer position for the `rescript-mode' function cache.")
(make-variable-buffer-local 'rescript--cache-end)

(defvar rescript--last-parse-pos nil
  "Latest parse position reached by `rescript--ensure-cache'.")
(make-variable-buffer-local 'rescript--last-parse-pos)

(defvar rescript--state-at-last-parse-pos nil
  "Parse state at `rescript--last-parse-pos'.")
(make-variable-buffer-local 'rescript--state-at-last-parse-pos)

(defun rescript--maybe-join (prefix separator suffix &rest list)
  "Helper function for `rescript--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (flatten-tree list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun rescript--update-quick-match-re ()
  "Internal function used by `rescript-mode' for caching buffer constructs.
This updates `rescript--quick-match-re', based on the current set of
enabled frameworks."
  (setq rescript--quick-match-re
        (rescript--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'extjs rescript-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype rescript-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (rescript--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*("

          (when (memq 'prototype rescript-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'extjs rescript-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress rescript-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo rescript-enabled-frameworks)
           "dojo\\.declare[ \t]*(")

         (when (memq 'mochikit rescript-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*(")

         ;; mumble.prototypeTHING
         (rescript--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'javascript rescript-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq rescript--quick-match-re-func
        (concat "function\\|" rescript--quick-match-re)))

(defun rescript--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun rescript--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst rescript--forward-pstate ()
  (rescript--forward-text-property 'rescript--pstate))

(defsubst rescript--backward-pstate ()
  (rescript--backward-text-property 'rescript--pstate))

(defun rescript--pitem-goto-h-end (pitem)
  (goto-char (rescript--pitem-h-begin pitem))
  (rescript--forward-pstate))

(defun rescript--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `rescript--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (rescript--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (point-at-eol) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (rescript--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun rescript--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'rescript--re-search-backward-inner)
               ((> count 0) #'rescript--re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))


(defun rescript--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `rescript--re-search-backward'."
  (let ((parse)
        (orig-macro-start
         (save-excursion
           (and (rescript--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((nth 8 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (rescript--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun rescript--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (rescript--re-search-forward regexp bound noerror (if count (- count) -1)))

(defun rescript--forward-expression ()
  "Move forward over a whole JavaScript expression.
This function doesn't move over expressions continued across
lines."
  (cl-loop
   ;; non-continued case; simplistic, but good enough?
   do (cl-loop until (or (eolp)
                         (progn
                           (forward-comment most-positive-fixnum)
                           (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
               do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (rescript--continued-expression-p)))))

(defun rescript--forward-function-decl ()
  "Move forward over a JavaScript function declaration.
This puts point at the `function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (cl-assert (looking-at "\\_<function\\_>"))
  (let ((name t))
    (forward-word-strictly)
    (forward-comment most-positive-fixnum)
    (when (eq (char-after) ?*)
      (forward-char)
      (forward-comment most-positive-fixnum))
    (when (looking-at rescript--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun rescript--function-prologue-beginning (&optional pos)
  "Return the start of the JavaScript function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at rescript--function-heading-2-re)
                  (looking-at rescript--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (rescript--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (rescript--forward-function-decl))

           (<= pos (point))
           (or prologue-begin (match-beginning 0))))))

(defun rescript--beginning-of-defun-raw ()
  "Helper function for `rescript-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (rescript--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (rescript--backward-pstate))
                (not (eq 'function (rescript--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun rescript--pstate-is-toplevel-defun (pstate)
  "Helper function for `rescript--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (cl-loop for pitem in pstate
           with func-depth = 0
           with func-pitem
           if (eq 'function (rescript--pitem-type pitem))
           do (cl-incf func-depth)
           and do (setq func-pitem pitem)
           finally return (if (eq func-depth 1) func-pitem)))

(defun rescript--beginning-of-defun-nested ()
  "Helper function for `rescript--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (cl-loop for pitem in (rescript--parse-state-at-point)
            if (and (eq 'function (rescript--pitem-type pitem))
                    (rescript--inside-pitem-p pitem))
            do (goto-char (rescript--pitem-h-begin pitem))
            and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (cl-loop for pstate = (rescript--backward-pstate)
            while pstate
            if (rescript--pstate-is-toplevel-defun pstate)
            do (goto-char (rescript--pitem-h-begin it))
            and return it)))

(defun rescript--beginning-of-defun-flat ()
  "Helper function for `rescript-beginning-of-defun'."
  (let ((pstate (rescript--beginning-of-defun-raw)))
    (when pstate
      (goto-char (rescript--pitem-h-begin (car pstate))))))

(defun rescript-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `rescript-mode'."
  (setq arg (or arg 1))
  (while (and (not (eobp)) (< arg 0))
    (cl-incf arg)
    (when (and (not rescript-flat-functions)
               (or (eq (rescript-syntactic-context) 'function)
                   (rescript--function-prologue-beginning)))
      (rescript-end-of-defun))

    (if (rescript--re-search-forward
         "\\_<function\\_>" nil t)
        (goto-char (rescript--function-prologue-beginning))
      (goto-char (point-max))))

  (while (> arg 0)
    (cl-decf arg)
    ;; If we're just past the end of a function, the user probably wants
    ;; to go to the beginning of *that* function
    (when (eq (char-before) ?})
      (backward-char))

    (let ((prologue-begin (rescript--function-prologue-beginning)))
      (cond ((and prologue-begin (< prologue-begin (point)))
             (goto-char prologue-begin))

            (rescript-flat-functions
             (rescript--beginning-of-defun-flat))
            (t
             (rescript--beginning-of-defun-nested))))))

(defun rescript--flush-caches (&optional beg ignored)
  "Flush the `rescript-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq rescript--cache-end (min rescript--cache-end beg)))

(defmacro rescript--debug (&rest _arguments)
  ;; `(message ,@arguments)
  )

(defun rescript--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (rescript--pitem-paren-depth top-item))
      (cl-assert (not (get-text-property (1- (point)) 'rescript-pend)))
      (put-text-property (1- (point)) (point) 'rescript--pend top-item)
      (setf (rescript--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (rescript--pitem-add-child (cl-second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro rescript--ensure-cache--update-parse ()
  "Helper function for `rescript--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `rescript--ensure-cache'."
  '(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (rescript--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (cl-assert (> (nth 0 parse)
                            (rescript--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (rescript--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (rescript--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (rescript--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun rescript--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'rescript--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun rescript--split-name (string)
  "Split a JavaScript name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar rescript--guess-function-name-start nil)

(defun rescript--guess-function-name (position)
  "Guess the name of the JavaScript function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `rescript--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq rescript--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at rescript--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq rescript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at rescript--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq rescript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun rescript--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (rescript--forward-text-property
                             'rescript--pend))
        (setf (rescript--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(rescript--pstate t rescript--pend t)))

(defun rescript--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< rescript--cache-end limit)

    (c-save-buffer-state
        (open-items
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         goal-point)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (cl-loop for style in rescript--class-styles
                     if (memq (plist-get style :framework)
                              rescript-enabled-frameworks)
                     collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char rescript--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'rescript--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'rescript--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'rescript--pstate))
                (cl-assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list rescript--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (rescript--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (cl-loop while (re-search-forward rescript--quick-match-re-func nil t)
                   for orig-match-start = (goto-char (match-beginning 0))
                   for orig-match-end = (match-end 0)
                   do (rescript--ensure-cache--update-parse)
                   for orig-depth = (nth 0 parse)

                   ;; Each of these conditions should return non-nil if
                   ;; we should add a new item and leave point at the end
                   ;; of the new item's header (h-end in the
                   ;; rescript--pitem diagram). This point is the one
                   ;; after the last character we need to unambiguously
                   ;; detect this construct. If one of these evaluates to
                   ;; nil, the location of the point is ignored.
                   if (cond
                       ;; In comment or string
                       ((nth 8 parse) nil)

                       ;; Regular function declaration
                       ((and (looking-at "\\_<function\\_>")
                             (setq name (rescript--forward-function-decl)))

                        (when (eq name t)
                          (setq name (rescript--guess-function-name orig-match-end))
                          (if name
                              (when rescript--guess-function-name-start
                                (setq orig-match-start
                                      rescript--guess-function-name-start))

                            (setq name t)))

                        (cl-assert (eq (char-after) ?{))
                        (forward-char)
                        (make-rescript--pitem
                         :paren-depth orig-depth
                         :h-begin orig-match-start
                         :type 'function
                         :name (if (eq name t)
                                   name
                                 (rescript--split-name name))))

                       ;; Macro
                       ((looking-at rescript--macro-decl-re)

                        ;; Macros often contain unbalanced parentheses.
                        ;; Make sure that h-end is at the textual end of
                        ;; the macro no matter what the parenthesis say.
                        (c-end-of-macro)
                        (rescript--ensure-cache--update-parse)

                        (make-rescript--pitem
                         :paren-depth (nth 0 parse)
                         :h-begin orig-match-start
                         :type 'macro
                         :name (list (match-string-no-properties 1))))

                       ;; "Prototype function" declaration
                       ((looking-at rescript--plain-method-re)
                        (goto-char (match-beginning 3))
                        (when (save-match-data
                                (rescript--forward-function-decl))
                          (forward-char)
                          (make-rescript--pitem
                           :paren-depth orig-depth
                           :h-begin orig-match-start
                           :type 'function
                           :name (nconc (rescript--split-name
                                         (match-string-no-properties 1))
                                        (list (match-string-no-properties 2))))))

                       ;; Class definition
                       ((cl-loop
                         with syntactic-context =
                         (rescript--syntactic-context-from-pstate open-items)
                         for class-style in filtered-class-styles
                         if (and (memq syntactic-context
                                       (plist-get class-style :contexts))
                                 (looking-at (plist-get class-style
                                                        :class-decl)))
                         do (goto-char (match-end 0))
                         and return
                         (make-rescript--pitem
                          :paren-depth orig-depth
                          :h-begin orig-match-start
                          :type class-style
                          :name (rescript--split-name
                                 (match-string-no-properties 1))))))

                   do (rescript--ensure-cache--update-parse)
                   and do (push it open-items)
                   and do (put-text-property
                           (1- (point)) (point) 'rescript--pstate open-items)
                   else do (goto-char orig-match-end))

          (goto-char limit)
          (rescript--ensure-cache--update-parse)
          (setq rescript--cache-end limit)
          (setq rescript--last-parse-pos limit)
          (setq rescript--state-at-last-parse-pos open-items)
          )))))

(defun rescript--end-of-defun-flat ()
  "Helper function for `rescript-end-of-defun'."
  (cl-loop while (rescript--re-search-forward "}" nil t)
           do (rescript--ensure-cache)
           if (get-text-property (1- (point)) 'rescript--pend)
           if (eq 'function (rescript--pitem-type it))
           return t
           finally do (goto-char (point-max))))

(defun rescript--end-of-defun-nested ()
  "Helper function for `rescript-end-of-defun'."
  (message "test")
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (rescript--beginning-of-defun-nested))
                          (rescript--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (rescript--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (rescript--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun rescript-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `rescript-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (cl-incf arg)
    (rescript-beginning-of-defun)
    (rescript-beginning-of-defun)
    (unless (bobp)
      (rescript-end-of-defun)))

  (while (> arg 0)
    (cl-decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if rescript-flat-functions
        (rescript--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call rescript--end-of-defun-nested to do the real work
      (let ((prologue-begin (rescript--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (rescript--forward-function-decl)
               (forward-list))

              (t (rescript--end-of-defun-nested)))))))

(defun rescript--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at rescript--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun rescript--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `rescript-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (rescript--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (rescript--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun rescript--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `rescript-mode'."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;; Like (up-list -1), but only considers lists that end nearby"
(defun rescript--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our computation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun rescript--inside-param-list-p ()
  "Return non-nil if point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (rescript--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun rescript--inside-dojo-class-list-p ()
  "Return non-nil if point is in a Dojo multiple-inheritance class block."
  (ignore-errors
    (save-excursion
      (rescript--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at rescript--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

;;; Font Lock
(defun rescript--make-framework-matcher (framework &rest regexps)
  "Helper function for building `rescript--font-lock-keywords'.
Create a byte-compiled function for matching a concatenation of
REGEXPS, but only if FRAMEWORK is in `rescript-enabled-frameworks'."
  (setq regexps (apply #'concat regexps))
  (byte-compile
   `(lambda (limit)
      (when (memq (quote ,framework) rescript-enabled-frameworks)
        (re-search-forward ,regexps limit t)))))

(defvar rescript--tmp-location nil)
(make-variable-buffer-local 'rescript--tmp-location)

(defun rescript--forward-destructuring-spec (&optional func)
  "Move forward over a JavaScript destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true if this was actually a
spec.  FUNC must preserve the match data."
  (pcase (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (rescript--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at rescript--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at rescript--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (rescript--forward-destructuring-spec func))
                      ((looking-at rescript--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun rescript--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at rescript--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (rescript--forward-destructuring-spec))

                          (rescript--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (rescript--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

;; It wouldn’t be sufficient to font-lock JSX with mere regexps, since
;; a JSXElement may be nested inside a JS expression within the
;; boundaries of a parent JSXOpeningElement, and such a hierarchy
;; ought to be fontified like JSX, JS, and JSX respectively:
;;
;;   <div attr={void(<div></div>) && void(0)}></div>
;;
;;   <div attr={           ← JSX
;;          void(          ← JS
;;            <div></div>  ← JSX
;;          ) && void(0)   ← JS
;;        }></div>         ← JSX
;;
;; `rescript-syntax-propertize' unambiguously identifies JSX syntax,
;; including when it’s nested.
;;
;; Using a matcher function for each relevant part, retrieve match
;; data recorded as syntax properties for fontification.

(defconst rescript-jsx--font-lock-keywords
  `((rescript-jsx--match-tag-name 0 font-lock-function-name-face t)
    (rescript-jsx--match-attribute-name 0 font-lock-variable-name-face t)
    (rescript-jsx--match-text 0 'default t) ; “Undo” keyword fontification.
    (rescript-jsx--match-tag-beg)
    (rescript-jsx--match-tag-end)
    (rescript-jsx--match-expr))
  "JSX font lock faces and multiline text properties.")

(defun rescript-jsx--match-tag-name (limit)
  "Match JSXBoundaryElement names, until LIMIT."
  (when rescript-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'rescript-jsx-tag-name nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'rescript-jsx-tag-name))
                 (progn (set-match-data value) t))
            (rescript-jsx--match-tag-name limit))))))

(defun rescript-jsx--match-attribute-name (limit)
  "Match JSXAttribute names, until LIMIT."
  (when rescript-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'rescript-jsx-attribute-name nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'rescript-jsx-attribute-name))
                 (progn (set-match-data value) t))
            (rescript-jsx--match-attribute-name limit))))))

(defun rescript-jsx--match-text (limit)
  "Match JSXText, until LIMIT."
  (when rescript-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'rescript-jsx-text nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'rescript-jsx-text))
                 (progn (set-match-data value)
                        (put-text-property (car value) (cadr value) 'font-lock-multiline t)
                        t))
            (rescript-jsx--match-text limit))))))

(defun rescript-jsx--match-tag-beg (limit)
  "Match JSXBoundaryElements from start, until LIMIT."
  (when rescript-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'rescript-jsx-tag-beg nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'rescript-jsx-tag-beg))
                 (progn (put-text-property pos (cdr value) 'font-lock-multiline t) t))
            (rescript-jsx--match-tag-beg limit))))))

(defun rescript-jsx--match-tag-end (limit)
  "Match JSXBoundaryElements from end, until LIMIT."
  (when rescript-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'rescript-jsx-tag-end nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'rescript-jsx-tag-end))
                 (progn (put-text-property value pos 'font-lock-multiline t) t))
            (rescript-jsx--match-tag-end limit))))))

(defun rescript-jsx--match-expr (limit)
  "Match JSXExpressionContainers, until LIMIT."
  (when rescript-jsx-syntax
    (let ((pos (next-single-char-property-change (point) 'rescript-jsx-expr nil limit))
          value)
      (when (and pos (> pos (point)))
        (goto-char pos)
        (or (and (setq value (get-text-property pos 'rescript-jsx-expr))
                 (progn (put-text-property pos value 'font-lock-multiline t) t))
            (rescript-jsx--match-expr limit))))))

(defconst rescript--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@rescript--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (rescript--class-decl-matcher
     ,(concat "\\(" rescript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (rescript--class-decl-matcher
     ,(concat "\\(" rescript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq rescript--tmp-location (match-end 2))
           (goto-char rescript--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq rescript--tmp-location nil)
       (goto-char (point-at-eol)))
     (when rescript--tmp-location
       (save-excursion
         (goto-char rescript--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (rescript--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(rescript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" rescript--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" rescript--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(rescript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" rescript--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" rescript--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(rescript--make-framework-matcher
       'dojo
       "^\\s-*" rescript--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" rescript--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (rescript--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" rescript--basic-type-re)
      (list #'rescript--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" rescript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" rescript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" rescript--name-re "\\)?\\s-*(\\s-*"
       rescript--name-start-re)
      (list (concat "\\(" rescript--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" rescript--name-re "\\s-*[,)]")
      (list rescript--name-re
            '(if (save-excursion (backward-char)
                                 (rescript--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face)))

    ;; jsx (when enabled)
    ,@rescript-jsx--font-lock-keywords)
  "Level three font lock for `rescript-mode'.")

(defun rescript--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (rescript--ensure-cache)
  (cl-assert (rescript--pitem-h-begin pitem))
  (cl-assert (rescript--pitem-paren-depth pitem))

  (and (> (point) (rescript--pitem-h-begin pitem))
       (or (null (rescript--pitem-b-end pitem))
           (> (rescript--pitem-b-end pitem) (point)))))

(defun rescript--parse-state-at-point ()
  "Parse the JavaScript program state at point.
Return a list of `rescript--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (rescript--ensure-cache)
      (let ((pstate (or (save-excursion
                          (rescript--backward-pstate))
                        (list rescript--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (cl-loop for pitem = (car pstate)
                 until (or (eq (rescript--pitem-type pitem)
                               'toplevel)
                           (rescript--inside-pitem-p pitem))
                 do (pop pstate))

        pstate))))

(defun rescript--syntactic-context-from-pstate (pstate)
  "Return the JavaScript syntactic context corresponding to PSTATE."
  (let ((type (rescript--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun rescript-syntactic-context ()
  "Return the JavaScript syntactic context at point.
When called interactively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (rescript--syntactic-context-from-pstate
                             (rescript--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun rescript--class-decl-matcher (limit)
  "Font lock function used by `rescript-mode'.
This performs fontification according to `rescript--class-styles'."
  (cl-loop initially (rescript--ensure-cache limit)
           while (re-search-forward rescript--quick-match-re limit t)
           for orig-end = (match-end 0)
           do (goto-char (match-beginning 0))
           if (cl-loop for style in rescript--class-styles
                       for decl-re = (plist-get style :class-decl)
                       if (and (memq (plist-get style :framework)
                                     rescript-enabled-frameworks)
                               (memq (rescript-syntactic-context)
                                     (plist-get style :contexts))
                               decl-re
                               (looking-at decl-re))
                       do (goto-char (match-end 0))
                       and return t)
           return t
           else do (goto-char orig-end)))

(defconst rescript--font-lock-keywords
  '(rescript--font-lock-keywords-3 rescript--font-lock-keywords-1
                                   rescript--font-lock-keywords-2
                                   rescript--font-lock-keywords-3)
  "Font lock keywords for `rescript-mode'.  See `font-lock-keywords'.")

(defun rescript-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      font-lock-string-face
    (if (save-excursion
          (goto-char (nth 8 state))
          (looking-at "/\\*\\*"))
        font-lock-doc-face
      font-lock-comment-face)))

(defconst rescript--syntax-propertize-regexp-regexp
  (rx
   ;; Start of regexp.
   "/"
   (0+ (or
        ;; Match characters outside of a character class.
        (not (any ?\[ ?/ ?\\))
        ;; Match backslash quoted characters.
        (and "\\" not-newline)
        ;; Match character class.
        (and
         "["
         (0+ (or
              (not (any ?\] ?\\))
              (and "\\" not-newline)))
         "]")))
   (group (zero-or-one "/")))
  "Regular expression matching a JavaScript regexp literal.")

(defun rescript-syntax-propertize-regexp (end)
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      ;; A /.../ regexp.
      (goto-char (nth 8 ppss))
      (when (looking-at rescript--syntax-propertize-regexp-regexp)
        ;; Don't touch text after END.
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defconst rescript--unary-keyword-re
  (rescript--regexp-opt-symbol '("await" "delete" "typeof" "void" "yield"))
  "Regexp matching unary operator keywords.")

(defun rescript--unary-keyword-p (string)
  "Check if STRING is a unary operator keyword in JavaScript."
  (string-match-p rescript--unary-keyword-re string))

;; Adding `syntax-multiline' text properties to JSX isn’t sufficient
;; to identify multiline JSX when first typing it.  For instance, if
;; the user is typing a JSXOpeningElement for the first time…
;;
;;   <div
;;       ^ (point)
;;
;; …and the user inserts a line break after the tag name (before the
;; JSXOpeningElement starting on that line has been unambiguously
;; identified as such), then the `syntax-propertize' region won’t be
;; extended backwards to the start of the JSXOpeningElement:
;;
;;   <div         ← This line wasn’t JSX when last edited.
;;     attr="">   ← Despite completing the JSX, the next
;;             ^    `syntax-propertize' region wouldn’t magically
;;                  extend back a few lines.
;;
;; Therefore, to try and recover from this scenario, parse backward
;; from “>” to try and find the start of JSXBoundaryElements, and
;; extend the `syntax-propertize' region there.

(defun rescript--syntax-propertize-extend-region (start end)
  "Extend the START-END region for propertization, if necessary.
For use by `syntax-propertize-extend-region-functions'."
  (if rescript-jsx-syntax (rescript-jsx--syntax-propertize-extend-region start end)))

(defun rescript-jsx--syntax-propertize-extend-region (start end)
  "Extend the START-END region for propertization, if necessary.
If any “>” in the region appears to be the end of a tag starting
before the start of the region, extend region backwards to the
start of that tag so parsing may proceed from that point.
For use by `syntax-propertize-extend-region-functions'."
  (let (new-start
        forward-sexp-function ; Use the Lisp version.
        parse-sexp-lookup-properties) ; Fix backward-sexp error here.
    (catch 'stop
      (goto-char start)
      (while (re-search-forward ">" end t)
        (catch 'continue
          ;; Check if this is really a right shift bitwise operator
          ;; (“>>” or “>>>”).
          (unless (or (eq (char-before (1- (point))) ?>)
                      (eq (char-after) ?>))
            (save-excursion
              (backward-char)
              (while (progn (if (= (point) (point-min)) (throw 'continue nil))
                            (/= (char-before) ?<))
                (skip-chars-backward " \t\n")
                (if (= (point) (point-min)) (throw 'continue nil))
                (cond
                 ((memq (char-before) '(?\" ?\' ?\` ?\}))
                  (condition-case nil
                      (backward-sexp)
                    (scan-error (throw 'continue nil))))
                 ((memq (char-before) '(?\/ ?\=)) (backward-char))
                 ((looking-back rescript--dotted-name-re (line-beginning-position) t)
                  (goto-char (match-beginning 0)))
                 (t (throw 'continue nil))))
              (when (< (point) start)
                (setq new-start (1- (point)))
                (throw 'stop nil)))))))
    (if new-start (cons new-start end))))

;; When applying syntax properties, since `rescript-syntax-propertize' uses
;; `syntax-propertize-rules' to parse JSXBoundaryElements iteratively
;; and statelessly, whenever we exit such an element, we need to
;; determine the JSX depth.  If >0, then we know to apply syntax
;; properties to JSXText up until the next JSXBoundaryElement occurs.
;; But if the JSX depth is 0, then—importantly—we know to NOT parse
;; the following code as JSXText, rather propertize it as regular JS
;; as long as warranted.
;;
;; Also, when indenting code, we need to know if the code we’re trying
;; to indent is on the 2nd or later line of multiline JSX, in which
;; case the code is indented according to XML-like JSX conventions.
;;
;; For the aforementioned reasons, we find ourselves needing to
;; determine whether point is enclosed in JSX or not; and, if so,
;; where the JSX is.  The following functions provide that knowledge.

(defconst rescript-jsx--tag-start-re
  (concat "\\(" rescript--dotted-name-re "\\)\\(?:"
          ;; Whitespace is only necessary if an attribute implies JSX.
          "\\(?:\\s-\\|\n\\)*[{/>]"
          "\\|"
          "\\(?:\\s-\\|\n\\)+" rescript--name-start-re
          "\\)")
  "Regexp unambiguously matching a JSXOpeningElement.")

(defun rescript-jsx--matched-tag-type ()
  "Determine if the last “<” was a JSXBoundaryElement and its type.
Return `close' for a JSXClosingElement/JSXClosingFragment match,
return `self-closing' for some self-closing JSXOpeningElements,
else return `other'."
  (cond
   ((= (char-after) ?/) (forward-char) 'close) ; JSXClosingElement/JSXClosingFragment
   ((= (char-after) ?>) (forward-char) 'other) ; JSXOpeningFragment
   ((and (looking-at rescript-jsx--tag-start-re) ; JSXOpeningElement
         (not (rescript--unary-keyword-p (match-string 1))))
    (goto-char (match-end 0))
    (if (= (char-before) ?/) 'self-closing 'other))))

(defconst rescript-jsx--self-closing-re "/\\s-*>"
  "Regexp matching the end of a self-closing JSXOpeningElement.")

(defun rescript-jsx--matching-close-tag-pos ()
  "Return position of the closer of the opener before point.
Assuming a JSXOpeningElement or a JSXOpeningFragment is
immediately before point, find a matching JSXClosingElement or
JSXClosingFragment, skipping over any nested JSXElements to find
the match.  Return nil if a match can’t be found."
  (let ((tag-stack 1) tag-pos type last-pos pos)
    (catch 'stop
      (while (and (re-search-forward "<\\s-*" nil t) (not (eobp)))
        ;; Not inside a comment or string.
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (when (setq tag-pos (match-beginning 0)
                      type (rescript-jsx--matched-tag-type))
            (when last-pos
              (setq pos (point))
              (goto-char last-pos)
              (while (re-search-forward rescript-jsx--self-closing-re pos 'move)
                (setq tag-stack (1- tag-stack))))
            (if (eq type 'close)
                (progn
                  (setq tag-stack (1- tag-stack))
                  (when (= tag-stack 0)
                    (throw 'stop tag-pos)))
              ;; JSXOpeningElements that we know are self-closing
              ;; aren’t added to the stack at all (because point is
              ;; already past that syntax).
              (unless (eq type 'self-closing)
                (setq tag-stack (1+ tag-stack))))
            (setq last-pos (point))))))))

(defun rescript-jsx--enclosing-tag-pos ()
  "Return beginning and end of a JSXElement about point.
Look backward for a JSXElement that both starts before point and
also ends at/after point.  That may be either a self-closing
JSXElement or a JSXOpeningElement/JSXClosingElement pair."
  (let ((start (point)) tag-beg tag-beg-pos tag-end-pos close-tag-pos)
    (while
        (and
         (setq tag-beg (rescript--backward-text-property 'rescript-jsx-tag-beg))
         (progn
           (setq tag-beg-pos (point)
                 tag-end-pos (cdr tag-beg))
           (not
            (or
             (and (eq (car tag-beg) 'self-closing)
                  (< start tag-end-pos))
             (and (eq (car tag-beg) 'open)
                  (or (< start tag-end-pos)
                      (progn
                        (unless
                            ;; Try to read a cached close position,
                            ;; but it might not be available yet.
                            (setq close-tag-pos
                                  (get-text-property (point) 'rescript-jsx-close-tag-pos))
                          (save-excursion
                            (goto-char tag-end-pos)
                            (setq close-tag-pos (rescript-jsx--matching-close-tag-pos)))
                          (when close-tag-pos
                            ;; Cache the close position to make future
                            ;; searches faster.
                            (put-text-property
                             (point) (1+ (point))
                             'rescript-jsx-close-tag-pos close-tag-pos)))
                        ;; The JSXOpeningElement may be unclosed, else
                        ;; the closure must occur at/after the start
                        ;; point (otherwise, a miscellaneous previous
                        ;; JSXOpeningElement has been found, so keep
                        ;; looking backwards for an enclosing one).
                        (or (not close-tag-pos) (<= start close-tag-pos)))))))))
      ;; Don’t return the last tag pos, as it wasn’t enclosing.
      (setq tag-beg nil close-tag-pos nil))
    (and tag-beg (list tag-beg-pos tag-end-pos close-tag-pos))))

(defun rescript-jsx--at-enclosing-tag-child-p ()
  "Return t if point is at an enclosing tag’s child."
  (let ((pos (save-excursion (rescript-jsx--enclosing-tag-pos))))
    (and pos (>= (point) (nth 1 pos)))))

;; We implement `syntax-propertize-function' logic fully parsing JSX
;; in order to provide very accurate JSX indentation, even in the most
;; complex cases (e.g. to indent JSX within a JS expression within a
;; JSXAttribute…), as over the years users have requested this.  Since
;; we find so much information during this parse, we later use some of
;; the useful bits for font-locking, too.
;;
;; Some extra effort is devoted to ensuring that no code which could
;; possibly be valid JS is ever misinterpreted as partial JSX, since
;; that would be regressive.
;;
;; We first parse trying to find the minimum number of components
;; necessary to unambiguously identify a JSXBoundaryElement, even if
;; it is a partial one.  If a complete one is parsed, we move on to
;; parse any JSXText.  When that’s terminated, we unwind back to the
;; `syntax-propertize-rules' loop so the next JSXBoundaryElement can
;; be parsed, if any, be it an opening or closing one.

(defun rescript-jsx--text-range (beg end)
  "Identify JSXText within a “>/{/}/<” pair."
  (when (> (- end beg) 0)
    (save-excursion
      (goto-char beg)
      (while (and (skip-chars-forward " \t\n" end) (< (point) end))
        ;; Comments and string quotes don’t serve their usual
        ;; syntactic roles in JSXText; make them plain punctuation to
        ;; negate those roles.
        (when (or (= (char-after) ?/) ; comment
                  (= (syntax-class (syntax-after (point))) 7)) ; string quote
          (put-text-property (point) (1+ (point)) 'syntax-table '(1)))
        (forward-char)))
    ;; Mark JSXText so it can be font-locked as non-keywords.
    (put-text-property beg (1+ beg) 'rescript-jsx-text (list beg end (current-buffer)))
    ;; Ensure future propertization beginning from within the
    ;; JSXText determines JSXText context from earlier lines.
    (put-text-property beg end 'syntax-multiline t)))

;; In order to respect the end boundary `syntax-propertize-function'
;; sets, care is taken in the following functions to abort parsing
;; whenever that boundary is reached.

(defun rescript-jsx--syntax-propertize-tag-text (end)
  "Determine if JSXText is before END and propertize it.
Text within an open/close tag pair may be JSXText.  Temporarily
interrupt JSXText by JSXExpressionContainers, and terminate
JSXText when another JSXBoundaryElement is encountered.  Despite
terminations, all JSXText will be identified once all the
JSXBoundaryElements within an outermost JSXElement’s tree have
been propertized."
  (let ((text-beg (point))
        forward-sexp-function) ; Use Lisp version.
    (catch 'stop
      (while (re-search-forward "[{<]" end t)
        (rescript-jsx--text-range text-beg (1- (point)))
        (cond
         ((= (char-before) ?{)
          (let (expr-beg expr-end)
            (condition-case nil
                (save-excursion
                  (backward-char)
                  (setq expr-beg (point))
                  (forward-sexp)
                  (setq expr-end (point)))
              (scan-error nil))
            ;; Recursively propertize the JSXExpressionContainer’s
            ;; (possibly-incomplete) expression.
            (rescript-syntax-propertize (1+ expr-beg) (if expr-end (min (1- expr-end) end) end))
            ;; Ensure future propertization beginning from within the
            ;; (possibly-incomplete) expression can determine JSXText
            ;; context from earlier lines.
            (put-text-property expr-beg (1+ expr-beg) 'rescript-jsx-expr (or expr-end end)) ; font-lock
            (put-text-property expr-beg (if expr-end (min expr-end end) end) 'syntax-multiline t) ; syntax-propertize
            ;; Exit the JSXExpressionContainer if that’s possible,
            ;; else move to the end of the propertized area.
            (goto-char (if expr-end (min expr-end end) end))))
         ((= (char-before) ?<)
          (backward-char) ; Ensure the next tag can be propertized.
          (throw 'stop nil)))
        (setq text-beg (point))))))

(defconst rescript-jsx--attribute-name-re (concat rescript--name-start-re
                                            "\\(?:\\s_\\|\\sw\\|-\\)*")
  "Like `rescript--name-re', but matches “-” as well.")

(defun rescript-jsx--syntax-propertize-tag (end)
  "Determine if a JSXBoundaryElement is before END and propertize it.
Disambiguate JSX from inequality operators and arrow functions by
testing for syntax only valid as JSX."
  (let ((tag-beg (1- (point))) tag-end (type 'open)
        name-beg name-match-data expr-attribute-beg unambiguous
        forward-sexp-function) ; Use Lisp version.
    (catch 'stop
      (while (and (< (point) end)
                  (progn (skip-chars-forward " \t\n" end)
                         (< (point) end)))
        (cond
         ((= (char-after) ?>)
          ;; Make the closing “>” a close parenthesis.
          (put-text-property (point) (1+ (point)) 'syntax-table
                             (eval-when-compile (string-to-syntax ")<")))
          (forward-char)
          (setq unambiguous t)
          (throw 'stop nil))
         ;; Handle a JSXSpreadChild (“<Foo {...bar}”) or a
         ;; JSXExpressionContainer as a JSXAttribute value
         ;; (“<Foo bar={…}”).  Check this early in case continuing a
         ;; JSXAttribute parse.
         ((or (and name-beg (= (char-after) ?{))
              (setq expr-attribute-beg nil))
          (setq unambiguous t) ; JSXExpressionContainer post tag name ⇒ JSX
          (when expr-attribute-beg
            ;; Remember that this JSXExpressionContainer is part of a
            ;; JSXAttribute, as that can affect its expression’s
            ;; indentation.
            (put-text-property
             (point) (1+ (point)) 'rescript-jsx-expr-attribute expr-attribute-beg)
            (setq expr-attribute-beg nil))
          (let (expr-end)
            (condition-case nil
                (save-excursion
                  (forward-sexp)
                  (setq expr-end (point)))
              (scan-error nil))
            (forward-char)
            (if (>= (point) end) (throw 'stop nil))
            (skip-chars-forward " \t\n" end)
            (if (>= (point) end) (throw 'stop nil))
            (if (= (char-after) ?}) (forward-char) ; Shortcut to bail.
              ;; Recursively propertize the JSXExpressionContainer’s
              ;; expression.
              (rescript-syntax-propertize (point) (if expr-end (min (1- expr-end) end) end))
              ;; Exit the JSXExpressionContainer if that’s possible,
              ;; else move to the end of the propertized area.
              (goto-char (if expr-end (min expr-end end) end)))))
         ((= (char-after) ?/)
          ;; Assume a tag is an open tag until a slash is found, then
          ;; figure out what type it actually is.
          (if (eq type 'open) (setq type (if name-beg 'self-closing 'close)))
          (forward-char))
         ((and (not name-beg) (looking-at rescript--dotted-name-re))
          ;; Don’t match code like “if (i < await foo)”
          (if (rescript--unary-keyword-p (match-string 0)) (throw 'stop nil))
          ;; Save boundaries for later fontification after
          ;; unambiguously determining the code is JSX.
          (setq name-beg (match-beginning 0)
                name-match-data (match-data))
          (goto-char (match-end 0)))
         ((and name-beg (looking-at rescript-jsx--attribute-name-re))
          (setq unambiguous t) ; Non-unary name followed by 2nd name ⇒ JSX
          ;; Save JSXAttribute’s name’s match data for font-locking later.
          (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                             'rescript-jsx-attribute-name (match-data))
          (goto-char (match-end 0))
          (if (>= (point) end) (throw 'stop nil))
          (skip-chars-forward " \t\n" end)
          (if (>= (point) end) (throw 'stop nil))
          ;; “=” is optional for null-valued JSXAttributes.
          (when (= (char-after) ?=)
            (forward-char)
            (if (>= (point) end) (throw 'stop nil))
            (skip-chars-forward " \t\n" end)
            (if (>= (point) end) (throw 'stop nil))
            ;; Skip over strings (if possible).  Any
            ;; JSXExpressionContainer here will be parsed in the
            ;; next iteration of the loop.
            (if (memq (char-after) '(?\" ?\' ?\`))
                (progn
                  ;; Record the string’s position so derived modes
                  ;; applying syntactic fontification atypically
                  ;; (e.g. js2-mode) can recognize it as part of JSX.
                  (put-text-property (point) (1+ (point)) 'rescript-jsx-string t)
                  (condition-case nil
                      (forward-sexp)
                    (scan-error (throw 'stop nil))))
              ;; Save JSXAttribute’s beginning in case we find a
              ;; JSXExpressionContainer as the JSXAttribute’s value which
              ;; we should associate with the JSXAttribute.
              (setq expr-attribute-beg (match-beginning 0)))))
         ;; There is nothing more to check; this either isn’t JSX, or
         ;; the tag is incomplete.
         (t (throw 'stop nil)))))
    (when unambiguous
      ;; Save JSXBoundaryElement’s name’s match data for font-locking.
      (if name-beg (put-text-property name-beg (1+ name-beg) 'rescript-jsx-tag-name name-match-data))
      ;; Make the opening “<” an open parenthesis.
      (put-text-property tag-beg (1+ tag-beg) 'syntax-table
                         (eval-when-compile (string-to-syntax "(>")))
      ;; Prevent “out of range” errors when typing at the end of a buffer.
      (setq tag-end (if (eobp) (1- (point)) (point)))
      ;; Mark beginning and end of tag for font-locking.
      (put-text-property tag-beg (1+ tag-beg) 'rescript-jsx-tag-beg (cons type tag-end))
      (put-text-property tag-end (1+ tag-end) 'rescript-jsx-tag-end tag-beg)
      ;; Use text properties to extend the syntax-propertize region
      ;; backward to the beginning of the JSXBoundaryElement in the
      ;; future.  Typically the closing angle bracket could suggest
      ;; extending backward, but that would also involve more rigorous
      ;; parsing, and the closing angle bracket may not even exist yet
      ;; if the JSXBoundaryElement is still being typed.
      (put-text-property tag-beg (1+ tag-end) 'syntax-multiline t))
    (if (rescript-jsx--at-enclosing-tag-child-p) (rescript-jsx--syntax-propertize-tag-text end))))

(defconst rescript-jsx--text-properties
  (list
   'rescript-jsx-tag-beg nil 'rescript-jsx-tag-end nil 'rescript-jsx-close-tag-pos nil
   'rescript-jsx-tag-name nil 'rescript-jsx-attribute-name nil 'rescript-jsx-string nil
   'rescript-jsx-text nil 'rescript-jsx-expr nil 'rescript-jsx-expr-attribute nil)
  "Plist of text properties added by `rescript-syntax-propertize'.")

(defun rescript-syntax-propertize (start end)
  ;; JavaScript allows immediate regular expression objects, written /.../.
  (goto-char start)
  (if rescript-jsx-syntax (remove-text-properties start end rescript-jsx--text-properties))
  (rescript-syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ;; Distinguish /-division from /-regexp chars (and from /-comment-starter).
    ;; FIXME: Allow regexps after infix ops like + ...
    ;; https://developer.mozilla.org/en/JavaScript/Reference/Operators
    ;; We can probably just add +, -, <, >, %, ^, ~, ?, : at which
    ;; point I think only * and / would be missing which could also be added,
    ;; but need care to avoid affecting the // and */ comment markers.
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
	 (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   ;; If the / is at the beginning of line, we have to check
                   ;; the end of the previous text.
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1) (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (rescript-syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b"))
    ("<" (0 (ignore
             (when rescript-jsx-syntax
               ;; Not inside a comment or string.
               (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
                 (rescript-jsx--syntax-propertize-tag end)))))))
   (point) end))

(defconst rescript--prettify-symbols-alist
  '(("=>" . ?⇒)
    (">=" . ?≥)
    ("<=" . ?≤))
  "Alist of symbol prettifications for JavaScript.")

;;; Indentation

(defconst rescript--possibly-braceless-keyword-re
  (rescript--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst rescript--declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
  "Regular expression matching variable declaration keywords.")

(defconst rescript--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\|"
          (rescript--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")

(defun rescript-jsx--looking-at-start-tag-p ()
  "Non-nil if a JSXOpeningElement immediately follows point."
  (let ((tag-beg (get-text-property (point) 'rescript-jsx-tag-beg)))
    (and tag-beg (memq (car tag-beg) '(open self-closing)))))

(defun rescript--looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at rescript--indent-operator-re)
         (or (not (eq (char-after) ?:))
             (save-excursion
               (rescript--backward-syntactic-ws)
               (when (= (char-before) ?\)) (backward-list))
               (and (rescript--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (eq (char-after) ??))))
         (not (and
               (eq (char-after) ?/)
               (save-excursion
                 (eq (nth 3 (syntax-ppss)) ?/))))
         (not (and
               (eq (char-after) ?*)
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" rescript--name-re " *(\\)"))
               (save-excursion
                 (rescript--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{)))))
         ;; “<” isn’t necessarily an operator in JSX.
         (not (and rescript-jsx-syntax (rescript-jsx--looking-at-start-tag-p))))))

(defun rescript--find-newline-backward ()
  "Move backward to the nearest newline that is not in a block comment."
  (let ((continue t)
        (result t))
    (while continue
      (setq continue nil)
      (if (search-backward "\n" nil t)
          (let ((parse (syntax-ppss)))
            ;; We match the end of a // comment but not a newline in a
            ;; block comment.
            (when (nth 4 parse)
              (goto-char (nth 8 parse))
              ;; If we saw a block comment, keep trying.
              (unless (nth 7 parse)
                (setq continue t))))
        (setq result nil)))
    result))

(defun rescript-jsx--looking-back-at-end-tag-p ()
  "Non-nil if a JSXClosingElement immediately precedes point."
  (get-text-property (point) 'rescript-jsx-tag-end))

(defun rescript--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (if (rescript--looking-at-operator-p)
        (if (eq (char-after) ?/)
            (prog1
                (not (nth 3 (syntax-ppss (1+ (point)))))
              (forward-char -1))
          (or
           (not (memq (char-after) '(?- ?+)))
           (progn
             (forward-comment (- (point)))
             (not (memq (char-before) '(?, ?\[ ?\())))))
      (and (rescript--find-newline-backward)
           (progn
             (skip-chars-backward " \t")
             (and
              ;; The “>” at the end of any JSXBoundaryElement isn’t
              ;; part of a continued expression.
              (not (and rescript-jsx-syntax (rescript-jsx--looking-back-at-end-tag-p)))
              (progn
                (or (bobp) (backward-char))
                (and (> (point) (point-min))
                     (save-excursion
                       (backward-char)
                       (not (looking-at "[/*]/\\|=>")))
                     (rescript--looking-at-operator-p)
                     (and (progn (backward-char)
                                 (not (looking-at "\\+\\+\\|--\\|/[/*]"))))))))))))

(defun rescript--skip-term-backward ()
  "Skip a term before point; return t if a term was skipped."
  (let ((term-skipped nil))
    ;; Skip backward over balanced parens.
    (let ((progress t))
      (while progress
        (setq progress nil)
        ;; First skip whitespace.
        (skip-syntax-backward " ")
        ;; Now if we're looking at closing paren, skip to the opener.
        ;; This doesn't strictly follow JS syntax, in that we might
        ;; skip something nonsensical like "()[]{}", but it is enough
        ;; if it works ok for valid input.
        (when (memq (char-before) '(?\] ?\) ?\}))
          (setq progress t term-skipped t)
          (backward-list))))
    ;; Maybe skip over a symbol.
    (let ((save-point (point)))
      (if (and (< (skip-syntax-backward "w_") 0)
                 (looking-at rescript--name-re))
          ;; Skipped.
          (progn
            (setq term-skipped t)
            (skip-syntax-backward " "))
        ;; Did not skip, so restore point.
        (goto-char save-point)))
    (when (and term-skipped (> (point) (point-min)))
      (backward-char)
      (eq (char-after) ?.))))

(defun rescript--skip-terms-backward ()
  "Skip any number of terms backward.
Move point to the earliest \".\" without changing paren levels.
Returns t if successful, nil if no term was found."
  (when (rescript--skip-term-backward)
    ;; Found at least one.
    (let ((last-point (point)))
      (while (rescript--skip-term-backward)
        (setq last-point (point)))
      (goto-char last-point)
      t)))

(defun rescript--chained-expression-p ()
  "A helper for rescript--proper-indentation that handles chained expressions.
A chained expression is when the current line starts with '.' and the
previous line also has a '.' expression.
This function returns the indentation for the current line if it is
a chained expression line; otherwise nil.
This should only be called while point is at the start of the line's content,
as determined by `back-to-indentation'."
  (when rescript-chain-indent
    (save-excursion
      (when (and (eq (char-after) ?.)
                 (rescript--continued-expression-p)
                 (rescript--find-newline-backward)
                 (rescript--skip-terms-backward))
        (current-column)))))

(defun rescript--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward " \t\n}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (rescript--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (rescript--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (rescript--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun rescript--ctrl-statement-indentation ()
  "Helper function for `rescript--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (rescript--re-search-backward "[[:graph:]]" nil t)
                 (progn
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at rescript--possibly-braceless-keyword-re))
                 (memq (char-before) '(?\s ?\t ?\n ?\}))
                 (not (rescript--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) rescript-indent-level)))))

(defun rescript--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c rescript-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun rescript--same-line (pos)
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defun rescript--multi-line-declaration-indentation ()
  "Helper function for `rescript--proper-indentation'.
Return the proper indentation of the current line if it belongs to a declaration
statement spanning multiple lines; otherwise, return nil."
  (let (forward-sexp-function ; Use Lisp version.
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at rescript--declaration-keyword-re))
        (let ((pt (point)))
          (when (looking-at rescript--indent-operator-re)
            (goto-char (match-end 0)))
          ;; The "operator" is probably a regexp literal opener.
          (when (nth 3 (syntax-ppss))
            (goto-char pt)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (rescript--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at rescript--indent-operator-re)
                                   (rescript--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (rescript--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at rescript--declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun rescript--indent-in-array-comp (bracket)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((end (point)))
    (save-excursion
      (goto-char bracket)
      (when (looking-at "\\[")
        (forward-char 1)
        (rescript--forward-syntactic-ws)
        (if (looking-at "[[{]")
            (let (forward-sexp-function) ; Use Lisp version.
              (condition-case nil
                  (progn
                    (forward-sexp)       ; Skip destructuring form.
                    (rescript--forward-syntactic-ws)
                    (if (and (/= (char-after) ?,) ; Regular array.
                             (looking-at "for"))
                        (match-beginning 0)))
                (scan-error
                 ;; Nothing to do here.
                 nil)))
          ;; To skip arbitrary expressions we need the parser,
          ;; so we'll just guess at it.
          (if (and (> end (point)) ; Not empty literal.
                   (re-search-forward "[^,]]* \\(for\\_>\\)" end t)
                   ;; Not inside comment or string literal.
                   (let ((status (parse-partial-sexp bracket (point))))
                     (and (= 1 (car status))
                          (not (nth 8 status)))))
              (match-beginning 1)))))))

(defun rescript--array-comp-indentation (bracket for-kwd)
  (if (rescript--same-line for-kwd)
      ;; First continuation line.
      (save-excursion
        (goto-char bracket)
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun rescript--maybe-goto-declaration-keyword-end (parse-status)
  "Helper function for `rescript--proper-indentation'.
Depending on the value of `rescript-indent-first-init', move
point to the end of a variable declaration keyword so that
indentation is aligned to that column."
  (cond
   ((eq rescript-indent-first-init t)
    (when (looking-at rescript--declaration-keyword-re)
      (goto-char (1+ (match-end 0)))))
   ((eq rescript-indent-first-init 'dynamic)
    (let ((bracket (nth 1 parse-status))
          declaration-keyword-end
          at-closing-bracket-p
          forward-sexp-function ; Use Lisp version.
          comma-p)
      (when (looking-at rescript--declaration-keyword-re)
        (setq declaration-keyword-end (match-end 0))
        (save-excursion
          (goto-char bracket)
          (setq at-closing-bracket-p
                (condition-case nil
                    (progn
                      (forward-sexp)
                      t)
                  (error nil)))
          (when at-closing-bracket-p
            (while (forward-comment 1))
            (setq comma-p (looking-at-p ","))))
        (when comma-p
          (goto-char (1+ declaration-keyword-end))))))))

(defconst rescript--line-terminating-arrow-re "=>\\s-*\\(/[/*]\\|$\\)"
  "Regexp matching the last \"=>\" (arrow) token on a line.
Whitespace and comments around the arrow are ignored.")

(defun rescript--broken-arrow-terminates-line-p ()
  "Helper function for `rescript--proper-indentation'.
Return non-nil if the last non-comment, non-whitespace token of the
current line is the \"=>\" token (of an arrow function)."
  (let ((from (point)))
    (end-of-line)
    (re-search-backward rescript--line-terminating-arrow-re from t)))

;; When indenting, we want to know if the line is…
;;
;;   - within a multiline JSXElement, or
;;   - within a string in a JSXBoundaryElement, or
;;   - within JSXText, or
;;   - within a JSXAttribute’s multiline JSXExpressionContainer.
;;
;; In these cases, special XML-like indentation rules for JSX apply.
;; If JS is nested within JSX, then indentation calculations may be
;; combined, such that JS indentation is “relative” to the JSX’s.
;;
;; Therefore, functions below provide such contextual information, and
;; `rescript--proper-indentation' may call itself once recursively in order
;; to finish calculating that “relative” JS+JSX indentation.

(defun rescript-jsx--context ()
  "Determine JSX context and move to enclosing JSX."
  (let ((pos (point))
        (parse-status (syntax-ppss))
        (enclosing-tag-pos (rescript-jsx--enclosing-tag-pos)))
    (when enclosing-tag-pos
      (if (< pos (nth 1 enclosing-tag-pos))
          (if (nth 3 parse-status)
              (list 'string (nth 8 parse-status))
            (list 'tag (nth 0 enclosing-tag-pos) (nth 1 enclosing-tag-pos)))
        (list 'text (nth 0 enclosing-tag-pos) (nth 2 enclosing-tag-pos))))))

(defun rescript-jsx--contextual-indentation (line context)
  "Calculate indentation column for LINE from CONTEXT.
The column calculation is based off of `sgml-calculate-indent'."
  (pcase (nth 0 context)

    ('string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (nth 1 context))
		 (zerop (forward-line -1))
		 (looking-at "[ \t]*$")))
     (if (> (point) (nth 1 context))
	 ;; Previous line is inside the string.
	 (current-indentation)
       (goto-char (nth 1 context))
       (1+ (current-column))))

    ('tag
     ;; Special JSX indentation rule: a “dangling” closing angle
     ;; bracket on its own line is indented at the same level as the
     ;; opening angle bracket of the JSXElement.  Otherwise, indent
     ;; JSXAttribute space like SGML.
     (if (and
          rescript-jsx-align->-with-<
          (progn
            (goto-char (nth 2 context))
            (and (= line (line-number-at-pos))
                 (looking-back "^\\s-*/?>" (line-beginning-position)))))
         (progn
           (goto-char (nth 1 context))
           (current-column))
       ;; Indent JSXAttribute space like SGML.
       (goto-char (nth 1 context))
       ;; Skip tag name:
       (skip-chars-forward " \t")
       (skip-chars-forward "^ \t\n")
       (skip-chars-forward " \t")
       (if (not (eolp))
	   (current-column)
         ;; This is the first attribute: indent.
         (goto-char (+ (nth 1 context) rescript-jsx-attribute-offset))
         (+ (current-column) (or rescript-jsx-indent-level rescript-indent-level)))))

    ('text
     ;; Indent to reflect nesting.
     (goto-char (nth 1 context))
     (+ (current-column)
        ;; The last line isn’t nested, but the rest are.
        (if (or (not (nth 2 context)) ; Unclosed.
                (< line (line-number-at-pos (nth 2 context))))
            (or rescript-jsx-indent-level rescript-indent-level)
          0)))

    ))

(defun rescript-jsx--enclosing-curly-pos ()
  "Return position of enclosing “{” in a “{/}” pair about point."
  (let ((parens (reverse (nth 9 (syntax-ppss)))) paren-pos curly-pos)
    (while
        (and
         (setq paren-pos (car parens))
         (not (when (= (char-after paren-pos) ?{)
                (setq curly-pos paren-pos)))
         (setq parens (cdr parens))))
    curly-pos))

(defun rescript-jsx--goto-outermost-enclosing-curly (limit)
  "Set point to enclosing “{” at or closest after LIMIT."
  (let (pos)
    (while
        (and
         (setq pos (rescript-jsx--enclosing-curly-pos))
         (if (>= pos limit) (goto-char pos))
         (> pos limit)))))

(defun rescript-jsx--expr-attribute-pos (start limit)
  "Look back from START to LIMIT for a JSXAttribute."
  (save-excursion
    (goto-char start) ; Skip the first curly.
    ;; Skip any remaining enclosing curlies until the JSXElement’s
    ;; beginning position; the last curly ought to be one of a
    ;; JSXExpressionContainer, which may refer to its JSXAttribute’s
    ;; beginning position (if it has one).
    (rescript-jsx--goto-outermost-enclosing-curly limit)
    (get-text-property (point) 'rescript-jsx-expr-attribute)))

(defvar rescript-jsx--indent-col nil
  "Baseline column for JS indentation within JSX.")

(defvar rescript-jsx--indent-attribute-line nil
  "Line relative to which indentation uses JSX as a baseline.")

(defun rescript-jsx--expr-indentation (parse-status pos col)
  "Indent using PARSE-STATUS; relative to POS, use base COL.
To indent a JSXExpressionContainer’s expression, calculate the JS
indentation, using JSX indentation as the base column when
indenting relative to the beginning line of the
JSXExpressionContainer’s JSXAttribute (if any)."
  (let* ((rescript-jsx--indent-col col)
         (rescript-jsx--indent-attribute-line
          (if pos (line-number-at-pos pos))))
    (rescript--proper-indentation parse-status)))

(defun rescript-jsx--indentation (parse-status)
  "Helper function for `rescript--proper-indentation'.
Return the proper indentation of the current line if it is part
of a JSXElement expression spanning multiple lines; otherwise,
return nil."
  (let ((current-line (line-number-at-pos))
        (curly-pos (rescript-jsx--enclosing-curly-pos))
        nth-context context expr-p beg-line col
        forward-sexp-function) ; Use the Lisp version.
    ;; Find the immediate context for indentation information, but
    ;; keep going to determine that point is at the N+1th line of
    ;; multiline JSX.
    (save-excursion
      (while
          (and
           (setq nth-context (rescript-jsx--context))
           (progn
             (unless context
               (setq context nth-context)
               (setq expr-p (and curly-pos (< (point) curly-pos))))
             (setq beg-line (line-number-at-pos))
             (and
              (= beg-line current-line)
              (or (not curly-pos) (> (point) curly-pos)))))))
    ;; When on the second or later line of JSX, indent as JSX,
    ;; possibly switching back to JS indentation within
    ;; JSXExpressionContainers, possibly using the JSX as a base
    ;; column while switching back to JS indentation.
    (when (and context (> current-line beg-line))
      (save-excursion
        (setq col (rescript-jsx--contextual-indentation current-line context)))
      (if expr-p
          (rescript-jsx--expr-indentation
           parse-status (rescript-jsx--expr-attribute-pos curly-pos (nth 1 context)) col)
        col))))

(defun rescript--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (rescript--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((when (and rescript-jsx-syntax (not rescript-jsx--indent-col))
             (save-excursion (rescript-jsx--indentation parse-status))))
          ((eq (char-after) ?#) 0)
          ((save-excursion (rescript--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (rescript--same-line bracket))
                  (setq beg (rescript--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (rescript--array-comp-indentation bracket beg))))
          ((rescript--chained-expression-p))
          ((rescript--ctrl-statement-indentation))
          ((rescript--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (rescript--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (or (not rescript-indent-align-list-continuation)
                     (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (save-excursion (forward-char) (rescript--broken-arrow-terminates-line-p)))
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (rescript--maybe-goto-declaration-keyword-end parse-status)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (+
                            (cond
                             ((and rescript-jsx--indent-attribute-line
                                   (eq rescript-jsx--indent-attribute-line
                                       (line-number-at-pos)))
                              rescript-jsx--indent-col)
                             (t
                              (current-column)))
                            (cond (same-indent-p 0)
                                  (continued-expr-p
                                   (+ (* 2 rescript-indent-level)
                                      rescript-expr-indent-offset))
                                  (t
                                   (+ rescript-indent-level
                                      (pcase (char-after (nth 1 parse-status))
                                        (?\( rescript-paren-indent-offset)
                                        (?\[ rescript-square-indent-offset)
                                        (?\{ rescript-curly-indent-offset))))))))
                     (if in-switch-p
                         (+ indent rescript-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((rescript--continued-expression-p)
           (+ rescript-indent-level rescript-expr-indent-offset))
          (t (prog-first-column)))))

(defun rescript-indent-line ()
  "Indent the current line as JavaScript."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (unless (nth 3 parse-status)
      (indent-line-to (rescript--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

(defun rescript-jsx-indent-line ()
  "Indent the current line as JavaScript+JSX."
  (interactive)
  (let ((rescript-jsx-syntax t)) (rescript-indent-line)))

;;; Filling

(defvar rescript--filling-paragraph nil)

;; FIXME: Such redefinitions are bad style.  We should try and use some other
;; way to get the same result.
(defun rescript--fill-c-advice (rescript-fun)
  (lambda (orig-fun &rest args)
    (if rescript--filling-paragraph
        (funcall rescript-fun (car args))
      (apply orig-fun args))))

(advice-add 'c-forward-sws
            :around (rescript--fill-c-advice #'rescript--forward-syntactic-ws))
(advice-add 'c-backward-sws
            :around (rescript--fill-c-advice #'rescript--backward-syntactic-ws))
(advice-add 'c-beginning-of-macro
            :around (rescript--fill-c-advice #'rescript--beginning-of-macro))

(define-obsolete-function-alias 'rescript-c-fill-paragraph #'rescript-fill-paragraph "27.1")
(defun rescript-fill-paragraph (&optional justify)
  "Fill the paragraph for Javascript code."
  (interactive "*P")
  (let ((rescript--filling-paragraph t)
        (fill-paragraph-function #'c-fill-paragraph))
    (c-fill-paragraph justify)))

(defun rescript-do-auto-fill ()
  (let ((rescript--filling-paragraph t))
    (c-do-auto-fill)))

;;; Type database and Imenu

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of rescript--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially rescript--initial-pitem.
;;


(defun rescript--pitem-format (pitem)
  (let ((name (rescript--pitem-name pitem))
        (type (rescript--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun rescript--make-merged-item (item child name-parts)
  "Helper function for `rescript--splice-into-items'.
Return a new item that is the result of merging CHILD into
ITEM.  NAME-PARTS is a list of parts of the name of CHILD
that we haven't consumed yet."
  (rescript--debug "rescript--make-merged-item: {%s} into {%s}"
                   (rescript--pitem-format child)
                   (rescript--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (rescript--pitem-type item))
    (rescript--debug "rescript--make-merged-item: changing dest into class")
    (setq item (make-rescript--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (rescript--pitem-type child))
                          rescript--dummy-class-style
                  (rescript--pitem-type child))

                :name (rescript--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (rescript--debug "rescript--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (rescript--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (rescript--pitem-type child))
          (rescript--debug "rescript--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (rescript--debug "rescript--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun rescript--pitem-strname (pitem)
  "Last part of the name of PITEM, as a string or symbol."
  (let ((name (rescript--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun rescript--splice-into-items (items child name-parts)
  "Splice CHILD into the `rescript--pitem' ITEMS at NAME-PARTS.
If a class doesn't exist in the tree, create it.  Return
the new items list.  NAME-PARTS is a list of strings given
the broken-down class name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons)

    (rescript--debug "rescript--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'rescript--pitem-name items))

    (cl-assert (stringp top-name))
    (cl-assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (rescript--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (rescript--make-merged-item
                                             (car item-ptr) child
                                             name-parts)
                                            (cdr item-ptr)))

                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))

                       ;; ...and terminate the loop
                       nil)

                      (t
                       ;; Otherwise, copy the current cons and move onto the
                       ;; text. This is tricky; we keep track of the tail of
                       ;; the list that begins with new-items in
                       ;; last-new-item.
                       (setq new-cons (cons (car item-ptr) nil))
                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))
                       (setq last-new-item new-cons)

                       ;; Go to the next cell in items
                       (setq item-ptr (cdr item-ptr))))))

    (if item-ptr
        ;; Yay! We stopped because we found something, not because
        ;; we ran out of items to search. Just return the new
        ;; list.
        (progn
          (rescript--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (rescript--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-rescript--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (rescript--splice-into-items
                            nil child (cdr name-parts))
                 :type rescript--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun rescript--pitem-add-child (pitem child)
  "Copy `rescript--pitem' PITEM, and push CHILD onto its list of children."
  (cl-assert (integerp (rescript--pitem-h-begin child)))
  (cl-assert (if (consp (rescript--pitem-name child))
              (cl-loop for part in (rescript--pitem-name child)
                       always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (rescript--pitem-name child))
         (type (rescript--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (rescript--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `rescript--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (cl-assert (consp name))
            (rescript--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))

(defun rescript--maybe-make-marker (location)
  "Return a marker for LOCATION if `imenu-use-markers' is non-nil."
  (if imenu-use-markers
      (set-marker (make-marker) location)
    location))

(defun rescript--pitems-to-imenu (pitems unknown-ctr)
  "Convert PITEMS, a list of `rescript--pitem' structures, to imenu format."

  (let (imenu-items pitem pitem-type pitem-name subitems)

    (while (setq pitem (pop pitems))
      (setq pitem-type (rescript--pitem-type pitem))
      (setq pitem-name (rescript--pitem-strname pitem))
      (when (eq pitem-name t)
        (setq pitem-name (format "[unknown %s]"
                                 (cl-incf (car unknown-ctr)))))

      (cond
       ((memq pitem-type '(function macro))
        (cl-assert (integerp (rescript--pitem-h-begin pitem)))
        (push (cons pitem-name
                    (rescript--maybe-make-marker
                     (rescript--pitem-h-begin pitem)))
              imenu-items))

       ((consp pitem-type) ; class definition
        (setq subitems (rescript--pitems-to-imenu
                        (rescript--pitem-children pitem)
                        unknown-ctr))
        (cond (subitems
               (push (cons pitem-name subitems)
                     imenu-items))

              ((rescript--pitem-h-begin pitem)
               (cl-assert (integerp (rescript--pitem-h-begin pitem)))
               (setq subitems (list
                               (cons "[empty]"
                                     (rescript--maybe-make-marker
                                      (rescript--pitem-h-begin pitem)))))
               (push (cons pitem-name subitems)
                     imenu-items))))

       (t (error "Unknown item type: %S" pitem-type))))

    imenu-items))

(defun rescript--imenu-create-index ()
  "Return an imenu index for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (rescript--ensure-cache)
      (cl-assert (or (= (point-min) (point-max))
                  (eq rescript--last-parse-pos (point))))
      (when rescript--last-parse-pos
        (let ((state rescript--state-at-last-parse-pos)
              (unknown-ctr (cons -1 nil)))

          ;; Make sure everything is closed
          (while (cdr state)
            (setq state
                  (cons (rescript--pitem-add-child (cl-second state) (car state))
                        (cddr state))))

          (cl-assert (= (length state) 1))

          ;; Convert the new-finalized state into what imenu expects
          (rescript--pitems-to-imenu
           (car (rescript--pitem-children state))
           unknown-ctr))))))

;; Silence the compiler.
(defvar which-func-imenu-joiner-function)

(defun rescript--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

(defun rescript--imenu-to-flat (items prefix symbols)
  (cl-loop for item in items
           if (imenu--subalist-p item)
           do (rescript--imenu-to-flat
               (cdr item) (concat prefix (car item) ".")
               symbols)
           else
           do (let* ((name (concat prefix (car item)))
                     (name2 name)
                     (ctr 0))

                (while (gethash name2 symbols)
                  (setq name2 (format "%s<%d>" name (cl-incf ctr))))

                (puthash name2 (cdr item) symbols))))

(defun rescript--get-all-known-symbols ()
  "Return a hash table of all JavaScript symbols.
This searches all existing `rescript-mode' buffers. Each key is the
name of a symbol (possibly disambiguated with <N>, where N > 1),
and each value is a marker giving the location of that symbol."
  (cl-loop with symbols = (make-hash-table :test 'equal)
           with imenu-use-markers = t
           for buffer being the buffers
           for imenu-index = (with-current-buffer buffer
                               (when (derived-mode-p 'rescript-mode)
                                 (rescript--imenu-create-index)))
           do (rescript--imenu-to-flat imenu-index "" symbols)
           finally return symbols))

(defvar rescript--symbol-history nil
  "History of entered JavaScript symbols.")

(defun rescript--read-symbol (symbols-table prompt &optional initial-input)
  "Helper function for `rescript-find-symbol'.
Read a symbol from SYMBOLS-TABLE, which is a hash table like the
one from `rescript--get-all-known-symbols', using prompt PROMPT and
initial input INITIAL-INPUT.  Return a cons of (SYMBOL-NAME
. LOCATION), where SYMBOL-NAME is a string and LOCATION is a
marker."
  (unless ido-mode
    (ido-mode 1)
    (ido-mode -1))

  (let ((choice (ido-completing-read
                 prompt
                 (cl-loop for key being the hash-keys of symbols-table
                          collect key)
                 nil t initial-input 'rescript--symbol-history)))
    (cons choice (gethash choice symbols-table))))

(defun rescript--guess-symbol-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (when (eq (char-before) ?.)
          (backward-char)
          (setf (car bounds) (point))))
      (buffer-substring (car bounds) (cdr bounds)))))

(defvar find-tag-marker-ring)           ; etags

;; etags loads ring.
(declare-function ring-insert "ring" (ring item))

(defun rescript-find-symbol (&optional arg)
  "Read a JavaScript symbol and jump to it.
With a prefix argument, restrict symbols to those from the
current buffer.  Pushes a mark onto the tag ring just like
`find-tag'."
  (interactive "P")
  (require 'etags)
  (let (symbols marker)
    (if (not arg)
        (setq symbols (rescript--get-all-known-symbols))
      (setq symbols (make-hash-table :test 'equal))
      (rescript--imenu-to-flat (rescript--imenu-create-index)
                               "" symbols))

    (setq marker (cdr (rescript--read-symbol
                       symbols "Jump to: "
                       (rescript--guess-symbol-at-point))))

    (ring-insert find-tag-marker-ring (point-marker))
    (switch-to-buffer (marker-buffer marker))
    (push-mark)
    (goto-char marker)))

;;; MozRepl integration

(define-error 'rescript-moz-bad-rpc "Mozilla RPC Error") ;; '(timeout error))
(define-error 'rescript-rescript-error "JavaScript Error") ;; '(rescript-error error))

(defun rescript--wait-for-matching-output
  (process regexp timeout &optional start)
  "Wait TIMEOUT seconds for PROCESS to output a match for REGEXP.
On timeout, return nil.  On success, return t with match data
set.  If START is non-nil, look for output starting from START.
Otherwise, use the current value of `process-mark'."
  (with-current-buffer (process-buffer process)
    (cl-loop with start-pos = (or start
                                  (marker-position (process-mark process)))
	     with end-time = (time-add nil timeout)
	     for time-left = (float-time (time-subtract end-time nil))
             do (goto-char (point-max))
             if (looking-back regexp start-pos) return t
             while (> time-left 0)
             do (accept-process-output process time-left nil t)
             do (goto-char (process-mark process))
             finally do (signal
                         'rescript-moz-bad-rpc
                         (list (format "Timed out waiting for output matching %S" regexp))))))

(cl-defstruct rescript--rescript-handle
  ;; Integer, mirrors the value we see in JS
  (id nil :read-only t)

  ;; Process to which this thing belongs
  (process nil :read-only t))

(defun rescript--rescript-handle-expired-p (x)
  (not (eq (rescript--rescript-handle-process x)
           (inferior-moz-process))))

(defvar rescript--rescript-references nil
  "Maps Elisp JavaScript proxy objects to their JavaScript IDs.")

(defvar rescript--rescript-process nil
  "The most recent MozRepl process object.")

(defvar rescript--rescript-gc-idle-timer nil
  "Idle timer for cleaning up JS object references.")

(defvar rescript--rescript-last-gcs-done nil)

(defconst rescript--moz-interactor
  (replace-regexp-in-string
   "[ \n]+" " "
   ; */" Make Emacs happy
"(function(repl) {
  repl.defineInteractor('rescript', {
    onStart: function onStart(repl) {
      if(!repl._jsObjects) {
        repl._jsObjects = {};
        repl._jsLastID = 0;
        repl._jsGC = this._jsGC;
      }
      this._input = '';
    },

    _jsGC: function _jsGC(ids_in_use) {
      var objects = this._jsObjects;
      var keys = [];
      var num_freed = 0;

      for(var pn in objects) {
        keys.push(Number(pn));
      }

      keys.sort(function(x, y) x - y);
      ids_in_use.sort(function(x, y) x - y);
      var i = 0;
      var j = 0;

      while(i < ids_in_use.length && j < keys.length) {
        var id = ids_in_use[i++];
        while(j < keys.length && keys[j] !== id) {
          var k_id = keys[j++];
          delete objects[k_id];
          ++num_freed;
        }
        ++j;
      }

      while(j < keys.length) {
        var k_id = keys[j++];
        delete objects[k_id];
        ++num_freed;
      }

      return num_freed;
    },

    _mkArray: function _mkArray() {
      var result = [];
      for(var i = 0; i < arguments.length; ++i) {
        result.push(arguments[i]);
      }
      return result;
    },

    _parsePropDescriptor: function _parsePropDescriptor(parts) {
      if(typeof parts === 'string') {
        parts = [ parts ];
      }

      var obj = parts[0];
      var start = 1;

      if(typeof obj === 'string') {
        obj = window;
        start = 0;
      } else if(parts.length < 2) {
        throw new Error('expected at least 2 arguments');
      }

      for(var i = start; i < parts.length - 1; ++i) {
        obj = obj[parts[i]];
      }

      return [obj, parts[parts.length - 1]];
    },

    _getProp: function _getProp(/*...*/) {
      if(arguments.length === 0) {
        throw new Error('no arguments supplied to getprop');
      }

      if(arguments.length === 1 &&
         (typeof arguments[0]) !== 'string')
      {
        return arguments[0];
      }

      var [obj, propname] = this._parsePropDescriptor(arguments);
      return obj[propname];
    },

    _putProp: function _putProp(properties, value) {
      var [obj, propname] = this._parsePropDescriptor(properties);
      obj[propname] = value;
    },

    _delProp: function _delProp(propname) {
      var [obj, propname] = this._parsePropDescriptor(arguments);
      delete obj[propname];
    },

    _typeOf: function _typeOf(thing) {
      return typeof thing;
    },

    _callNew: function(constructor) {
      if(typeof constructor === 'string')
      {
        constructor = window[constructor];
      } else if(constructor.length === 1 &&
                typeof constructor[0] !== 'string')
      {
        constructor = constructor[0];
      } else {
        var [obj,propname] = this._parsePropDescriptor(constructor);
        constructor = obj[propname];
      }

      /* Hacky, but should be robust */
      var s = 'new constructor(';
      for(var i = 1; i < arguments.length; ++i) {
        if(i != 1) {
          s += ',';
        }

        s += 'arguments[' + i + ']';
      }

      s += ')';
      return eval(s);
    },

    _callEval: function(thisobj, js) {
      return eval.call(thisobj, js);
    },

    getPrompt: function getPrompt(repl) {
      return 'EVAL>'
    },

    _lookupObject: function _lookupObject(repl, id) {
      if(typeof id === 'string') {
        switch(id) {
        case 'global':
          return window;
        case 'nil':
          return null;
        case 't':
          return true;
        case 'false':
          return false;
        case 'undefined':
          return undefined;
        case 'repl':
          return repl;
        case 'interactor':
          return this;
        case 'NaN':
          return NaN;
        case 'Infinity':
          return Infinity;
        case '-Infinity':
          return -Infinity;
        default:
          throw new Error('No object with special id:' + id);
        }
      }

      var ret = repl._jsObjects[id];
      if(ret === undefined) {
        throw new Error('No object with id:' + id + '(' + typeof id + ')');
      }
      return ret;
    },

    _findOrAllocateObject: function _findOrAllocateObject(repl, value) {
      if(typeof value !== 'object'  && typeof value !== 'function') {
        throw new Error('_findOrAllocateObject called on non-object('
                        + typeof(value) + '): '
                        + value)
      }

      for(var id in repl._jsObjects) {
        id = Number(id);
        var obj = repl._jsObjects[id];
        if(obj === value) {
          return id;
        }
      }

      var id = ++repl._jsLastID;
      repl._jsObjects[id] = value;
      return id;
    },

    _fixupList: function _fixupList(repl, list) {
      for(var i = 0; i < list.length; ++i) {
        if(list[i] instanceof Array) {
          this._fixupList(repl, list[i]);
        } else if(typeof list[i] === 'object') {
          var obj = list[i];
          if(obj.funcall) {
            var parts = obj.funcall;
            this._fixupList(repl, parts);
            var [thisobj, func] = this._parseFunc(parts[0]);
            list[i] = func.apply(thisobj, parts.slice(1));
          } else if(obj.objid) {
            list[i] = this._lookupObject(repl, obj.objid);
          } else {
            throw new Error('Unknown object type: ' + obj.toSource());
          }
        }
      }
    },

    _parseFunc: function(func) {
      var thisobj = null;

      if(typeof func === 'string') {
        func = window[func];
      } else if(func instanceof Array) {
        if(func.length === 1 && typeof func[0] !== 'string') {
          func = func[0];
        } else {
          [thisobj, func] = this._parsePropDescriptor(func);
          func = thisobj[func];
        }
      }

      return [thisobj,func];
    },

    _encodeReturn: function(value, array_as_mv) {
      var ret;

      if(value === null) {
        ret = ['special', 'null'];
      } else if(value === true) {
        ret = ['special', 'true'];
      } else if(value === false) {
        ret = ['special', 'false'];
      } else if(value === undefined) {
        ret = ['special', 'undefined'];
      } else if(typeof value === 'number') {
        if(isNaN(value)) {
          ret = ['special', 'NaN'];
        } else if(value === Infinity) {
          ret = ['special', 'Infinity'];
        } else if(value === -Infinity) {
          ret = ['special', '-Infinity'];
        } else {
          ret = ['atom', value];
        }
      } else if(typeof value === 'string') {
        ret = ['atom', value];
      } else if(array_as_mv && value instanceof Array) {
        ret = ['array', value.map(this._encodeReturn, this)];
      } else {
        ret = ['objid', this._findOrAllocateObject(repl, value)];
      }

      return ret;
    },

    _handleInputLine: function _handleInputLine(repl, line) {
      var ret;
      var array_as_mv = false;

      try {
        if(line[0] === '*') {
          array_as_mv = true;
          line = line.substring(1);
        }
        var parts = eval(line);
        this._fixupList(repl, parts);
        var [thisobj, func] = this._parseFunc(parts[0]);
        ret = this._encodeReturn(
          func.apply(thisobj, parts.slice(1)),
          array_as_mv);
      } catch(x) {
        ret = ['error', x.toString() ];
      }

      var JSON = Components.classes['@mozilla.org/dom/json;1'].createInstance(Components.interfaces.nsIJSON);
      repl.print(JSON.encode(ret));
      repl._prompt();
    },

    handleInput: function handleInput(repl, chunk) {
      this._input += chunk;
      var match, line;
      while(match = this._input.match(/.*\\n/)) {
        line = match[0];

        if(line === 'EXIT\\n') {
          repl.popInteractor();
          repl._prompt();
          return;
        }

        this._input = this._input.substring(line.length);
        this._handleInputLine(repl, line);
      }
    }
  });
})
")

  "String to set MozRepl up into a simple-minded evaluation mode.")

(defun rescript--rescript-encode-value (x)
  "Marshall the given value for JS.
Strings and numbers are JSON-encoded.  Lists (including nil) are
made into JavaScript array literals and their contents encoded
with `rescript--rescript-encode-value'."
  (cond ((stringp x) (json-encode-string x))
        ((numberp x) (json-encode-number x))
        ((symbolp x) (format "{objid:%S}" (symbol-name x)))
        ((rescript--rescript-handle-p x)

         (when (rescript--rescript-handle-expired-p x)
           (error "Stale JS handle"))

         (format "{objid:%s}" (rescript--rescript-handle-id x)))

        ((sequencep x)
         (if (eq (car-safe x) 'rescript--funcall)
             (format "{funcall:[%s]}"
                     (mapconcat #'rescript--rescript-encode-value (cdr x) ","))
           (concat
            "[" (mapconcat #'rescript--rescript-encode-value x ",") "]")))
        (t
         (error "Unrecognized item: %S" x))))

(defconst rescript--rescript-prompt-regexp "\\(repl[0-9]*\\)> $")
(defconst rescript--rescript-repl-prompt-regexp "^EVAL>$")
(defvar rescript--rescript-repl-depth 0)

(defun rescript--rescript-wait-for-eval-prompt ()
  (rescript--wait-for-matching-output
   (inferior-moz-process)
   rescript--rescript-repl-prompt-regexp rescript-rescript-timeout

   ;; start matching against the beginning of the line in
   ;; order to catch a prompt that's only partially arrived
   (save-excursion (forward-line 0) (point))))

;; Presumably "inferior-moz-process" loads comint.
(declare-function comint-send-string "comint" (process string))
(declare-function comint-send-input "comint"
                  (&optional no-newline artificial))

(defun rescript--rescript-enter-repl ()
  (inferior-moz-process) ; called for side-effect
  (with-current-buffer inferior-moz-buffer
    (goto-char (point-max))

    ;; Do some initialization the first time we see a process
    (unless (eq (inferior-moz-process) rescript--rescript-process)
      (setq rescript--rescript-process (inferior-moz-process))
      (setq rescript--rescript-references (make-hash-table :test 'eq :weakness t))
      (setq rescript--rescript-repl-depth 0)

      ;; Send interactor definition
      (comint-send-string rescript--rescript-process rescript--moz-interactor)
      (comint-send-string rescript--rescript-process
                          (concat "(" moz-repl-name ")\n"))
      (rescript--wait-for-matching-output
       (inferior-moz-process) rescript--rescript-prompt-regexp
       rescript-rescript-timeout))

    ;; Sanity check
    (when (looking-back rescript--rescript-prompt-regexp
                        (save-excursion (forward-line 0) (point)))
      (setq rescript--rescript-repl-depth 0))

    (if (> rescript--rescript-repl-depth 0)
        ;; If rescript--rescript-repl-depth > 0, we *should* be seeing an
        ;; EVAL> prompt. If we don't, give Mozilla a chance to catch
        ;; up with us.
        (rescript--rescript-wait-for-eval-prompt)

      ;; Otherwise, tell Mozilla to enter the interactor mode
      (insert (match-string-no-properties 1)
              ".pushInteractor('rescript')")
      (comint-send-input nil t)
      (rescript--wait-for-matching-output
       (inferior-moz-process) rescript--rescript-repl-prompt-regexp
       rescript-rescript-timeout))

    (cl-incf rescript--rescript-repl-depth)))

(defun rescript--rescript-leave-repl ()
  (cl-assert (> rescript--rescript-repl-depth 0))
  (when (= 0 (cl-decf rescript--rescript-repl-depth))
    (with-current-buffer inferior-moz-buffer
      (goto-char (point-max))
      (rescript--rescript-wait-for-eval-prompt)
      (insert "EXIT")
      (comint-send-input nil t)
      (rescript--wait-for-matching-output
       (inferior-moz-process) rescript--rescript-prompt-regexp
       rescript-rescript-timeout))))

(defsubst rescript--rescript-not (value)
  (memq value '(nil null false undefined)))

(defsubst rescript--rescript-true (value)
  (not (rescript--rescript-not value)))

(eval-and-compile
  (defun rescript--optimize-arglist (arglist)
    "Convert immediate js< and js! references to deferred ones."
    (cl-loop for item in arglist
             if (eq (car-safe item) 'rescript<)
             collect (append (list 'list ''rescript--funcall
                                   '(list 'interactor "_getProp"))
                             (rescript--optimize-arglist (cdr item)))
             else if (eq (car-safe item) 'rescript>)
             collect (append (list 'list ''rescript--funcall
                                   '(list 'interactor "_putProp"))

                             (if (atom (cadr item))
                                 (list (cadr item))
                               (list
                                (append
                                 (list 'list ''rescript--funcall
                                       '(list 'interactor "_mkArray"))
                                 (rescript--optimize-arglist (cadr item)))))
                             (rescript--optimize-arglist (cddr item)))
             else if (eq (car-safe item) 'rescript!)
             collect (pcase-let ((`(,_ ,function . ,body) item))
                       (append (list 'list ''rescript--funcall
                                     (if (consp function)
                                         (cons 'list
                                               (rescript--optimize-arglist function))
                                       function))
                               (rescript--optimize-arglist body)))
             else
             collect item)))

(defmacro rescript--rescript-get-service (class-name interface-name)
    `(js! ("Components" "classes" ,class-name "getService")
        (js< "Components" "interfaces" ,interface-name)))

(defmacro rescript--rescript-create-instance (class-name interface-name)
  `(js! ("Components" "classes" ,class-name "createInstance")
        (js< "Components" "interfaces" ,interface-name)))

(defmacro rescript--rescript-qi (object interface-name)
  `(js! (,object "QueryInterface")
        (js< "Components" "interfaces" ,interface-name)))

(defmacro with-js (&rest forms)
  "Run FORMS with the Mozilla repl set up for js commands.
Inside the lexical scope of `with-js', `js?', `js!',
`rescript-new', `rescript-eval', `rescript-list', `js<', `js>', `rescript-get-service',
`rescript-create-instance', and `rescript-qi' are defined."
  (declare (indent 0) (debug t))
  `(progn
     (rescript--rescript-enter-repl)
     (unwind-protect
         (cl-macrolet ((js? (&rest body) `(rescript--rescript-true ,@body))
                       (js! (function &rest body)
                            `(rescript--rescript-funcall
                              ,(if (consp function)
                                   (cons 'list
                                         (rescript--optimize-arglist function))
                                 function)
                              ,@(rescript--optimize-arglist body)))

                       (rescript-new (function &rest body)
                               `(rescript--rescript-new
                                 ,(if (consp function)
                                      (cons 'list
                                            (rescript--optimize-arglist function))
                                    function)
                                 ,@body))

                       (rescript-eval (thisobj js)
                                `(rescript--rescript-eval
                                  ,@(rescript--optimize-arglist
                                     (list thisobj js))))

                       (rescript-list (&rest args)
                                `(rescript--rescript-list
                                  ,@(rescript--optimize-arglist args)))

                       (rescript-get-service (&rest args)
                                       `(rescript--rescript-get-service
                                         ,@(rescript--optimize-arglist args)))

                       (rescript-create-instance (&rest args)
                                           `(rescript--rescript-create-instance
                                             ,@(rescript--optimize-arglist args)))

                       (rescript-qi (&rest args)
                              `(rescript--rescript-qi
                                ,@(rescript--optimize-arglist args)))

                       (js< (&rest body) `(rescript--rescript-get
                                           ,@(rescript--optimize-arglist body)))
                       (js> (props value)
                            `(rescript--rescript-funcall
                              '(interactor "_putProp")
                              ,(if (consp props)
                                   (cons 'list
                                         (rescript--optimize-arglist props))
                                 props)
                              ,@(rescript--optimize-arglist (list value))
                              ))
                       (rescript-handle? (arg) `(rescript--rescript-handle-p ,arg)))
           ,@forms)
       (rescript--rescript-leave-repl))))

(defvar rescript--rescript-array-as-list nil
  "Whether to listify any Array returned by a Mozilla function.
If nil, the whole Array is treated as a JS symbol.")

(defun rescript--rescript-decode-retval (result)
  (pcase (intern (cl-first result))
    ('atom (cl-second result))
    ('special (intern (cl-second result)))
    ('array
     (mapcar #'rescript--rescript-decode-retval (cl-second result)))
    ('objid
     (or (gethash (cl-second result)
                  rescript--rescript-references)
         (puthash (cl-second result)
                  (make-rescript--rescript-handle
                   :id (cl-second result)
                   :process (inferior-moz-process))
                  rescript--rescript-references)))

    ('error (signal 'rescript-rescript-error (list (cl-second result))))
    (x (error "Unmatched case in rescript--rescript-decode-retval: %S" x))))

(defvar comint-last-input-end)

(defun rescript--rescript-funcall (function &rest arguments)
  "Call the Mozilla function FUNCTION with arguments ARGUMENTS.
If function is a string, look it up as a property on the global
object and use the global object for `this'.
If FUNCTION is a list with one element, use that element as the
function with the global object for `this', except that if that
single element is a string, look it up on the global object.
If FUNCTION is a list with more than one argument, use the list
up to the last value as a property descriptor and the last
argument as a function."

  (with-js
   (let ((argstr (rescript--rescript-encode-value
                  (cons function arguments))))

     (with-current-buffer inferior-moz-buffer
       ;; Actual funcall
       (when rescript--rescript-array-as-list
         (insert "*"))
       (insert argstr)
       (comint-send-input nil t)
       (rescript--wait-for-matching-output
        (inferior-moz-process) "EVAL>"
        rescript-rescript-timeout)
       (goto-char comint-last-input-end)

       ;; Read the result
       (let* ((json-array-type 'list)
              (result (prog1 (json-read)
                        (goto-char (point-max)))))
         (rescript--rescript-decode-retval result))))))

(defun rescript--rescript-new (constructor &rest arguments)
  "Call CONSTRUCTOR as a constructor, with arguments ARGUMENTS.
CONSTRUCTOR is a JS handle, a string, or a list of these things."
  (apply #'rescript--rescript-funcall
         '(interactor "_callNew")
         constructor arguments))

(defun rescript--rescript-eval (thisobj js)
  (rescript--rescript-funcall '(interactor "_callEval") thisobj js))

(defun rescript--rescript-list (&rest arguments)
  "Return a Lisp array resulting from evaluating each of ARGUMENTS."
  (let ((rescript--rescript-array-as-list t))
    (apply #'rescript--rescript-funcall '(interactor "_mkArray")
           arguments)))

(defun rescript--rescript-get (&rest props)
  (apply #'rescript--rescript-funcall '(interactor "_getProp") props))

(defun rescript--rescript-put (props value)
  (rescript--rescript-funcall '(interactor "_putProp") props value))

(defun rescript-gc (&optional force)
  "Tell the repl about any objects we don't reference anymore.
With argument, run even if no intervening GC has happened."
  (interactive)

  (when force
    (setq rescript--rescript-last-gcs-done nil))

  (let ((this-gcs-done gcs-done) keys num)
    (when (and rescript--rescript-references
               (boundp 'inferior-moz-buffer)
               (buffer-live-p inferior-moz-buffer)

               ;; Don't bother running unless we've had an intervening
               ;; garbage collection; without a gc, nothing is deleted
               ;; from the weak hash table, so it's pointless telling
               ;; MozRepl about that references we still hold
               (not (eq rescript--rescript-last-gcs-done this-gcs-done))

               ;; Are we looking at a normal prompt? Make sure not to
               ;; interrupt the user if he's doing something
               (with-current-buffer inferior-moz-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (looking-back rescript--rescript-prompt-regexp
                                 (save-excursion (forward-line 0) (point))))))

      (setq keys (cl-loop for x being the hash-keys
                          of rescript--rescript-references
                          collect x))
      (setq num (rescript--rescript-funcall '(repl "_jsGC") (or keys [])))

      (setq rescript--rescript-last-gcs-done this-gcs-done)
      (when (called-interactively-p 'interactive)
        (message "Cleaned %s entries" num))

      num)))

(run-with-idle-timer 30 t #'rescript-gc)

(defun rescript-eval (js)
  "Evaluate the JavaScript in JS and return JSON-decoded result."
  (interactive "MJavaScript to evaluate: ")
  (with-js
   (let* ((content-window (rescript--rescript-content-window
                           (rescript--get-rescript-context)))
          (result (rescript-eval content-window js)))
     (when (called-interactively-p 'interactive)
       (message "%s" (js! "String" result)))
     result)))

(defun rescript--get-tabs ()
  "Enumerate all JavaScript contexts available.
Each context is a list:
   (TITLE URL BROWSER TAB TABBROWSER) for content documents
   (TITLE URL WINDOW) for windows

All tabs of a given window are grouped together.  The most recent
window is first.  Within each window, the tabs are returned
left-to-right."
  (with-js
   (let (windows)

     (cl-loop with window-mediator = (js! ("Components" "classes"
                                           "@mozilla.org/appshell/window-mediator;1"
                                           "getService")
                                          (js< "Components" "interfaces"
                                               "nsIWindowMediator"))
              with enumerator = (js! (window-mediator "getEnumerator") nil)

              while (js? (js! (enumerator "hasMoreElements")))
              for window = (js! (enumerator "getNext"))
              for window-info = (rescript-list window
                                         (js< window "document" "title")
                                         (js! (window "location" "toString"))
                                         (js< window "closed")
                                         (js< window "windowState"))

              unless (or (js? (cl-fourth window-info))
                         (eq (cl-fifth window-info) 2))
              do (push window-info windows))

     (cl-loop for (window title location) in windows
              collect (list title location window)

              for gbrowser = (js< window "gBrowser")
              if (rescript-handle? gbrowser)
              nconc (cl-loop
                     for x below (js< gbrowser "browsers" "length")
                     collect (rescript-list (js< gbrowser
                                           "browsers"
                                           x
                                           "contentDocument"
                                           "title")

                                      (js! (gbrowser
                                            "browsers"
                                            x
                                            "contentWindow"
                                            "location"
                                            "toString"))
                                      (js< gbrowser
                                           "browsers"
                                           x)

                                      (js! (gbrowser
                                            "tabContainer"
                                            "childNodes"
                                            "item")
                                           x)

                                      gbrowser))))))

(defvar rescript-read-tab-history nil)

(declare-function ido-chop "ido" (items elem))

(defun rescript--read-tab (prompt)
  "Read a Mozilla tab with prompt PROMPT.
Return a cons of (TYPE . OBJECT).  TYPE is either `window' or
`tab', and OBJECT is a JavaScript handle to a ChromeWindow or a
browser, respectively."

  ;; Prime IDO
  (unless ido-mode
    (ido-mode 1)
    (ido-mode -1))

  (with-js
   (let ((tabs (rescript--get-tabs)) selected-tab-cname
         selected-tab prev-hitab)

     ;; Disambiguate names
     (setq tabs
           (cl-loop with tab-names = (make-hash-table :test 'equal)
                    for tab in tabs
                    for cname = (format "%s (%s)"
                                        (cl-second tab) (cl-first tab))
                    for num = (cl-incf (gethash cname tab-names -1))
                    if (> num 0)
                    do (setq cname (format "%s <%d>" cname num))
                    collect (cons cname tab)))

     (cl-labels
         ((find-tab-by-cname
           (cname)
           (cl-loop for tab in tabs
                    if (equal (car tab) cname)
                    return (cdr tab)))

          (mogrify-highlighting
           (hitab unhitab)

           ;; Hack to reduce the number of
           ;; round-trips to mozilla
           (let (cmds)
             (cond
              ;; Highlighting tab
              ((cl-fourth hitab)
               (push '(js! ((cl-fourth hitab) "setAttribute")
                       "style"
                       "color: red; font-weight: bold")
                     cmds)

               ;; Highlight window proper
               (push '(js! ((cl-third hitab)
                            "setAttribute")
                       "style"
                       "border: 8px solid red")
                     cmds)

               ;; Select tab, when appropriate
               (when rescript-rescript-switch-tabs
                 (push
                  '(js> ((cl-fifth hitab) "selectedTab") (cl-fourth hitab))
                  cmds)))

              ;; Highlighting whole window
              ((cl-third hitab)
               (push '(js! ((cl-third hitab) "document"
                            "documentElement" "setAttribute")
                       "style"
                       (concat "-moz-appearance: none;"
                               "border: 8px solid red;"))
                     cmds)))

             (cond
              ;; Unhighlighting tab
              ((cl-fourth unhitab)
               (push '(js! ((cl-fourth unhitab) "setAttribute") "style" "")
                     cmds)
               (push '(js! ((cl-third unhitab) "setAttribute") "style" "")
                     cmds))

              ;; Unhighlighting window
              ((cl-third unhitab)
               (push '(js! ((cl-third unhitab) "document"
                            "documentElement" "setAttribute")
                       "style" "")
                     cmds)))

             (eval (list 'with-js
                         (cons 'rescript-list (nreverse cmds))))))

          (command-hook
           ()
           (let* ((tab (find-tab-by-cname (car ido-matches))))
             (mogrify-highlighting tab prev-hitab)
             (setq prev-hitab tab)))

          (setup-hook
           ()
           ;; Fiddle with the match list a bit: if our first match
           ;; is a tabbrowser window, rotate the match list until
           ;; the active tab comes up
           (let ((matched-tab (find-tab-by-cname (car ido-matches))))
             (when (and matched-tab
                        (null (cl-fourth matched-tab))
                        (equal "navigator:browser"
                               (js! ((cl-third matched-tab)
                                     "document"
                                     "documentElement"
                                     "getAttribute")
                                    "windowtype")))

               (cl-loop with tab-to-match = (js< (cl-third matched-tab)
                                                 "gBrowser"
                                                 "selectedTab")

                        for match in ido-matches
                        for candidate-tab = (find-tab-by-cname match)
                        if (eq (cl-fourth candidate-tab) tab-to-match)
                        do (setq ido-cur-list
                                 (ido-chop ido-cur-list match))
                        and return t)))

           (add-hook 'post-command-hook #'command-hook t t)))


       (unwind-protect
           ;; FIXME: Don't impose IDO on the user.
           (setq selected-tab-cname
                 (let ((ido-minibuffer-setup-hook
                        (cons #'setup-hook ido-minibuffer-setup-hook)))
                   (ido-completing-read
                    prompt
                    (mapcar #'car tabs)
                    nil t nil
                    'rescript-read-tab-history)))

         (when prev-hitab
           (mogrify-highlighting nil prev-hitab)
           (setq prev-hitab nil)))

       (add-to-history 'rescript-read-tab-history selected-tab-cname)

       (setq selected-tab (cl-loop for tab in tabs
                                   if (equal (car tab) selected-tab-cname)
                                   return (cdr tab)))

       (cons (if (cl-fourth selected-tab) 'browser 'window)
             (cl-third selected-tab))))))

(defun rescript--guess-eval-defun-info (pstate)
  "Helper function for `rescript-eval-defun'.
Return a list (NAME . CLASSPARTS), where CLASSPARTS is a list of
strings making up the class name and NAME is the name of the
function part."
  (cond ((and (= (length pstate) 3)
              (eq (rescript--pitem-type (cl-first pstate)) 'function)
              (= (length (rescript--pitem-name (cl-first pstate))) 1)
              (consp (rescript--pitem-type (cl-second pstate))))

         (append (rescript--pitem-name (cl-second pstate))
                 (list (cl-first (rescript--pitem-name (cl-first pstate))))))

        ((and (= (length pstate) 2)
              (eq (rescript--pitem-type (cl-first pstate)) 'function))

         (append
          (butlast (rescript--pitem-name (cl-first pstate)))
          (list (car (last (rescript--pitem-name (cl-first pstate)))))))

        (t (error "Function not a toplevel defun or class member"))))

(defvar rescript--rescript-context nil
  "The current JavaScript context.
This is a cons like the one returned from `rescript--read-tab'.
Change with `rescript-set-rescript-context'.")

(defconst rescript--rescript-inserter
  "(function(func_info,func) {
    func_info.unshift('window');
    var obj = window;
    for(var i = 1; i < func_info.length - 1; ++i) {
      var next = obj[func_info[i]];
      if(typeof next !== 'object' && typeof next !== 'function') {
        next = obj.prototype && obj.prototype[func_info[i]];
        if(typeof next !== 'object' && typeof next !== 'function') {
          alert('Could not find ' + func_info.slice(0, i+1).join('.') +
                ' or ' + func_info.slice(0, i+1).join('.') + '.prototype');
          return;
        }

        func_info.splice(i+1, 0, 'prototype');
        ++i;
      }
    }

    obj[func_info[i]] = func;
    alert('Successfully updated '+func_info.join('.'));
  })")

(defun rescript-set-rescript-context (context)
  "Set the JavaScript context to CONTEXT.
When called interactively, prompt for CONTEXT."
  (interactive (list (rescript--read-tab "JavaScript Context: ")))
  (setq rescript--rescript-context context))

(defun rescript--get-rescript-context ()
  "Return a valid JavaScript context.
If one hasn't been set, or if it's stale, prompt for a new one."
  (with-js
   (when (or (null rescript--rescript-context)
             (rescript--rescript-handle-expired-p (cdr rescript--rescript-context))
             (pcase (car rescript--rescript-context)
               ('window (js? (js< (cdr rescript--rescript-context) "closed")))
               ('browser (not (js? (js< (cdr rescript--rescript-context)
                                        "contentDocument"))))
               (x (error "Unmatched case in rescript--get-rescript-context: %S" x))))
     (setq rescript--rescript-context (rescript--read-tab "JavaScript Context: ")))
   rescript--rescript-context))

(defun rescript--rescript-content-window (context)
  (with-js
   (pcase (car context)
     ('window (cdr context))
     ('browser (js< (cdr context)
                    "contentWindow" "wrappedJSObject"))
     (x (error "Unmatched case in rescript--rescript-content-window: %S" x)))))

(defun rescript--make-nsilocalfile (path)
  (with-js
   (let ((file (rescript-create-instance "@mozilla.org/file/local;1"
                                   "nsILocalFile")))
     (js! (file "initWithPath") path)
     file)))

(defun rescript--rescript-add-resource-alias (alias path)
  (with-js
   (let* ((io-service (rescript-get-service "@mozilla.org/network/io-service;1"
                                                "nsIIOService"))
          (res-prot (js! (io-service "getProtocolHandler") "resource"))
          (res-prot (rescript-qi res-prot "nsIResProtocolHandler"))
          (path-file (rescript--make-nsilocalfile path))
          (path-uri (js! (io-service "newFileURI") path-file)))
     (js! (res-prot "setSubstitution") alias path-uri))))

(cl-defun rescript-eval-defun ()
  "Update a Mozilla tab using the JavaScript defun at point."
  (interactive)

  ;; This function works by generating a temporary file that contains
  ;; the function we'd like to insert. We then use the elisp-js bridge
  ;; to command mozilla to load this file by inserting a script tag
  ;; into the document we set. This way, debuggers and such will have
  ;; a way to find the source of the just-inserted function.
  ;;
  ;; We delete the temporary file if there's an error, but otherwise
  ;; we add an unload event listener on the Mozilla side to delete the
  ;; file.

  (save-excursion
    (let (begin end pstate defun-info temp-name defun-body)
      (rescript-end-of-defun)
      (setq end (point))
      (rescript--ensure-cache)
      (rescript-beginning-of-defun)
      (re-search-forward "\\_<function\\_>")
      (setq begin (match-beginning 0))
      (setq pstate (rescript--forward-pstate))

      (when (or (null pstate)
                (> (point) end))
        (error "Could not locate function definition"))

      (setq defun-info (rescript--guess-eval-defun-info pstate))

      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'face 'highlight)
        (unwind-protect
            (unless (y-or-n-p (format "Send %s to Mozilla? "
                                      (mapconcat #'identity defun-info ".")))
              (message "") ; question message lingers until next command
              (cl-return-from rescript-eval-defun))
          (delete-overlay overlay)))

      (setq defun-body (buffer-substring-no-properties begin end))

      (make-directory rescript-rescript-tmpdir t)

      ;; (Re)register a Mozilla resource URL to point to the
      ;; temporary directory
      (rescript--rescript-add-resource-alias "js" rescript-rescript-tmpdir)

      (setq temp-name (make-temp-file (concat rescript-rescript-tmpdir
                                             "/rescript-")
                                      nil ".js"))
      (unwind-protect
          (with-js
            (with-temp-buffer
              (insert rescript--rescript-inserter)
              (insert "(")
              (insert (json-encode-list defun-info))
              (insert ",\n")
              (insert defun-body)
              (insert "\n)")
              (write-region (point-min) (point-max) temp-name
                            nil 1))

            ;; Give Mozilla responsibility for deleting this file
            (let* ((content-window (rescript--rescript-content-window
                                    (rescript--get-rescript-context)))
                   (content-document (js< content-window "document"))
                   (head (if (js? (js< content-document "body"))
                             ;; Regular content
                             (js< (js! (content-document "getElementsByTagName")
                                       "head")
                                  0)
                           ;; Chrome
                           (js< content-document "documentElement")))
                   (elem (js! (content-document "createElementNS")
                              "http://www.w3.org/1999/xhtml" "script")))

              (js! (elem "setAttribute") "type" "text/javascript")
              (js! (elem "setAttribute") "src"
                   (format "resource://js/%s"
                           (file-name-nondirectory temp-name)))

              (js! (head "appendChild") elem)

              (js! (content-window "addEventListener") "unload"
                   (js! ((rescript-new
                          "Function" "file"
                          "return function() { file.remove(false) }"))
                        (rescript--make-nsilocalfile temp-name))
                   'false)
              (setq temp-name nil)



              ))

        ;; temp-name is set to nil on success
        (when temp-name
          (delete-file temp-name))))))

;;; Syntax extensions

(defvar rescript-syntactic-mode-name t
  "If non-nil, print enabled syntaxes in the mode name.")

(defun rescript--syntactic-mode-name-part ()
  "Return a string like “[JSX]” when `rescript-jsx-syntax' is enabled."
  (if rescript-syntactic-mode-name
      (let (syntaxes)
        (if rescript-jsx-syntax (push "JSX" syntaxes))
        (if syntaxes
            (concat "[" (mapconcat #'identity syntaxes ",") "]")
          ""))
    ""))

(defun rescript-use-syntactic-mode-name ()
  "Print enabled syntaxes if `rescript-syntactic-mode-name' is t.
Modes deriving from `rescript-mode' should call this to ensure that
their `mode-name' updates to show enabled syntax extensions."
  (when (stringp mode-name)
    (setq mode-name `(,mode-name (:eval (rescript--syntactic-mode-name-part))))))

(defun rescript-jsx-enable ()
  "Enable JSX in the current buffer."
  (interactive)
  (setq-local rescript-jsx-syntax t))

;; To make discovering and using syntax extensions features easier for
;; users (who might not read the docs), try to safely and
;; automatically enable syntax extensions based on heuristics.

(defvar rescript-jsx-regexps
  (list "\\_<\\(?:var\\|let\\|const\\|import\\)\\_>.*?React")
  "Case-sensitive regexps for detecting JSX in JavaScript buffers.
When `rescript-jsx-detect-syntax' is non-nil and any of these regexps
match text near the beginning of a JavaScript buffer,
`rescript-jsx-syntax' (which see) will be made buffer-local and set to
t.")

(defun rescript-jsx--detect-and-enable (&optional arbitrarily)
  "Detect if JSX is likely to be used, and enable it if so.
Might make `rescript-jsx-syntax' buffer-local and set it to t.  Matches
from the beginning of the buffer, unless optional arg ARBITRARILY
is non-nil.  Return t after enabling, nil otherwise."
  (when (or (and (buffer-file-name)
                 (string-match-p "\\.jsx\\'" (buffer-file-name)))
            (and rescript-jsx-detect-syntax
                 (save-excursion
                   (unless arbitrarily
                     (goto-char (point-min)))
                   (catch 'match
                     (mapc
                      (lambda (regexp)
                        (when (let (case-fold-search)
                                (re-search-forward regexp 4000 t))
                          (throw 'match t)))
                      rescript-jsx-regexps)
                     nil))))
    (rescript-jsx-enable)
    t))

(defun rescript-jsx--detect-after-change (beg end _len)
  "Detect if JSX is likely to be used after a change.
This function is intended for use in `after-change-functions'."
  (when (<= end 4000)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (save-restriction
        (narrow-to-region (point) end)
        (when (rescript-jsx--detect-and-enable 'arbitrarily)
          (remove-hook 'after-change-functions #'rescript-jsx--detect-after-change t))))))

;; Ensure all CC Mode "lang variables" are set to valid values.
;; rescript-mode, however, currently uses only those needed for filling.
(eval-and-compile
  (c-add-language 'rescript-mode 'java-mode))

(c-lang-defconst c-paragraph-start
  rescript-mode "\\(@[[:alpha:]]+\\>\\|$\\)")

;;; Main Function

;;;###autoload
(define-derived-mode rescript-mode prog-mode "JavaScript"
  "Major mode for editing JavaScript."
  :group 'rescript
  ;; Ensure all CC Mode "lang variables" are set to valid values.
  (c-init-language-vars rescript-mode)
  (setq-local indent-line-function #'rescript-indent-line)
  (setq-local beginning-of-defun-function #'rescript-beginning-of-defun)
  (setq-local end-of-defun-function #'rescript-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local font-lock-defaults
              (list rescript--font-lock-keywords nil nil nil nil
                    '(font-lock-syntactic-face-function
                      . rescript-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'rescript-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (add-hook 'syntax-propertize-extend-region-functions
            #'rescript--syntax-propertize-extend-region 'append 'local)
  (setq-local prettify-symbols-alist rescript--prettify-symbols-alist)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local which-func-imenu-joiner-function #'rescript--which-func-joiner)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  (setq-local fill-paragraph-function #'rescript-fill-paragraph)
  (setq-local normal-auto-fill-function #'rescript-do-auto-fill)

  ;; Parse cache
  (add-hook 'before-change-functions #'rescript--flush-caches t t)

  ;; Frameworks
  (rescript--update-quick-match-re)

  ;; Syntax extensions
  (unless (rescript-jsx--detect-and-enable)
    (add-hook 'after-change-functions #'rescript-jsx--detect-after-change nil t))
  (rescript-use-syntactic-mode-name)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (setq imenu-create-index-function #'rescript--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  (c-init-language-vars rescript-mode)
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local comment-multi-line t)
  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
  (setq-local electric-layout-rules
	      '((?\; . after) (?\{ . after) (?\} . before)))

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    ;; While the full CC Mode style system is not yet in use, set the
    ;; pertinent style variables manually.
    (c-initialize-builtin-style)
    (let ((style (cc-choose-style-for-mode 'rescript-mode c-default-style)))
      (c-set-style style))
    (setq c-block-comment-prefix "* "
          c-comment-prefix-regexp "//+\\|\\**")
    (c-setup-paragraph-variables))

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expression literal and the problem
  ;; will mysteriously disappear.
  ;; FIXME: We should instead do this fontification lazily by adding
  ;; calls to syntax-propertize wherever it's really needed.
  ;;(syntax-propertize (point-max))
  )

;; Since we made JSX support available and automatically-enabled in
;; the base `rescript-mode' (for ease of use), now `rescript-jsx-mode' simply
;; serves as one other interface to unconditionally enable JSX in
;; buffers, mostly for backwards-compatibility.
;;
;; Since it is probably more common for packages to integrate with
;; `rescript-mode' than with `rescript-jsx-mode', it is therefore probably
;; slightly better for users to use one of the many other methods for
;; enabling JSX syntax.  But using `rescript-jsx-mode' can’t be that bad
;; either, so we won’t bother users with an obsoletion warning.

;;;###autoload
(define-derived-mode rescript-jsx-mode rescript-mode "JavaScript"
  "Major mode for editing JavaScript+JSX.

Simply makes `rescript-jsx-syntax' buffer-local and sets it to t.

`rescript-mode' may detect and enable support for JSX automatically if
it appears to be used in a JavaScript file.  You could also
customize `rescript-jsx-regexps' to improve that detection; or, you
could set `rescript-jsx-syntax' to t in your init file, or in a
.dir-locals.el file, or using file variables; or, you could call
`rescript-jsx-enable' in `rescript-mode-hook'.  You may be better served by
one of the aforementioned options instead of using this mode."
  :group 'rescript
  (rescript-jsx-enable)
  (rescript-use-syntactic-mode-name))

;;;###autoload (defalias 'javascript-mode 'rescript-mode)

(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'rescript-mode "// {{{" "// }}}" )))

;;;###autoload
(dolist (name (list "node" "nodejs" "gjs" "rhino"))
  (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'rescript-mode)))

(provide 'rescript-indent)

;; rescript-indent.el ends here
