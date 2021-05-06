;;; rescript-indent.el --- Indentation functions for ReScript -*-lexical-binding: t-*-

;; Copyright (C) 2008-2020 Free Software Foundation, Inc.
;; Copyright (C) 2021 John Lee <jjl@pobox.com>

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

;; Based heavily (in fact entirely, right now) on js.el from GNU Emacs
;; There is very likely still a lot of code in here that makes little sense for
;; ReScript, and sometimes the indentation it supplies will be just plain wrong.
;;
;; The goal for now is just to get vaguely sensible most of the time when you
;; hit return or tab.  To format your ReScript code properly, use bsc -format.
;;
;; Exported names start with "rescript-"; private names start with
;; "rescript--".


;;; Code:

;;; Constants

(require 'cc-mode)

(defconst rescript--name-start-re "[[:alpha:]_$]"
  "Regexp matching the start of a ReScript identifier, without grouping.")

(defconst rescript--name-re (concat rescript--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a ReScript identifier, without grouping.")

(defconst rescript--objfield-re (concat rescript--name-re ":")
  "Regexp matching the start of a ReScript object field.")

(defconst rescript--dotted-name-re
  (concat rescript--name-re "\\(?:\\." rescript--name-re "\\)*")
  "Regexp matching a dot-separated sequence of ReScript names.")

(defconst rescript--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defun rescript--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

;;; User Customization

(defcustom rescript-indent-level 2
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

;;; Indentation

(defconst rescript--possibly-braceless-keyword-re
  (rescript--regexp-opt-symbol
   '("catch" "else" "finally" "for" "if" "try" "while"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst rescript--declaration-keyword-re
  (regexp-opt '("let") 'words)
  "Regular expression matching variable declaration keywords.")

(defconst rescript--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\|"
          (rescript--regexp-opt-symbol '("in" "to" "downto")))
  "Regexp matching operators that affect indentation of continued expressions.")

(defun rescript--looking-at-operator-p ()
  "Return non-nil if point is on a ReScript operator, other than a comma."
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
                 (memq (char-before) '(?, ?} ?{))))))))

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
             (or (bobp) (backward-char))
             (and (> (point) (point-min))
                  (save-excursion
                    (backward-char)
                    (not (looking-at "[/*]/\\|=>")))
                  (rescript--looking-at-operator-p)
                  (and (progn (backward-char)
                              (not (looking-at "\\+\\+\\|--\\|/[/*]"))))))))))

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
                 (memq (char-before) '(?\s ?\t ?\n ?\}))))
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

(defun rescript--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (rescript--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (rescript--beginning-of-macro)) 4)
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
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at "\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (+
                            (current-column)
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

(provide 'rescript-indent)

;;; rescript-indent.el ends here
