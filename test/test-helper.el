;;; test-helper.el --- Helpers for rescript-mode-test.el

;; This resource file stuff is from ert-x.el and macroexp.el from Emacs git 6dabbdd
(defun rescript-mode-tests--file-name ()
  ;; `eval-buffer' binds `current-load-list' but not `load-file-name',
  ;; so prefer using it over using `load-file-name'.
  (let ((file (car (last current-load-list))))
    (or (if (stringp file) file)
        (bound-and-true-p byte-compile-current-file))))

;; Has to be a macro for `load-file-name'.
(defmacro rescript-mode-tests--resource-directory ()
  `(let* ((testfile ,(or (rescript-mode-tests--file-name)
                         buffer-file-name))
          (default-directory (file-name-directory testfile)))
     (file-truename
      (if (file-accessible-directory-p "resources/")
          (expand-file-name "resources/")
        (expand-file-name
         (format "%s-resources/"
                 (string-trim testfile
                              ""
                              "\\(-tests?\\)?\\.el")))))))

(defmacro rescript-mode-tests--resource-file (file)
  `(expand-file-name ,file (rescript-mode-tests--resource-directory)))


;; This is from js.el's own tests
(defun rescript-mode-tests--remove-indentation ()
  "Remove all indentation in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward (rx bol (+ (in " \t"))) nil t)
    (let ((syntax (save-match-data (syntax-ppss))))
      (unless (nth 3 syntax)       ; Avoid multiline string literals.
        (replace-match "")))))

(defmacro rescript-mode-deftest-indent (file)
  `(ert-deftest ,(intern (format "rescript-mode-indent-test/%s" file)) ()
     :tags '(:expensive-test)
     (let ((buf (find-file-noselect (rescript-mode-tests--resource-file ,file))))
       (unwind-protect
           (with-current-buffer buf
             (let ((orig (buffer-string)))
               (rescript-mode-tests--remove-indentation)
               ;; Indent and check that we get the original text.
               (indent-region (point-min) (point-max))
               (should (equal (buffer-string) orig))
               ;; Verify idempotency.
               (indent-region (point-min) (point-max))
               (should (equal (buffer-string) orig))))
         (kill-buffer buf)))))

;;; test-helper.el ends here
