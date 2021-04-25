;;; rescript-mode-test.el --- Tests for rescript-mode

(require 'ert)
(require 'rescript-mode)

(ert-deftest rescript-mode-hmm ()
  (should t))

(rescript-mode-deftest-indent "Simple.res")
(rescript-mode-deftest-indent "ModulesAndTypes.res")
(rescript-mode-deftest-indent "Exceptions.res")

;;; rescript-mode-test.el ends here
