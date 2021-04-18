(require 'lsp-mode)
(require 'rescript-mode)

(defcustom rescript-mode-lsp-server-command '()
  "Full command to run the ReScript language server."
  :group 'rescript
  :risky t
  :type '(repeat string))

(defcustom rescript-mode-prompt-for-build t
  "Non-nil means prompt for a build."
  :group 'rescript
  :risky t
  :type 'boolean)

(defun rescript-mode--hash-table-symbol-value-items (table)
  (let (results)
    (maphash
     (lambda (key value)
       (setf results (append results (list (intern (concat ":" key)) value))))
     table)
    results))

(lsp-defun rescript-mode--window-log-message-request ((&ShowMessageRequestParams :message :type :actions?))
  "Display a message request to the user and send the user's selection back to the server."
  ;; rescript-vscode arranges via an LSP request to give you an interactive
  ;; prompt about whether you want to start a build.  This differs from the
  ;; upstream lsp-mode implementation in also sending back any additional
  ;; parameters sent with the request.  In particular, the rescript-vscode LSP
  ;; server wants to see projectRootPath when it processes the response from the
  ;; client.  This sends back all of the parameters instead of only `title' as
  ;; vanilla lsp-mode.el does.
  (let* ((message (lsp--propertize message type))
          (choices (--map (gethash "title" it) actions?)))
    (if choices
        (let* ((selected (completing-read (concat message " ") choices nil t))
                (ht (car (--filter (equal (gethash "title" it) selected) (append actions? nil))))
                (response (rescript-mode--hash-table-symbol-value-items ht)))
          response)
      (lsp-log message))))

(defun rescript-mode--handle-show-message-request (workspace params)
  (rescript-mode--window-log-message-request params))

(defun rescript-mode-register-with-lsp-mode ()
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection rescript-mode-lsp-server-command)
                    :major-modes '(rescript-mode)
                    :notification-handlers (ht ("client/registerCapability" #'ignore))
                    :request-handlers (ht("window/showMessageRequest"
                                          (if rescript-mode-prompt-for-build
                                              #'rescript-mode--handle-show-message-request
                                            #'ignore)))
                    :priority 1
                    :server-id 'rescript-ls))
  (add-to-list 'lsp-language-id-configuration '(rescript-mode . "rescript")))

(provide 'rescript-lsp-mode-config)

;;; rescript-lsp-mode-config.el ends here
