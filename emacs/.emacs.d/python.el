(require 'lsp-mode)
(require 'flycheck)

(defun tuxee-prioritize-flycheck-checker (name)
  (setq flycheck-checkers (cons name (delete name flycheck-checkers))))

(tuxee-prioritize-flycheck-checker 'python-pycompile)

(flycheck-add-next-checker 'python-pycompile 'python-mypy)

(defconst lsp-python--get-root
  (lsp-make-traverser (lambda (dir)
                        (file-exists-p (concat dir "/setup.py")))))

(lsp-define-stdio-client
 lsp-python
 "python"
 lsp-python--get-root
 '("pyls" "--log-file" "/tmp/pyls.log" "-v" "-v" "-v" "-v")
 ;; See https://github.com/palantir/python-language-server/issues/403
 ;; :extra-init-params '(:pyls (:configurationSources ["flake8"]))
 )

(add-hook 'python-mode-hook 'lsp-python-enable)


(defun lsp-set-cfg ()
  (let ((lsp-cfg `(:pyls (:configurationSources ["flake8"]))))
    ;; TODO: check lsp--cur-workspace here to decide per server / project
    (lsp--set-configuration lsp-cfg)))

(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
