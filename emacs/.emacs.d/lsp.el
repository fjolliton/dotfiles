(setq lsp-clients-clangd-executable "clangd-10")
(setq lsp-clients-clangd-args '("--background-index"))
(setq lsp-enable-on-type-formatting nil)

(add-hook 'js-mode-hook 'lsp-mode)

(add-hook 'c-mode-hook 'lsp)

;;; Show messages in popup, not in mini-buffer
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(defun tuxee-disable-ui-doc ()
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil))

(add-hook 'lsp-mode-hook 'tuxee-disable-ui-doc)

(defun tuxee-lsp-bindings ()
  (local-set-key (kbd "C-.") 'lsp-find-references)
  (local-set-key (kbd "C-c r") 'lsp-rename))

(add-hook 'lsp-mode-hook 'tuxee-lsp-bindings)
