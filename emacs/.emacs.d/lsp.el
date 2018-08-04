(add-hook 'js-mode-hook 'lsp-mode)

;;; Show messages in popup, not in mini-buffer
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
