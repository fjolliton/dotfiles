(setq lsp-clients-clangd-executable "clangd-10")
(setq lsp-clients-clangd-args '("--background-index"))
(setq lsp-enable-on-type-formatting nil)

(add-hook 'js-mode-hook 'lsp-mode)

(add-hook 'c-mode-hook 'lsp)

;;; Show messages in popup, not in mini-buffer
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
