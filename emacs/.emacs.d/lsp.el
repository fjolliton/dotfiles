;;; lsp.el -- LSP related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package lsp-mode
  :ensure t

  :custom
  (lsp-clients-clangd-executable "clangd-9")
  (lsp-clients-clangd-args '("--background-index"))
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 1)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)

  :bind
  ("C-." . 'lsp-find-references)
  ("C-c r" . 'lsp-rename)

  :config
  (defun tuxee-flycheck-setup ()
    (flycheck-add-next-checker 'lsp 'javascript-eslint))
  (add-hook 'lsp-mode-hook 'tuxee-flycheck-setup))

;;; lsp.el ends here
