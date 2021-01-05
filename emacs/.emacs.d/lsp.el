;;; lsp.el -- LSP related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(defun tuxee-enable-lsp-breadcrumb ()
  "Enable the LSP breadcrumb without changing the header line."
  (interactive)
  (lsp-headerline-breadcrumb-mode 1)
  ;; This will revert the header change made when enabling the mode.
  (setq header-line-format (remove '(t (:eval lsp-headerline--string)) header-line-format)))

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
  (lsp-idle-delay 0.25)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols))

  :hook
  (lsp-mode . tuxee-enable-lsp-breadcrumb)

  :bind
  ("C-." . 'lsp-find-references)
  ("C-c r" . 'lsp-rename))

;;; lsp.el ends here
