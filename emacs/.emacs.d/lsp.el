;;; lsp.el -- LSP related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(defun tuxee-lsp-breadcrum-advice (orig &rest args)
  (apply orig args)
  ;; This will revert the header change made when enabling the mode.
  (setq header-line-format (remove '(t (:eval lsp-headerline--string)) header-line-format)))

(require 'lsp)
(require 'lsp-ui)
(require 'lsp-eslint)

(advice-add 'lsp-headerline-breadcrumb-mode :around #'tuxee-lsp-breadcrum-advice)

(setq lsp-eslint-package-manager "yarn")
(use-package lsp-mode
  :ensure t

  :custom
  (lsp-clients-clangd-executable "clangd-9")
  (lsp-clients-clangd-args '("--background-index"))
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 1)
  (lsp-ui-sideline-enable t)
  (lsp-completion-provider :capf)
  (lsp-idle-delay 0.25)
  (lsp-headerline-breadcrumb-segments '(project path-up-to-project file))
  (lsp-pylsp-plugins-mccabe-enabled nil)

  :bind
  ("C-." . 'lsp-find-references)
  ("C-c r" . 'lsp-rename)
  ("C-c i" . 'lsp-ui-imenu))

(lsp-register-custom-settings '(("pylsp.plugins.pyls_mypy.enabled" t t)
                                ("pylsp.plugins.pyls_mypy.live_mode" nil t)
                                ("pylsp.plugins.pydocstyle.enabled" nil t)))

(use-package lsp-ui
  :ensure t)

;;; lsp.el ends here
