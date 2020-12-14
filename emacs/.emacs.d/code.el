;;; code.el -- Setup related to code editing
;;; Commentary:
;;; (no comment)
;;; Code:

(require 'cc-vars)

(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(electric-indent-mode 0)
(c-set-offset 'case-label '+)

(add-hook 'c-mode-hook 'lsp)

(use-package xml-mode
  :mode "\\.zcml\\'")

(use-package haskell-mode
  :ensure t)

;;; code.el ends here
