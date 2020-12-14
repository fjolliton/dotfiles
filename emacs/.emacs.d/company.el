;;; company.el -- Company related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("A-<tab>" . 'company-complete))

;;; company.el ends here
