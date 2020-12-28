;;; company.el -- Company related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind
  ("A-<tab>" . 'company-complete))

;;; company.el ends here
