;;; flycheck.el -- Flycheck related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package flycheck
  :ensure t
  :hook
  (after-init . global-flycheck-mode))

;;; flycheck.el ends here
