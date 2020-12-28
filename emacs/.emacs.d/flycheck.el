;;; flycheck.el -- Flycheck related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package flycheck
  :ensure t
  :hook
  (prog-mode . global-flycheck-mode))

;;; flycheck.el ends here
