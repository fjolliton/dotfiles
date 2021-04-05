;;; flycheck.el -- Flycheck related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(defun tuxee-disable-truncate-lines ()
  "Enable truncated lines."
  (toggle-truncate-lines 0))

(use-package flycheck
  :ensure t
  :hook
  (flycheck-error-list-mode . tuxee-disable-truncate-lines)
  (prog-mode . global-flycheck-mode))

;;; flycheck.el ends here
