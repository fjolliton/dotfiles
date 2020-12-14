;;; javascript.el -- JS/TS related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(defun tuxee-extend-exec-path-for-javascript ()
  "Extend EXEC-PATH for a Javascript buffer."
  (tuxee-smart-exec-path "node_modules" "node_modules/.bin"))

(use-package prettier-js
  :ensure t
  :hook
  (js-mode . prettier-js-mode)
  (typescript-mode . prettier-js-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx\\'"
  :hook
  (typescript-mode . tuxee-extend-exec-path-for-javascript)
  (typescript-mode . lsp)
  :custom
  (typescript-auto-indent-flag nil))

;;; javascript.el ends here
