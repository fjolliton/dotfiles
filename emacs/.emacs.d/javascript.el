;;; javascript.el -- JS/TS related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(defun tuxee-extend-exec-path-for-javascript ()
  "Extend EXEC-PATH for a Javascript buffer."
  (tuxee-smart-exec-path "node_modules" "node_modules/.bin"))

(defun tuxee-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'javascript-eslint))

(use-package prettier-js
  :ensure t
  :hook
  (js-mode . prettier-js-mode)
  (typescript-mode . prettier-js-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.[jt]sx?\\'"
  :hook
  (typescript-mode . tuxee-flycheck-setup)
  (typescript-mode . lsp)
  (typescript-mode . tuxee-extend-exec-path-for-javascript)
  :custom
  (typescript-auto-indent-flag nil))

(use-package js-mode
  :mode "\\.scad\\'")

;;; javascript.el ends here
