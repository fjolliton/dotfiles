;;; python.el -- Python related setup
;;; Commentary:
;;; (no comment)
;;; Code:

;;; FIXME: Too specific
(add-to-list 'exec-path "/home/fred/py3/bin")

(defun tuxee-flycheck-py-setup ()
  (flycheck-add-next-checker 'lsp 'python-mypy))

(use-package blacken
  :ensure t
  :hook
  (python-mode . tuxee-flycheck-py-setup)
  (python-mode . lsp)
  (python-mode . blacken-mode))

;;; python.el ends here
