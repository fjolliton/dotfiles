;;; python.el -- Python related setup
;;; Commentary:
;;; (no comment)
;;; Code:

;;; FIXME: Too specific
(add-to-list 'exec-path "/home/fred/py3/bin")

(use-package blacken
  :ensure t
  :hook
  (python-mode . blacken-mode))

;;; python.el ends here
