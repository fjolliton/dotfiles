;;; We need this commented line to prevent package-initialize to put
;;; it itself.
;(package-initialize)

;;; Note: the order is important.
(defvar tuxee-files
  '("prelude.el"
    "packages.el"
    "color-theme-tuxee.el"
    "style.el"
    "mail.el"
    "company.el"
    "lsp.el"
    "flycheck.el"
    "python.el"
    "javascript.el"
    "magit.el"
    "code.el"
    "misc.el"
    "postlude.el")
  "Initial files to load on start up")

(dolist (file tuxee-files)
  (load (concat user-emacs-directory file)))
(put 'scroll-left 'disabled nil)
