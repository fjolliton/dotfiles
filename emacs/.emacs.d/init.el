;;; init.el -- Emacs main configuration file
;;; Commentary:
;;; (no comment)
;;; Code:

;;; Note: the order is important.
(defvar tuxee-files
  '("prelude.el"
    "utils.el"
    "color-theme-tuxee.el"
    "style.el"
    "elegant.el"
    "org.el"
    "gnus.el"
    "mail.el"
    "company.el"
    "javascript.el"
    "lsp.el"
    "flycheck.el"
    "python.el"
    "magit.el"
    "code.el"
    "misc.el"
    "postlude.el")
  "Initial files to load on start up.")

(dolist (file tuxee-files)
  (load (concat user-emacs-directory file)))

(put 'scroll-left 'disabled nil)

;;; init.el ends here
