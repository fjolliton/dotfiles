;;; gnus-init.el -- GNUS init file
;;; Commentary:
;;; (no comment)
;;; Code:

(require 'mm-bodies)

;; To prevent quoted-printable..
(add-to-list 'mm-body-charset-encoding-alist '(utf-8 . 8bit))

;;; gnus-init.el ends here
