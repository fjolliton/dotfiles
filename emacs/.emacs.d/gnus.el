(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq gnus-select-method '(nnnil ""))

(defvar tuxee-gcc-default-folder "nnimap+imap.home:INBOX.Sent")

(defun gnus-guess-current-group ()
  "Read the groups name from the current line. Return NIL if no
such names can be found."
  (save-excursion
    (beginning-of-line)
    (and (re-search-forward "nnimap+.*:\\(.*\\)" (point-max) t)
         (match-string 1))))

(defun gnus-smart-gcc-field ()
  "Return the field value for a Gcc field such that the mail is
stored in the folder corresponding to the current folder."
  (let ((default-folder tuxee-gcc-default-folder))
    (cond
      ;; empty name, probably in Group buffer summary. We extract the
      ;; name of the group from the current line, and revert to the
      ;; default folder otherwise. FIXME: test buffer mode more
      ;; precisely.
      ((string= gnus-newsgroup-name "")
       (or (gnus-guess-current-group)
           default-folder))
      ;; We revert to default folder if it is not a IMAP group
      ((not (string-match "^nnimap+" gnus-newsgroup-name))
       default-folder)
      ;; For all other cases, we keep the newsgroup name.
      (t
       gnus-newsgroup-name))))

;;; Contains gnus-secondary-select-methods and gnus-posting-styles
(load (concat user-emacs-directory "gnus-personal.el") :noerror t)

;;; Do not replace character sequences with funny icons.
(setq gnus-treat-display-smileys nil)

;;; Might also work on terminal, but let's restrict that to graphical
;;; display.
(when window-system
  (setq gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "● "
        gnus-sum-thread-tree-false-root "◯ "
        gnus-sum-thread-tree-single-indent "◎ "
        gnus-sum-thread-tree-leaf-with-other "├─► "
        gnus-sum-thread-tree-vertical "│"
        gnus-sum-thread-tree-single-leaf "╰─► "))

(setq gnus-summary-line-format "%U%R%z %d │ %5V │ %4L │ %1{%B%}%*%(%-20,20n%) │ %s\n")

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;;-- Encoding -----------------------------------------------------------------

;; Preferred encoding: ASCII -> UTF8
;; FIXME: Don't work for encrypted mail.
;; FIXME: Replace this setq by a loop which also set m-b-c-e-alist?
(setq mm-coding-system-priorities '(ascii utf-8))

;; To prevent quoted-printable..
(add-to-list 'mm-body-charset-encoding-alist '(utf-8 . 8bit))

;;
;; Prevent use of qp-or-base64.
;;
;; This is probably bad for some cases, but at least long lines are
;; not folded at 75 characters in the raw contents of the mail.
;;
(setq mm-content-transfer-encoding-defaults
      '(("text/plain" 8bit)
        (".*" base64)))

(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))
