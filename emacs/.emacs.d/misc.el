;;; Other things that should probably be categorized.

(which-key-mode)

;;; XMonad doesn't support iconifying windows
(when window-system
  (global-unset-key (kbd "C-z")))

;;; Don't use two spaces as sentence separators.
(setq sentence-end-double-space nil)

;;; Let Dired picks the other dired window as target
(setq dired-dwim-target t)

(defun tuxee-move-line-up ()
  "Move the current line above the previous one."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (forward-char col)))

(defun tuxee-move-line-down ()
  "Move the current line below the next one."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (forward-char col)))

(defun tuxee-toggle-selective-display ()
  "Toggle selective display."
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun tuxee-shift (amount)
  "Shift text by AMOUNT characters."
  (indent-rigidly (mark) (point) amount))

(defun tuxee-shift-left (arg)
  "Shift text to the left by ARG characters."
  (interactive "P")
  (tuxee-shift (- (or arg c-basic-offset))))

(defun tuxee-shift-right (arg)
  "Shift text to the right by ARG characters."
  (interactive "P")
  (tuxee-shift (or arg c-basic-offset)))

;;; Lazy set because we need to load custom.el first which set the
;;; font size.
(defvar tuxee-default-face-height nil)

(defvar tuxee-prez-face-height 220)

(defun tuxee-prez ()
  "Toggle between two font sizes."
  (interactive)
  (unless tuxee-default-face-height
    (setq tuxee-default-face-height (face-attribute 'default :height)))
  (set-face-attribute 'default nil
                      :height (if (/= (face-attribute 'default :height) tuxee-default-face-height)
                                  tuxee-default-face-height
                                tuxee-prez-face-height)))

;;; Extra bindings
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)

(global-set-key (kbd "A-6") 'delete-indentation) ; Because M-6 (aka M-^) conflicts with XMonad
(global-set-key (kbd "A-<down>") 'tuxee-move-line-down)
(global-set-key (kbd "A-<left>") 'tuxee-shift-left)
(global-set-key (kbd "A-<right>") 'tuxee-shift-right)
(global-set-key (kbd "A-<up>") 'tuxee-move-line-up)
(global-set-key (kbd "A-c") 'ace-jump-mode)
(global-set-key (kbd "C-<return>") 'tuxee-toggle-selective-display)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-a") 'align-regexp)

(global-set-key (kbd "<f6>") 'kill-this-buffer)
(global-set-key (kbd "<f8>") 'neotree-toggle)
(global-set-key (kbd "<f11>") 'whitespace-mode)
(global-set-key (kbd "C-<f11>") 'whitespace-cleanup)
(global-set-key (kbd "<f12>") 'magit-blame)
(global-set-key (kbd "A-<f12>") 'tuxee-prez)
(global-set-key (kbd "C-<f12>") 'magit-log)
