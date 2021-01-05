;;; misc.el -- Other setup to perform
;;; Commentary:
;;; (no comment)
;;; Code:

;;; Other things that should probably be categorized.

;;; For faster LSP (remove once lsp do it itself properly)
(setq read-process-output-max (* 1024 1024))

(require 'whitespace)
(require 'dired)
(require 'cc-vars)

(use-package htmlize
  :ensure t)

(use-package paredit
  :ensure t)

(use-package evil
  :ensure t)

(use-package elfeed
  :ensure t
  :bind
  ("C-x w" . elfeed))

(use-package ido
  :ensure t
  :config
  (ido-mode)
  :custom
  (ido-enable-flex-matching t)
  ;; Does not seem to have any effects.
  (ido-create-new-buffer 'always))

(use-package buffer-move
  :ensure
  :bind
  ("A-S-<left>" . buf-move-left)
  ("A-S-<right>" . buf-move-right)
  ("A-S-<up>" . buf-move-up)
  ("A-S-<down>" . buf-move-down))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  ("C-S-<mouse-1>" . 'mc/toggle-cursor-on-click))

(use-package rainbow-delimiters
  :ensure t
  :bind
  ("C-<f9>" . 'rainbow-delimiters-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  (defun tuxee-capitalize-first-char (text)
    (concat (upcase (subseq text 0 1)) (subseq text 1))))

(use-package yasnippet-snippets
  :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")

(use-package ace-jump-mode
  :ensure t
  :bind
  ("A-c" . 'ace-jump-mode))

;;; Rarely used. Remove?
(use-package neotree
  :ensure t
  :bind
  ("<f8>" . 'neotree-toggle))

(setq whitespace-line-column 100)

;;; XMonad doesn't support iconifying windows
(when window-system
  (global-unset-key (kbd "C-z")))

;;; Don't use two spaces as sentence separators.
(setq sentence-end-double-space nil)

;;; Let Dired picks the other dired window as target
(setq-default dired-dwim-target t)

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

(defvar tuxee-font-sizes '(nil 200 300 105))

(defun tuxee-font-size-cycle ()
  "Cycle between font sizes."
  (interactive)
  (setq tuxee-font-sizes
        (nconc (cdr tuxee-font-sizes)
               (list (or (car tuxee-font-sizes)
                         (face-attribute 'default :height)))))
  (let ((next-height (car tuxee-font-sizes)))
    (set-face-attribute 'default nil
                        :height next-height)))

(defun tuxee-toggle-explicit-lines ()
  "Show or hide line numbers."
  (interactive)
  (let ((mode (if global-display-line-numbers-mode 0 nil)))
    (global-display-line-numbers-mode mode)
    (global-hl-line-mode mode)))

;;; Extra bindings
(global-set-key (kbd "<f6>") 'kill-this-buffer)
(global-set-key (kbd "<f9>") 'tuxee-toggle-explicit-lines)
(global-set-key (kbd "<f11>") 'whitespace-mode)
(global-set-key (kbd "C-<f11>") 'whitespace-cleanup)
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)
(global-set-key (kbd "A-6") 'delete-indentation) ; Because M-6 (aka M-^) conflicts with XMonad
(global-set-key (kbd "A-<f12>") 'tuxee-font-size-cycle)
(global-set-key (kbd "A-<left>") 'tuxee-shift-left)
(global-set-key (kbd "A-M-<left>") 'previous-buffer)
(global-set-key (kbd "A-<right>") 'tuxee-shift-right)
(global-set-key (kbd "A-M-<right>") 'next-buffer)
(global-set-key (kbd "A-<up>") 'tuxee-move-line-up)
(global-set-key (kbd "A-<down>") 'tuxee-move-line-down)
(global-set-key (kbd "C-<return>") 'tuxee-toggle-selective-display)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;; misc.el ends here
