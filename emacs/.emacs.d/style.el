;;;; FIXME: Rename to appearance.el?

(enable-theme 'tuxee)

;;; Remove visual noise
(menu-bar-mode -1) ;; Note: C-mouse-3 to access the menu
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Show column position
(column-number-mode 1)

(visual-line-mode)

(setq inhibit-startup-message t)
(setq blink-cursor-blinks 0)
(setq uniquify-buffer-name-style 'forward)
(setq-default show-trailing-whitespace t)

(defun tuxee-update-cursor-style ()
  "Change the cursor according to the current edit mode."
  (set-cursor-color (if overwrite-mode "#e44" "#ccc")))

(add-hook 'post-command-hook 'tuxee-update-cursor-style)

(modify-frame-parameters (selected-frame) '((right-divider-width . 3)
                                            (bottom-divider-width . 3)))

;;; http://emacs.stackexchange.com/questions/16834/how-to-change-the-title-from-emacshost-to-file-name
(setq-default frame-title-format
  '(:eval (format "%s@%s: %s %s"
                  (or (file-remote-p default-directory 'user)
                      user-real-login-name)
                  (or (file-remote-p default-directory 'host)
                      system-name)
                  (buffer-name)
                  (cond (buffer-file-truename
                         (concat "(" buffer-file-truename ")"))
                        (dired-directory
                         (concat "{" dired-directory "}"))
                        (t
                         "[no file]")))))

;;; Stolen from https://github.com/rougier/elegant-emacs
;;; BEGIN: Elegant Emacs

(tooltip-mode 0)

(setq x-underline-at-descent-line t)

(defun mode-line-render (left right)
  "Function to render the modeline LEFT to RIGHT."
  (let* ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds" available-width) left right)))

(setq-default mode-line-format
  '((:eval
     (mode-line-render
      (format-mode-line
       (list
        (propertize "☰"
                    ;'face `(:inherit mode-line-buffer-id)
                    'help-echo "Mode(s) menu"
                    'mouse-face 'mode-line-highlight
                    'local-map   mode-line-major-mode-keymap)
        " %b "
        (if (and buffer-file-name (buffer-modified-p))
            (propertize "(modified)" 'face `(:inherit face-faded)))))
      (format-mode-line
       (propertize "%4l:%c" 'face `(:inherit face-faded)))))))

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format '(""))

(setq default-frame-alist
      '(;(width . 20) '(height . 8)
        ;(vertical-scroll-bars . nil)
        (internal-border-width . 6)
        ;(font . "Inconsolata 14")
        ))

(set-frame-parameter (selected-frame)
                     'internal-border-width 6)

(setq window-divider-default-right-width 8)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

;;; END: Elegant Emacs
