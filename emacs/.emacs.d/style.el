;;;; FIXME: Rename to appearance.el?

(color-theme-tuxee)

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
