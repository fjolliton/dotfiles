(setq c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(show-paren-mode 1)
(electric-indent-mode 0)

(defun tuxee-setup-reason ()
  (local-set-key (kbd "C-c C-c") 'refmt))

(add-hook 'reason-mode-hook 'tuxee-setup-reason)

(defun tuxee-find-base (path name)
  "Find the first ancestor of PATH containing a file named NAME."
  (if (file-exists-p (concat (file-name-as-directory path) name))
      path
    (let ((parent (file-name-directory (directory-file-name path))))
      (unless (or (string= parent "/")
                  (string= parent ""))
        (tuxee-find-base parent name)))))

(defun tuxee-find-local-base (name)
  "Search an ancestor directory of the current buffer that contain NAME."
  (let ((here (file-name-directory (buffer-file-name))))
    (tuxee-find-base here name)))

(defun tuxee-smart-exec-path (pattern path)
  "Extend EXEC-PATH with the combination of first ancestor directory that contain PATTERN of the current buffer and PATH."
  (let ((root (tuxee-find-local-base pattern)))
    (when root
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path (concat root "/" path)))))

(defun tuxee-extend-exec-path-for-javascript ()
  "Extend EXEC-PATH for a Javascript buffer."
  (tuxee-smart-exec-path "node_modules" "node_modules/.bin"))

(defun tuxee-extend-exec-path-for-python ()
  "Extend EXEC-PATH for a Python buffer."
  (or (tuxee-smart-exec-path "bin/activate" "bin")
      (tuxee-smart-exec-path "venv/bin/activate" "venv/bin")))

(add-hook 'js-mode-hook 'tuxee-extend-exec-path-for-javascript)
(add-hook 'reason-mode-hook 'tuxee-extend-exec-path-for-javascript)
(add-hook 'python-mode-hook 'tuxee-extend-exec-path-for-python)
