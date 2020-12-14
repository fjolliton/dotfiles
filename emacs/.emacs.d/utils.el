;;; utils.el -- Utilities
;;; Commentary:
;;; (no comment)
;;; Code:

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

;;; utils.el ends here
