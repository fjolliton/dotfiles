;;; elegant.el -- Derived theme from the "Elegant" and "Nano Emacs" theme by Nicolas P. Rougier
;;;
;;; Commentary:
;;;
;;;   A theme created by mixing idea stolen from
;;;     https://github.com/rougier/elegant-emacs
;;;   and
;;;     https://github.com/rougier/nano-emacs/
;;;
;;; Code:

(defface face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'elegance)

(defface nano-face-header-default nil
  "Default face for ther header line."
  :group 'nano)

(defface nano-face-header-critical nil
  "Critical face for ther header line."
  :group 'nano)

(defface nano-face-header-popout nil
  "Popout face for ther header line."
  :group 'nano)

(defface nano-face-header-faded nil
  "Faded face for ther header line."
  :group 'nano)

(tooltip-mode 0)

(setq x-underline-at-descent-line t)

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  "Function to render the modeline LEFT to RIGHT."
  (let* ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds" available-width) left right)))

(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  (let ((read-only buffer-read-only)
        (modified (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))

(setq-default mode-line-format
  '((:eval
     (mode-line-render
      (format-mode-line
       (let* ((status (nano-modeline-status))
              (prefix (cond ((string= status "RO")
	 		    (propertize " RO " 'face 'nano-face-header-popout))
                            ((string= status "**")
	 		    (propertize " ** " 'face 'nano-face-header-critical))
                            ((string= status "RW")
	 		    (propertize " RW " 'face 'nano-face-header-faded))
                            (t (propertize status 'face 'nano-face-header-popout)))))
         (list
          ;(propertize "☰"
          ;            ;'face `(:inherit mode-line-buffer-id)
          ;            'help-echo "Mode(s) menu"
          ;            'mouse-face 'mode-line-highlight
          ;            'local-map   mode-line-major-mode-keymap)
          prefix
          " %b ")))
      (format-mode-line
       (list
        (propertize "(%m)" 'face 'nano-face-header-default)
        " "
        (propertize "%4l:%c" 'face `(:inherit face-faded))))))))

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format '(""))

(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (internal-border-width . 24)))

(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

(setq window-divider-default-right-width 8)
(setq window-divider-default-places 'right-only)
(window-divider-mode nil)

;;; elegant.el ends here

