;;; prelude.el -- Setup to perform at the very beginning
;;; Commentary:
;;; (no comment)
;;; Code:

;;; Temporary set a higher limit on GC threshold
;;; This speeds up Emacs startup a bit.
(setq gc-cons-threshold (max 50000000 gc-cons-threshold))

(setq package-native-compile t)

;;; FIXME: Move somewhere else to stay generic.
(defvar tuxee-profile
  (if (string-match "\.tuxee\.net$" (system-name)) 'home 'work)
  "The selected configuration profile.")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(package-install 'use-package)

;;; prelude.el ends here
