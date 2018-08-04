;;;; The things to setup before anything else

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq gnus-init-file "~/.emacs.d/gnus.el")

(defvar tuxee-profile
  (if (string-match "\.tuxee\.net$" (system-name)) 'home 'work)
  "The selected configuration profile")
