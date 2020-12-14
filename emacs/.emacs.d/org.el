;;; org.el -- Org related settings
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package org
  :ensure t)

(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-screenshot-method "scrot -s %s")
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;;; Usefulness?
;;; Seems that the entire concept is just adding links between org
;;; files. Let's see over time if it worst keeping it.
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate))))

(org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

(setq-default org-babel-python-command "/home/fred/py3/bin/python3")
(setq-default org-startup-with-inline-images t)

;;; FIXME: Risky?
(defun tuxee-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))

(setq-default org-confirm-babel-evaluate #'tuxee-org-confirm-babel-evaluate)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;;; org.el ends here
