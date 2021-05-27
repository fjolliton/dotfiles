;;; org.el -- Org related settings
;;; Commentary:
;;; (no comment)
;;; Code:

(use-package org
  :ensure t
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :config
  (setq org-format-latex-options
        (plist-put (plist-put org-format-latex-options :scale 2.0)
                   :background "Transparent"))
  :custom
  (org-support-shift-select t)
  (org-refile-use-outline-path t)
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 1)
                        (org-buffer-list :maxlevel . 1)))
  (org-capture-templates
   '(("t" "ToDo" entry (file+headline "~/org/home/private/todo.org" "Tasks")
      "\n* TODO %?\n%U\n" :empty-lines 1)
     ("j" "Journal" entry (file "~/org/home/private/journal.org" )
      "\n* %?\n%U\n" :empty-lines 1)
     ("p" "Refile (Private)" entry (file+headline "~/org/home/private/refile.org" "Entries")
      "\n* %?\n%U\n" :empty-lines 1)
     ("r" "Refile (Public)" entry (file+headline "~/org/home/public/refile.org" "Entries")
      "\n* %?\n%U\n" :empty-lines 1))))

(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-screenshot-method "scrot -s %s")
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package org-ql
  :ensure t)

(org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (latex . t) (dot . t)))

(setq-default org-babel-python-command "/home/fred/py3/bin/python3")
(setq-default org-startup-with-inline-images t)

;;; FIXME: Risky?
(defun tuxee-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))

(setq-default org-confirm-babel-evaluate #'tuxee-org-confirm-babel-evaluate)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;;; org.el ends here
