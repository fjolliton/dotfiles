;;; magit.el -- Magit related setup
;;; Commentary:
;;; (no comment)
;;; Code:

(require 'color)
(require 'cl-lib)

(defun tuxee-color-from-git-hash (value style &optional lightness)
  (let* ((h value)
         (s (cond ((= style 0) .1)
                  ((= style 1) .3)
                  ((= style 2) .3)))
         (l (or lightness (cond ((= style 0) .8)
                                ((= style 1) .3)
                                ((= style 2) .28))))
         (rgb (color-hsl-to-rgb h s l))
         (r (car rgb))
         (g (cadr rgb))
         (b (caddr rgb))
         (color (+ (* (floor (* r 255)) 256 256)
                   (* (floor (* g 255)) 256)
                   (floor (* b 255)))))
    (format "#%06X" color)))

(defun tuxee-git-initial (name)
  (let ((parts (split-string name)))
    (if parts
        (upcase (if (cdr parts)
                    (string-join (mapcar (lambda (part) (substring part 0 1)) parts) "")
                  (substring (concat (car parts) "--") 0 2)))
      "??")))

(defun tuxee-shorten-date (dt)
  (substring dt 0 10))

(defun tuxee-magit-custom-blame-format (rev revinfo face mode empty)
  (if (string= rev (make-string 40 ?0))
      (propertize (if (not empty)
                      "Uncommited          "
                    "                    ")
                  'face (list :foreground "#888" :background "#333"))
    (let* ((dt (magit-blame--format-time-string (cdr (assoc "author-time" revinfo))
                                                (cdr (assoc "author-tz" revinfo))))
           (ts (if empty "          " (tuxee-shorten-date dt)))
           (age (float-time (time-subtract (current-time)
                                           (encode-time 0 0 0
                                                        (string-to-number (substring dt 8 10))
                                                        (string-to-number (substring dt 5 7))
                                                        (string-to-number (substring dt 0 4))))))
           (hash (if empty "      " (substring rev 0 6)))
           (initials (if empty "  " (tuxee-git-initial (cdr (assoc "author" revinfo)))))
           (value (if (eq mode 'author)
                      (/ (% (* 17 (sxhash-equal initials)) 255) 255.0)
                    (/ (string-to-number (substring hash 0 4) 16) 65535.0)))
           (lightness (max 0.6 (min 1.0 (- 1.0 (/ age 8e7)))))
           (fg (tuxee-color-from-git-hash value 0 lightness))
           (bg (tuxee-color-from-git-hash value 1))
           (sep (tuxee-color-from-git-hash value 2)))
      (concat (propertize (if empty "      " hash) 'face (list :foreground fg :background bg))
              (propertize " " 'face (list :foreground fg :background sep))
              (propertize ts 'face (list :foreground fg :background bg))
              (propertize " " 'face (list :foreground fg :background sep))
              (propertize (if empty "  " initials) 'face (list :foreground fg :background bg))))))

;;; {tuxee} produces a format like "123ABC 2018-09-08 JD", combining
;;; an abbreviated hash, commit date and author initials.
(defun tuxee-magit-blame-format-string-1-advice (orig rev revinfo format face)
  (if (or (string= format "{tuxee}")
          (string= format "{tuxee!}")
          (string= format "{tuxee-author}")
          (string= format "{tuxee-author!}"))
      (tuxee-magit-custom-blame-format rev revinfo face
                                       (if (or (string= format "{tuxee}")
                                               (string= format "{tuxee!}"))
                                           'rev
                                         'author)
                                       (or (string= format "{tuxee!}")
                                           (string= format "{tuxee-author!}")))
    (apply orig (list rev revinfo format face))))

(use-package magit
  :ensure t

  :config
  (advice-add 'magit-blame--format-string-1 :around 'tuxee-magit-blame-format-string-1-advice)
  (setq magit-blame-styles '((tuxee-test (margin-width . 20) (margin-format "{tuxee}" "{tuxee!}"))
                           (tuxee-time (margin-width . 16) (margin-format "%A%f"))
                           (tuxee-hash (margin-width . 16) (margin-format "%H%f"))
                           (tuxee-author (margin-width . 16) (margin-format "%a%f"))))

  :bind
  ("<f12>" . 'magit-blame-addition)
  ("C-<f12>" . 'magit-log))

;;; magit.el ends here
