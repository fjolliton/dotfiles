(setq magit-blame-styles '((tuxee-test (margin-width . 20) (margin-format "{tuxee}" "{tuxee!}"))
                           (tuxee-time (margin-width . 16) (margin-format "%A%f"))
                           (tuxee-hash (margin-width . 16) (margin-format "%H%f"))
                           (tuxee-author (margin-width . 16) (margin-format "%a%f"))))

(defun tuxee-color-from-git-hash (hash style &optional lightness)
  (let* ((value (string-to-number (substring hash 0 4) 16))
         (h (/ value 65535.0))
         (s (case style (0 .1) (1 .3) (2 .3)))
         (l (or lightness (case style (0 .8) (1 .3) (2 .28))))
         (rgb (color-hsl-to-rgb h s l)))
    (destructuring-bind (r g b) rgb
      (let ((color (+ (* (floor (* r 255)) 256 256)
                      (* (floor (* g 255)) 256)
                      (floor (* b 255)))))
        (format "#%06X" color)))))

(defun tuxee-git-initial (name)
  (let ((parts (split-string name)))
    (if parts
        (upcase (if (rest parts)
                    (string-join (map 'list (lambda (part) (subseq part 0 1)) parts) "")
                  (subseq (concat (first parts) "--") 0 2)))
      "??")))

(defun tuxee-shorten-date (dt)
  (subseq dt 0 10))

(defun tuxee-magit-custom-blame-format (rev revinfo face empty)
  (if (string= rev "0000000000000000000000000000000000000000")
      (propertize (if (not empty)
                      "Uncommited          "
                    "                    ")
                  'face (list :foreground "#888" :background "#333"))
    (let* ((dt (magit-blame--format-time-string (cdr (assoc "author-time" revinfo))
                                                (cdr (assoc "author-tz" revinfo))))
           (ts (if empty "          " (tuxee-shorten-date dt)))
           (age (float-time (time-subtract (current-time)
                                           (encode-time 0 0 0
                                                        (string-to-number (subseq dt 8 10))
                                                        (string-to-number (subseq dt 5 7))
                                                        (string-to-number (subseq dt 0 4))))))
           (hash (if empty "      " (subseq rev 0 6)))
           (initials (if empty "  " (tuxee-git-initial (cdr (assoc "author" revinfo)))))
           (lightness (max 0.6 (min 1.0 (- 1.0 (/ age 8e7)))))
           (fg (tuxee-color-from-git-hash rev 0 lightness))
           (bg (tuxee-color-from-git-hash rev 1))
           (sep (tuxee-color-from-git-hash rev 2)))
      (concat (propertize hash 'face (list :foreground fg :background bg))
              (propertize " " 'face (list :foreground fg :background sep))
              (propertize ts 'face (list :foreground fg :background bg))
              (propertize " " 'face (list :foreground fg :background sep))
              (propertize initials 'face (list :foreground fg :background bg))))))

;;; {tuxee} produces a format like "123ABC 2018-09-08 JD", combining
;;; an abbreviated hash, commit date and author initials.
(defun tuxee-magit-blame-format-string-1-advice (orig rev revinfo format face)
  (if (or (string= format "{tuxee}")
          (string= format "{tuxee!}"))
      (tuxee-magit-custom-blame-format rev revinfo face (string= format "{tuxee!}"))
    (apply orig (list rev revinfo format face))))

(advice-add 'magit-blame--format-string-1 :around 'tuxee-magit-blame-format-string-1-advice)
