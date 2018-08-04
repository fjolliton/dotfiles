(setq magit-blame-styles '((tuxee-test (margin-width . 20) (margin-format "{tuxee}"))
                           (tuxee-time (margin-width . 16) (margin-format "%A%f"))
                           (tuxee-hash (margin-width . 16) (margin-format "%H%f"))
                           (tuxee-author (margin-width . 16) (margin-format "%a%f"))))

(defun tuxee-color-from-git-hash (hash)
  (let* ((value (string-to-number (substring hash 0 6) 16))
         (color (+ (logand value #x1F1F1F) #x202020)))
    (format "#%06X" (if (= value 0) #x111111 color))))

(defun tuxee-git-initial (name)
  (let ((parts (split-string name)))
    (if parts
        (upcase (if (rest parts)
                    (string-join (map 'list (lambda (part) (subseq part 0 1)) parts) "")
                  (subseq (first parts) 0 2)))
      "??")))

(defun tuxee-magit-custom-blame-format (rev revinfo face)
  (let ((ts (subseq (magit-blame--format-time-string (cdr (assoc "author-time" revinfo))
                                                     (cdr (assoc "author-tz" revinfo)))
                    0 10))
        (hash (subseq rev 0 6))
        (initials (tuxee-git-initial (cdr (assoc "author" revinfo))))
        (color (tuxee-color-from-git-hash rev)))
    (propertize (format "%s %s %s" hash ts initials) 'face (list :foreground "#fff" :background color))))

;;; {tuxee} produces a format like "123ABC 2018-09-08 JD", combining
;;; an abbreviated hash, commit date and author initials.
(defun tuxee-magit-blame-format-string-1-advice (orig rev revinfo format face)
  (if (string= format "{tuxee}")
      (tuxee-magit-custom-blame-format rev revinfo face)
    (apply orig (list rev revinfo format face))))

(advice-add 'magit-blame--format-string-1 :around 'tuxee-magit-blame-format-string-1-advice)
