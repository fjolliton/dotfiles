(projectile-mode +1)

(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-globally-ignored-file-suffixes '("pyc" "whl" "otf" "ttf" "woff" "woff2" "eot"))
(setq projectile-globally-ignored-directories '(".git" ".pyre" "*node_modules" "lib/bs" "out"))
