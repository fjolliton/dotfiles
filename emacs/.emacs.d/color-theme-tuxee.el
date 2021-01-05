;;; color-theme-tuxee.el -- Tuxee Color Theme
;;; Commentary:
;;; (no comment)
;;; Code:

(deftheme tuxee "Tuxee color theme")

(let ((bg-color "#1a1a1a")
      (fg-color "#f2f2f2"))
  (custom-theme-set-faces 'tuxee
    `(default ((t (:inherit nil :stipple nil :background ,bg-color :foreground ,fg-color :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 115 :width normal :foundry "PfEd" :family "Inconsolata"))))
    '(border ((t (:background "black"))))
    '(buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
    '(company-tooltip ((t (:background "#242424"))))
    '(company-tooltip-annotation ((t (:foreground "#888"))))
    '(company-tooltip-common ((t (:foreground "#ccc"))))
    '(company-tooltip-selection ((t (:background "#3c3c3c"))))
    '(completions-annotations ((t (:weight bold))))
    '(cursor ((t (:background "#A7A7A7"))))
    '(dired-directory ((t (:foreground "#7ccff2"))))
    '(elfeed-search-date-face ((t (:foreground "#78a"))))
    '(elfeed-search-feed-face ((t (:foreground "#ee5"))))
    '(elfeed-search-tag-face ((t (:foreground "#6d3"))))
    '(elfeed-search-title-face ((t (:foreground "#999"))))
    '(elfeed-search-unread-title-face ((t nil)))
    '(evil-face-state ((t (:foreground "#666"))) t)
    '(evil-face-state-insert ((t (:inherit evil-face-state :foreground "#cc0"))) t)
    '(evil-face-state-normal ((t (:inherit evil-face-state :foreground "#26c"))) t)
    '(evil-face-state-visual ((t (:inherit evil-face-state :foreground "#929"))) t)
    '(evil-state ((t (:inherit nano-face-header-faded :foreground "black"))) t)
    '(evil-state-face ((t (:background "#333333" :foreground "#888888"))) t)
    '(font-lock-builtin-face ((t (:foreground "#f2f2f2" :weight bold))))
    '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#6c6c6c"))))
    '(font-lock-comment-face ((t (:foreground "#9e9e9e" :slant italic))))
    '(font-lock-constant-face ((t (:foreground "#c8f277"))))
    '(font-lock-doc-face ((t (:foreground "#84dcff"))))
    '(font-lock-doc-string-face ((t (:foreground "#84dcff"))))
    '(font-lock-function-name-face ((t (:foreground "#fca21c"))))
    '(font-lock-keyword-face ((t (:foreground "#ffe852"))))
    '(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
    '(font-lock-reference-face ((t (:foreground "SlateBlue"))))
    '(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
    '(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
    '(font-lock-string-face ((t (:foreground "#82d4ff"))))
    '(font-lock-type-face ((t (:foreground "#fc8132"))))
    '(font-lock-variable-name-face ((t (:foreground "#98d542"))))
    '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
    '(gnus-cite-face-1 ((t (:foreground "#ad7fa8"))))
    '(gnus-cite-face-2 ((t (:foreground "sienna4"))))
    '(gnus-cite-face-3 ((t (:foreground "khaki4"))))
    '(gnus-cite-face-4 ((t (:foreground "PaleTurquoise4"))))
    '(gnus-group-mail-1-empty-face ((t (:foreground "light cyan"))))
    '(gnus-group-mail-1-face ((t (:bold t :foreground "light cyan"))))
    '(gnus-group-mail-2-empty-face ((t (:foreground "turquoise"))))
    '(gnus-group-mail-2-face ((t (:bold t :foreground "turquoise"))))
    '(gnus-group-mail-3 ((t (:foreground "white"))))
    '(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :foreground "#bbb"))))
    '(gnus-group-mail-3-empty-face ((t (:foreground "#729fcf"))))
    '(gnus-group-mail-3-face ((t (:bold t :foreground "#edd400"))))
    '(gnus-group-mail-low-empty-face ((t (:foreground "dodger blue"))))
    '(gnus-group-mail-low-face ((t (:bold t :foreground "dodger blue"))))
    '(gnus-group-news-1-empty-face ((t (:foreground "light cyan"))))
    '(gnus-group-news-1-face ((t (:bold t :foreground "light cyan"))))
    '(gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
    '(gnus-group-news-2-face ((t (:bold t :foreground "turquoise"))))
    '(gnus-group-news-3-empty-face ((t (:foreground "#729fcf"))))
    '(gnus-group-news-3-face ((t (:bold t :foreground "#edd400"))))
    '(gnus-group-news-low-empty-face ((t (:foreground "dodger blue"))))
    '(gnus-group-news-low-face ((t (:bold t :foreground "dodger blue"))))
    '(gnus-header-content ((t (:italic t :foreground "#8ae234"))))
    '(gnus-header-from ((t (:bold t :foreground "#edd400"))))
    '(gnus-header-name-face ((t (:bold t :foreground "#729fcf"))))
    '(gnus-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
    '(gnus-header-subject ((t (:foreground "#edd400"))))
    '(gnus-signature-face ((t (:italic t :foreground "dark grey"))))
    '(gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
    '(gnus-summary-high-ancient-face ((t (:bold t :foreground "rotal blue"))))
    '(gnus-summary-high-read-face ((t (:bold t :foreground "lime green"))))
    '(gnus-summary-high-ticked-face ((t (:bold t :foreground "tomato"))))
    '(gnus-summary-high-unread-face ((t (:bold t :foreground "white"))))
    '(gnus-summary-low-ancient-face ((t (:italic t :foreground "lime green"))))
    '(gnus-summary-low-read-face ((t (:italic t :foreground "royal blue"))))
    '(gnus-summary-low-ticked-face ((t (:italic t :foreground "dark red"))))
    '(gnus-summary-low-unread-face ((t (:italic t :foreground "white"))))
    '(gnus-summary-normal-ancient-face ((t (:foreground "royal blue"))))
    '(gnus-summary-normal-read-face ((t (:foreground "lime green"))))
    '(gnus-summary-normal-ticked-face ((t (:foreground "indian red"))))
    '(gnus-summary-normal-unread-face ((t (:foreground "white"))))
    '(gnus-summary-selected ((t (:background "brown4" :foreground "white"))))
    '(gui-element ((t (:background "#D4D0C8" :foreground "black"))))
    '(header-line ((t (:background "#1a1a1a" :foreground "grey70" :box nil :overline nil :underline "#666"))))
    '(highlight ((t (:background "#224477"))))
    '(highline-face ((t (:background "SeaGreen"))))
    '(hl-line ((t (:inherit highlight :extend t :background "#121212"))))
    '(isearch ((t (:background "#ff0" :foreground "black"))))
    '(js2-function-param ((t (:foreground "#9f4"))))
    '(lazy-highlight ((t (:background "#114499" :foreground "white"))))
    '(line-number ((t (:foreground "#666666"))))
    '(line-number-current-line ((t (:inherit line-number :background "#111111" :foreground "#ffffff"))))
    '(link ((t (:foreground "#6fc8f4" :underline t))))
    '(linum ((t (:background "#2c2c2c" :foreground "#555"))))
    '(lsp-face-highlight-textual ((t (:foreground "#f20" :weight bold))))
    '(lsp-face-highlight-write ((t (:background "#005511" :foreground "white"))))
    '(lsp-headerline-breadcrumb-path-face ((t (:foreground "#79a"))))
    '(lsp-headerline-breadcrumb-project-prefix-face ((t (:foreground "#97a"))))
    '(lsp-headerline-breadcrumb-separator-face ((t (:weight bold))))
    '(lsp-headerline-breadcrumb-symbols-face ((t (:foreground "#797"))))
    '(lsp-ui-doc-background ((t (:background "black"))))
    '(lsp-ui-doc-header ((t (:background "#cccccc" :foreground "black" :weight bold))))
    '(magit-blame-highlight ((t (:background "#333" :foreground "#aaa"))))
    '(magit-diff-added ((t (:foreground "#af8"))))
    '(magit-diff-added-highlight ((t (:inherit (magit-diff-added magit-diff-context-highlight)))))
    '(magit-diff-context ((t nil)))
    '(magit-diff-context-highlight ((t (:background "#242424"))))
    '(magit-diff-hunk-heading ((t (:background "#333" :foreground "grey70"))))
    '(magit-diff-hunk-heading-highlight ((t (:background "#444" :foreground "white"))))
    '(magit-diff-removed ((t (:foreground "#f55"))))
    '(magit-diff-removed-highlight ((t (:inherit (magit-diff-removed magit-diff-context-highlight)))))
    '(mc/cursor-bar-face ((t (:height 1 :box (:line-width (2 . 1) :color "red") :background "red"))))
    '(mc/cursor-face ((t (:foreground "#f53" :inverse-video t))))
    '(message-cited-text ((t (:foreground "#edd400"))))
    '(message-header-cc ((t (:foreground "white"))))
    '(message-header-name-face ((t (:foreground "tomato"))))
    '(message-header-newsgroups-face ((t (:italic t :bold t :foreground "LightSkyBlue3"))))
    '(message-header-other-face ((t (:foreground "LightSkyBlue3"))))
    '(message-header-subject ((t (:foreground "white"))))
    '(message-header-to ((t (:foreground "white"))))
    '(message-header-xheader-face ((t (:foreground "DodgerBlue3"))))
    '(minibuffer-prompt ((t (:foreground "#bbb"))))
    `(mode-line ((t (:background ,bg-color :foreground "white" :overline "#444" :height 10))))
    '(mode-line-buffer-id ((t (:foreground "#aaa298" :weight normal))))
    '(mode-line-inactive ((t (:inherit mode-line :overline "#333"))))
    '(nano-face-header-critical ((t (:inherit nano-face-header-popout :background "#921" :foreground "#eee"))) t)
    '(nano-face-header-default ((t (:foreground "#777"))) t)
    '(nano-face-header-faded ((t (:inherit header-line :foreground "#666666"))) t)
    '(nano-face-header-popout ((t (:inherit header-line :background "#37a" :foreground "black"))) t)
    '(neo-dir-link-face ((t (:foreground "#4cf"))))
    '(org-block ((t (:inherit shadow :extend t :foreground "#bbb"))))
    '(org-code ((t (:foreground "#ccc"))))
    '(org-date ((t (:foreground "#9cd" :underline t))))
    '(org-done ((t (:foreground "#7b5"))))
    '(org-headline-done ((t (:foreground "#5af" :strike-through "#38c"))))
    '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.25))))
    '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.13))))
    '(org-tag ((t (:foreground "#999"))))
    '(org-todo ((t (:foreground "#f31"))))
    '(org-verbatim ((t (:inherit shadow :foreground "#ee8866"))))
    '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "#e64"))))
    '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "#5af"))))
    '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#2e9"))))
    '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#e7d"))))
    '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#f14"))))
    '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#ff8"))))
    '(region ((t (:background "#253B76"))))
    '(shadow ((t (:foreground "grey40"))))
    '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#228822"))))
    '(term-color-black ((t (:background "#2e3436" :foreground "#555753"))))
    '(term-color-blue ((t (:background "#3465a4" :foreground "#729fcf"))))
    '(term-color-cyan ((t (:background "#06989a" :foreground "#34e2e2"))))
    '(term-color-green ((t (:background "#4e9a06" :foreground "#8ae234"))))
    '(term-color-magenta ((t (:background "#75507b" :foreground "#ad7fa8"))))
    '(term-color-red ((t (:background "#cc0000" :foreground "#ef2929"))))
    '(term-color-white ((t (:background "#d3d7cf" :foreground "#eeeeec"))))
    '(text-cursor ((t (:background "yellow" :foreground "black"))))
    '(tuareg-font-lock-governing-face ((t (:foreground "#fe7"))))
    '(tuareg-font-lock-operator-face ((t (:foreground "#ddd"))))
    '(variable-pitch ((t (:family "Source Sans Pro"))))
    `(window-divider ((t (:foreground ,bg-color))))
    `(window-divider-first-pixel ((t (:foreground ,bg-color))))
    `(window-divider-last-pixel ((t (:foreground ,bg-color))))))

(provide-theme 'tuxee)

;;; color-theme-tuxee.el ends here
