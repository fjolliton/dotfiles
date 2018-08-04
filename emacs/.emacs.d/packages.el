;;; FIXME: Why not look at custom.el instead?
(defvar default-packages
  '(ace-jump-mode
    bbdb
    color-theme
    company
    company-lsp
    flycheck
    flycheck-flow
    glsl-mode
    haskell-mode
    htmlize
    lsp-mode
    lsp-ui
    lsp-ocaml
    markdown-mode
    merlin-eldoc
    neotree
    reason-mode)
  "The list of packages to be present at start up")

;;; Up to Emacs 25, there is no package--initialized, and we should do
;;; the package-initialize to define package-achives first.
(unless (and (boundp 'package--initialized) package--initialized)
  (package-initialize))

;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; FIXME: the list doesn't seem to persist.
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package default-packages)
  (unless (package-installed-p package)
    (package-install package)))
