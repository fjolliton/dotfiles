;;; Only works with:
;;; OCaml 4.02.3
;;; Merlin 2.5.4

(require 'lsp-mode)
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'lsp)

(setq typescript-auto-indent-flag nil)

(defconst lsp-javascript--get-root
  (lsp-make-traverser (lambda (dir)
                        (file-exists-p (concat dir "/package.json")))))

(lsp-define-stdio-client
 lsp-javascript-flow
 "javascript"
 lsp-javascript--get-root
 '("flow" "lsp"))

(add-hook 'js-mode-hook 'lsp-javascript-flow-enable)

(lsp-define-stdio-client
 lsp-reason
 "reason"
 lsp-javascript--get-root
 '("ocaml-language-server" "--stdio"))

(add-to-list 'flycheck-checkers 'javascript-eslint)
(add-to-list 'flycheck-checkers 'javascript-flow)
