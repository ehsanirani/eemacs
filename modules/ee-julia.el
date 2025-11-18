;;; ee-julia.el --- Julia configuration -*- lexical-binding: t; -*-

;; Ensure Julia is in exec-path
(add-to-list 'exec-path (expand-file-name "~/.juliaup/bin"))

;; Julia LSP client
(use-package eglot-jl
  :straight t)

(setq eglot-ignored-server-capabilities '(:inlayHintProvider))

;; Julia major mode
(use-package julia-mode
  :straight t
  :interpreter "julia"
  :config
  (defun my-julia-eglot-setup ()
    "Set longer timeout for Julia LSP."
    (setq-local eglot-connect-timeout 1000)
    (eglot-jl-init))
  (add-hook 'julia-mode-hook 'my-julia-eglot-setup)
  (add-hook 'julia-mode-hook 'eglot-ensure))

;; Julia REPL integration
(use-package julia-repl
  :straight t)

(provide 'ee-julia)
