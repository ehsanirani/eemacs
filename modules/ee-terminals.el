;;; ee-terminals.el --- Terminal configuration for eemacs -*- lexical-binding: t; -*-

;; Vterm terminal emulator
(use-package vterm
  :straight t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  :config
  (setq vterm-shell (getenv "SHELL")))

;; Vterm toggle - use default behavior
(use-package vterm-toggle
  :straight t
  :after vterm
  :bind
  (("C-c t" . vterm-toggle)
   ("C-c T" . vterm-toggle-cd)
   :map vterm-mode-map
   ("C-c t" . vterm-toggle)))

(provide 'ee-terminals)