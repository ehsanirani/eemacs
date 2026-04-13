;;; ee-terminals.el --- Terminal configuration for eemacs -*- lexical-binding: t; -*-

;; Vterm terminal emulator
;; On NixOS, vterm (with its native module) is provided by the Nix Emacs
;; package, so we skip straight.el to avoid recompiling from source.
(use-package vterm
  :straight (not (file-exists-p "/etc/NIXOS"))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  :config
  (setq vterm-shell (getenv "SHELL")))

;; Vterm toggle - VS Code style bottom terminal
(use-package vterm-toggle
  :straight t
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :bind
  (("C-`" . vterm-toggle)
   ("C-c T" . vterm-toggle-cd)
   :map vterm-mode-map
   ("C-`" . vterm-toggle))
  :config
  ;; Show vterm at bottom with 30% height
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide 'ee-terminals)