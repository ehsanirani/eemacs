;;; ee-editor.el --- Editor enhancements for eemacs -*- lexical-binding: t; -*-

;; Line wrapping - wrap lines at window edge
(use-package emacs
  :config
  ;; Enable visual line mode globally for better line wrapping
  (global-visual-line-mode 1))

;; Window navigation with Shift + arrow keys
(use-package windmove
  :straight (:type built-in)
  :config
  (windmove-default-keybindings 'shift))

;; Window layout undo/redo — C-c <left> to undo, C-c <right> to redo
(use-package winner
  :straight (:type built-in)
  :hook (after-init . winner-mode))

;; Auto-pairing parentheses, brackets, and quotes
(use-package elec-pair
  :ensure nil
  :config
  (electric-pair-mode 1)
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\' . ?\')
          (?\( . ?\))
          (?\[ . ?\])
          (?\{ . ?\})))
  (setq electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t
        electric-pair-open-newline-between-pairs t
        electric-pair-skip-whitespace nil))

;; Vundo - Visual undo trajectory
(use-package vundo
  :straight t
  :bind (("C-x u" . vundo)
         :map vundo-mode-map
         ("h" . vundo-previous)
         ("l" . vundo-next)
         ("j" . vundo-next)
         ("k" . vundo-previous)
         ("q" . vundo-quit)
         ("RET" . vundo-rollback))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t
        vundo-window-max-height 12
        vundo-window-max-width 60))

;; Auto-revert: automatically reload files changed on disk
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(provide 'ee-editor)
;;; ee-editor.el ends here