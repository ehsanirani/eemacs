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
  (windmove-default-keybindings))

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

(provide 'ee-editor)
;;; ee-editor.el ends here