;;; ee-vc.el --- Version control configuration for eemacs -*- lexical-binding: t; -*-

;; Git interface
(use-package magit
  :straight t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)
  (magit-refs-show-commit-count 'all)
  (magit-revision-show-gravatars nil)
  :config
  ;; Integrate with diff-hl for better visual feedback
  (with-eval-after-load 'diff-hl
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(provide 'ee-vc)