;;; ee-eca.el --- ECA (AI pair-programming) integration for eemacs -*- lexical-binding: t; -*-

;; ECA - AI pair-programming client for Emacs
;; First-time setup: open chat with C-c A e, then type /login to authenticate
;; See: https://github.com/editor-code-assistant/eca-emacs

(use-package eca
  :straight (:type git :host github :repo "editor-code-assistant/eca-emacs")
  :bind ("C-c A e" . eca)
  :custom
  (eca-chat-use-side-window t)
  (eca-chat-window-side 'right))

(provide 'ee-eca)
;;; ee-eca.el ends here
