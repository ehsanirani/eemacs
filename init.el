;;; init.el --- eemacs: Ehsan's Emacs Configuration -*- lexical-binding: t; -*-

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable file-name handlers during init
(setq file-name-handler-alist nil)

;; Turn off UI clutter early
(setq inhibit-startup-message t)

;; Load paths
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Core modules
(require 'ee-core)
(require 'ee-completion)
(require 'ee-ui)
(require 'ee-evil)
(require 'ee-editor)

;; Programming
(require 'ee-prog)
(require 'ee-treesit)
(require 'ee-eglot)
(require 'ee-python)
(require 'ee-julia)
(require 'ee-rust)
(require 'ee-nix)

;; Org mode
(require 'ee-org)

;; Markdown support
(require 'ee-markdown)

;; Typst
(require 'ee-typst)

;; Version control
(require 'ee-vc)

;; Terminals
(require 'ee-terminals)

;; RTL languages
(require 'ee-rtl)

;; AI integration
(require 'ee-ai)

;; User configuration
(load (expand-file-name "config.el" user-emacs-directory) nil 'nomessage)

;; Post-init cleanup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(provide 'ee-init)
;;; eemacs init.el ends here
