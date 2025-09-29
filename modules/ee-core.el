;;; ee-core.el --- Core configuration for eemacs -*- lexical-binding: t; -*-

;; Bootstrap straight.el
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         (or (bound-and-true-p straight-base-dir)
                             user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight with use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(require 'use-package)

;; Basic settings
(setq inhibit-startup-message t
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      delete-selection-mode 1
      use-short-answers t
      shift-select-mode nil)

;; Line numbers
(global-display-line-numbers-mode -1)
(setq display-line-numbers-type 'absolute
      column-number-mode t)

;; Recent files
(use-package recentf
  :init (recentf-mode 1)
  :custom (recentf-max-menu-items 25))

;; Save place in files
(save-place-mode 1)

;; Save history
(use-package savehist
  :init (savehist-mode))

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;; Key discovery
(use-package which-key
  :init (which-key-mode)
  :diminish
  :custom (which-key-idle-delay 0.5))

(provide 'ee-core)
;;; ee-core.el ends here
