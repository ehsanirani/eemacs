;;; ee-ui.el --- UI configuration for eemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; UI packages and visual polish: themes (modus, ef), ligatures, indent
;; guides, diff-hl, rainbow-delimiters, pulsar, and dired tweaks.

;;; Code:

;; Load color library early to avoid native-comp warnings in theme packages
(require 'color)

;; Theme packages
(use-package modus-themes
  :straight t
  :demand t  ;; Load immediately to ensure modus-themes.el is available before theme files compile
  :init
  (require 'modus-themes))

(use-package ef-themes
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :defer t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package catppuccin-theme
  :straight t
  :config
  (setq catppuccin-flavor 'mocha
        catppuccin-accent 'mauve
        catppuccin-italic-comments t
        catppuccin-colored-modes t
        catppuccin-mode-line-style 'colored)
  (custom-set-faces
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red)))))
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green)))))))

;; Basic UI settings
(use-package emacs
  :init
  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01)

  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)

  ;; Fringes are now set asymmetrically in early-init.el (8px left, 13px right)
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  ;; Fill column indicator with dashed line
  (setq-default fill-column 80)
  (setq display-fill-column-indicator-character ?\u250a)

  (setq x-stretch-cursor t
        blink-cursor-interval 0.5
        blink-cursor-blinks 10
        frame-resize-pixelwise t
        window-divider-default-bottom-width 2
        window-divider-default-right-width 2)

  (setq display-line-numbers-type 'absolute
        display-line-numbers-width 4
        display-line-numbers-widen t)

  (global-display-line-numbers-mode 1)
  (window-divider-mode 1)

  ;; Fill column indicator disabled by default
  ;; Uncomment to enable:
  ;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  ;; (add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-use-momentum t))

  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2))

;; Custom modeline (replaces doom-modeline)
(require 'ee-modeline)
(ee-modeline-mode 1)

;; Ligatures for programming (proper ligature rendering)
(use-package ligature
  :straight t
  :hook (prog-mode . ligature-mode)
  :config
  (defvar ee-ligature-common
    '("<<" ">>" ">>=" "<<=" "<=" ">=" "::" ":::" "..=" "::<" "=="
      "*=" "+=" "<|" "<|>" "|>" "++" "+++" "&&" "||" "/=" "--" "#!"
      "::=" "#[" "]#" "{|" "|}" "__"))
  (defvar ee-ligature-c-like
    '("!=" "<>" "/*" "*/" "//" "///" "^=" "|=" "?." "??" "<~>"))
  (defvar ee-ligature-arrows
    '("<-" "->" "<<-" "->>" "<--" "-->" "<---" "--->" "=>" "<==" "==>"
      "<===" "===>" "<<=" "=>>" "<<==" "==>>" "<->" "<=>" "<~~" "~~>"
      "<-->" "<--->" "<---->" "<==>" "<===>" "<====>"))

  ;; Common ligatures for all modes
  (ligature-set-ligatures 't '("ff" "ffi" "fi" "fj" "fl" "ft" "www"))
  ;; Programming ligatures
  (ligature-set-ligatures '(prog-mode conf-mode) (append ee-ligature-common ee-ligature-arrows))
  ;; C-like language specific
  (ligature-set-ligatures '(c-mode c++-mode rust-mode go-mode) ee-ligature-c-like))

;; Vim-like tab bar
(use-package vim-tab-bar
  :straight t
  :hook (emacs-startup . vim-tab-bar-mode)
  :custom
  (vim-tab-bar-show-groups t))

;; Icons
(use-package nerd-icons
  :straight t
  :config
  (setq nerd-icons-dired-icons-enabled t
        nerd-icons-completion-icons-enabled t)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

;; Visual enhancements
(use-package diff-hl
  :straight t
  :config
  (setq diff-hl-flydiff-mode-threshold 1000
        diff-hl-flydiff-update-delay 0.5
        diff-hl-show-ignore-whitespace t)
  (global-diff-hl-mode 1))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package pulsar
  :straight t
  :config
  (setq pulsar-delay 0.05
        pulsar-face 'pulsar-green
        pulsar-highlight-duration 0.1
        pulsar-pulse-only-if-changed nil
        pulsar-display-source-at-end t)
  (pulsar-global-mode 1)
  (setq pulsar-pulse-functions '(projectile-find-file embark-act ace-jump-char-mode avy-action-goto))
  (add-hook 'evil-yank-hook #'pulsar-pulse-line)
  (add-hook 'isearch-mode-end-hook #'pulsar-pulse-line))

;; Dired
(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-lah --group-directories-first"
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t))

(provide 'ee-ui)
;;; ee-ui.el ends here
