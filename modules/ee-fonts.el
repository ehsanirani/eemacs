;;; ee-fonts.el --- Font and face configuration for eemacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Font utilities and face tweaks inspired by MinEmacs.
;; Ligatures are managed by the `ligature' package in ee-ui.el.
;;
;; NOTE: For italic keywords to display, your font must have italic variants!
;; - FiraCode does NOT have italics
;; - JetBrains Mono HAS italics (recommended)
;; - Cascadia Code HAS italics
;; - Victor Mono HAS cursive italics
;; - Iosevka HAS italics

;;; Code:

(defgroup ee-fonts nil
  "Font configuration for eemacs."
  :group 'faces)

;;; Ligature toggle (uses ligature-mode from ligature package)

(defun ee-toggle-ligatures ()
  "Toggle ligatures in current buffer (ligature-mode)."
  (interactive)
  (if (bound-and-true-p ligature-mode)
      (progn
        (ligature-mode -1)
        (message "Ligatures DISABLED"))
    (progn
      (ligature-mode 1)
      (message "Ligatures ENABLED"))))

(defun ee-toggle-ligatures-global ()
  "Toggle ligatures globally in all programming buffers."
  (interactive)
  (if (bound-and-true-p global-ligature-mode)
      (progn
        (global-ligature-mode -1)
        (message "Ligatures DISABLED globally"))
    (progn
      (global-ligature-mode 1)
      (message "Ligatures ENABLED globally"))))

;;; Face tweaks (MinEmacs style)

(defun ee-tweak-faces (&optional _theme)
  "Apply subtle face tweaks for better code readability.
Makes keywords italic, types semi-bold, etc. (MinEmacs style).

NOTE: Your font must support italic variants for this to work!
FiraCode does NOT have italics - use JetBrains Mono, Cascadia Code, or Victor Mono."
  (interactive)
  (when (display-graphic-p)
    ;; Builtin faces - medium weight, normal slant
    (set-face-attribute 'font-lock-builtin-face nil :weight 'medium :slant 'normal)
    ;; Keywords - italic (the key visual distinction)
    (set-face-attribute 'font-lock-keyword-face nil :weight 'medium :slant 'italic)
    ;; Types - semi-bold
    (set-face-attribute 'font-lock-type-face nil :weight 'semi-bold)
    ;; Numbers - semi-bold (Emacs 29+)
    (when (facep 'font-lock-number-face)
      (set-face-attribute 'font-lock-number-face nil :weight 'semi-bold))
    ;; Function names - medium weight
    (set-face-attribute 'font-lock-function-name-face nil :weight 'medium :slant 'normal)
    ;; Function calls - medium weight (Emacs 29+)
    (when (facep 'font-lock-function-call-face)
      (set-face-attribute 'font-lock-function-call-face nil :weight 'medium :slant 'normal))
    ;; Eglot symbol highlight
    (with-eval-after-load 'eglot
      (set-face-attribute 'eglot-highlight-symbol-face nil :underline t))
    (message "Face tweaks applied (italic keywords, semi-bold types)")))

;;; Subtle mode-line (MinEmacs style)

(defun ee-subtle-mode-line (&rest _args)
  "Apply subtle look for the mode-line (MinEmacs style)."
  (interactive)
  (when (display-graphic-p)
    (set-face-attribute
     'mode-line-active nil
     :box `(:line-width 4 :color ,(face-attribute 'default :background nil t) :style nil)
     :overline (face-attribute 'default :foreground nil t)
     :background (face-attribute 'default :background nil t))
    (set-face-attribute
     'mode-line-inactive nil
     :box `(:line-width 4 :color ,(face-attribute 'mode-line-inactive :background nil t) :style nil)
     :overline (face-attribute 'mode-line-inactive :foreground nil t))
    (message "Subtle mode-line applied")))

;;; Multi-language font support

(defun ee-setup-emoji-font ()
  "Setup emoji font fallback."
  (interactive)
  (when (display-graphic-p)
    (dolist (font '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji"))
      (when (member font (font-family-list))
        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
        (message "Emoji font set to %s" font)
        (cl-return)))))

(defun ee-setup-arabic-font ()
  "Setup Arabic script font."
  (interactive)
  (when (display-graphic-p)
    (dolist (font '("Amiri" "Cascadia Code" "DejaVu Sans"))
      (when (member font (font-family-list))
        (set-fontset-font t 'arabic (font-spec :family font) nil)
        (message "Arabic font set to %s" font)
        (cl-return)))))

;;; Apply on startup and theme change

(add-hook 'enable-theme-functions #'ee-tweak-faces)
(add-hook 'emacs-startup-hook #'ee-tweak-faces)
(add-hook 'emacs-startup-hook #'ee-subtle-mode-line)
(add-hook 'enable-theme-functions #'ee-subtle-mode-line)

;;; Keybindings

(global-set-key (kbd "C-c L") #'ee-toggle-ligatures)
(global-set-key (kbd "C-c M-L") #'ee-toggle-ligatures-global)

(provide 'ee-fonts)
;;; ee-fonts.el ends here
