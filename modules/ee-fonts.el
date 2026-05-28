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

;;; Multi-language font support

(defun ee-setup-emoji-font ()
  "Setup emoji font fallback.
Iterates candidates in order; first installed family wins. We use
`find-font' (which asks the font backend to resolve a spec) rather
than `(member \"...\" (font-family-list))' because color emoji fonts
on Linux/fontconfig are often resolvable yet absent from
`font-family-list' at startup-hook time. To override, install a
higher-priority candidate or `setq' a custom call earlier."
  (interactive)
  (when (display-graphic-p)
    (catch 'done
      (dolist (font '("Noto Color Emoji"
                      "Twitter Color Emoji"
                      "Twemoji"
                      "JoyPixels"
                      "Emoji One"
                      "Apple Color Emoji"
                      "Segoe UI Emoji"
                      "Symbola"))
        (when (find-font (font-spec :family font))
          (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend)
          (message "Emoji font set to %s" font)
          (throw 'done nil))))))

(defun ee-setup-arabic-font ()
  "Setup Arabic/Persian script font.
Iterates candidates in order; first installed family wins. Vazirmatn
leads because the primary user reads Persian — swap order or extend
this list to taste."
  (interactive)
  (when (display-graphic-p)
    (catch 'done
      (dolist (font '("Vazirmatn"
                      "Amiri"
                      "Noto Sans Arabic"
                      "Noto Naskh Arabic"
                      "Cascadia Code"
                      "DejaVu Sans"))
        (when (find-font (font-spec :family font))
          (set-fontset-font t 'arabic (font-spec :family font) nil)
          (message "Arabic font set to %s" font)
          (throw 'done nil))))))

;;; Apply on startup and theme change

(add-hook 'enable-theme-functions #'ee-tweak-faces)
(add-hook 'emacs-startup-hook #'ee-tweak-faces)
(add-hook 'emacs-startup-hook #'ee-setup-emoji-font)
(add-hook 'emacs-startup-hook #'ee-setup-arabic-font)
(add-hook 'server-after-make-frame-hook #'ee-setup-emoji-font)
(add-hook 'server-after-make-frame-hook #'ee-setup-arabic-font)

;;; Keybindings

(global-set-key (kbd "C-c L") #'ee-toggle-ligatures)
(global-set-key (kbd "C-c M-L") #'ee-toggle-ligatures-global)

(provide 'ee-fonts)
;;; ee-fonts.el ends here
