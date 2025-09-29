;;; config.el --- User-specific configuration -*- lexical-binding: t; -*-

;; Personal information
(setq user-full-name "User Name"
      user-mail-address "user.name@youremail.com")

;; Org mode directory
(setq org-directory "~/org")

;; Theme
(load-theme 'catppuccin t)

;; Font configuration
(when (display-graphic-p)
  ;; Set font for current frame
  (set-frame-font "Fira Code Nerd Font-10.5" nil t)
  ;; Set font for future frames
  (add-to-list 'default-frame-alist '(font . "Fira Code Nerd Font-10.5"))
  ;; Set default font size
  (set-face-attribute 'default nil :font "Fira Code Nerd Font-10.5")
  (setq-default line-spacing 2))

;; Personal keybindings
(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (message "Toggle theme placeholder")))

;; Ensure font is applied after theme loads (fixes font size issue)
(defun ehsan/apply-font-after-theme ()
  "Apply font configuration after theme loads."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "Fira Code Nerd Font-10.5")))

(add-hook 'after-load-theme-hook #'ehsan/apply-font-after-theme)

;; Also apply font after init
(add-hook 'after-init-hook #'ehsan/apply-font-after-theme)

(provide 'config)
