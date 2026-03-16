;;; config.el --- User-specific configuration -*- lexical-binding: t; -*-

;; Personal information
(setq user-full-name "User Name"
      user-mail-address "user.name@youremail.com")

;; Org mode directory
(setq org-directory "~/org")

;; AI Integration (gptel) - API Keys
;; Keys are resolved in order: custom variable → environment variable → agenix secret file.
;; On NixOS + agenix, keys at ~/.config/secrets/ are read automatically — no extra config needed.
;;
;; Option 1: Environment variables (shell config or systemd user env)
;;   export DEEPSEEK_API_KEY="your-deepseek-key-here"
;;   export KIMI_API_KEY="your-kimi-key-here"
;;   export ANTHROPIC_API_KEY="your-anthropic-key-here"
;;
;; Option 2: Set directly in this config file (uncomment and fill in):
;; (setq deepseek-api-key "your-deepseek-key-here")
;; (setq kimi-api-key "your-kimi-key-here")
;; (setq anthropic-api-key "your-anthropic-key-here")
;;
;; Option 3: Agenix secret files (override default paths if needed):
;; (setq ee-ai-deepseek-secret-file "~/.config/secrets/deepseek-api-key")
;; (setq ee-ai-kimi-secret-file "~/.config/secrets/kimi-api-key")
;; (setq ee-ai-anthropic-secret-file "~/.config/secrets/anthropic-api-key")

;; Theme
(load-theme 'catppuccin t)

;; Font configuration
(when (display-graphic-p)
  ;; Set font for current frame
  (set-frame-font "JetBrainsMono Nerd Font-10.5" nil t)
  ;; Set font for future frames
  (add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10.5"))
  ;; Set default font size
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10.5")
  (setq-default line-spacing 2))

;; Personal keybindings
(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (message "Toggle theme placeholder")))

;; Ensure font is applied after theme loads (fixes font size issue)
(defun ehsan/apply-font-after-theme ()
  "Apply font configuration after theme loads."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10.5")))

(add-hook 'after-load-theme-hook #'ehsan/apply-font-after-theme)

;; Also apply font after init
(add-hook 'after-init-hook #'ehsan/apply-font-after-theme)

(provide 'config)
