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
;;
;; Two font choices, defined as variables and reused everywhere:
;;   - `mono'  -> `default' + `fixed-pitch'  (code, UI, code blocks, tables)
;;   - `prose' -> `variable-pitch'           (org/markdown body via `mixed-pitch-mode')
;;
;; To try a different font, change the active `setq' value below (or uncomment
;; an alternate). Re-evaluate the block with C-M-x -- no restart needed.

;; --- Monospace (default + fixed-pitch) -------------------------------------
(setq ehsan/mono-font-family "JetBrainsMono Nerd Font"
      ehsan/mono-font-height 105
      ehsan/mono-font-weight 'normal)
;; Mono alternates -- uncomment one to swap:
;; (setq ehsan/mono-font-family "FiraCode Nerd Font")
;; (setq ehsan/mono-font-family "IBM Plex Mono")

;; --- Proportional (variable-pitch) for prose -------------------------------
;; `ehsan/prose-font-scale' is a *float multiplier* on top of the mono
;; baseline -- 1.0 = same size as code, 1.15 = 15% larger, etc. Stored as a
;; float `:height' on `variable-pitch', which Emacs interprets relative to
;; the underlying face. mixed-pitch-mode then propagates that scale into
;; org/markdown body text (this requires `mixed-pitch-set-height' to be
;; non-nil, which `ee-org.el' sets).
(setq ehsan/prose-font-family "JetBrainsMono Nerd Font"
      ehsan/prose-font-scale  1.0
      ehsan/prose-font-weight 'normal)
;; Prose alternates -- uncomment one to swap:
;; (setq ehsan/prose-font-family "Source Sans 3"  ehsan/prose-font-scale 1.15 ehsan/prose-font-weight 'normal)
;; (setq ehsan/prose-font-family "IBM Plex Serif" ehsan/prose-font-scale 1.15 ehsan/prose-font-weight 'normal)
;; (setq ehsan/prose-font-family "IBM Plex Sans"  ehsan/prose-font-scale 1.15 ehsan/prose-font-weight 'normal)

(defun ehsan/apply-font-after-theme ()
  "Apply fonts from the `ehsan/mono-*' and `ehsan/prose-*' variables.
The mono height is absolute (sets the baseline). The prose face uses a
float `:height', which Emacs treats as a scale factor relative to the
underlying face -- so prose tracks the mono baseline automatically.
Called on startup and after every theme change so themes can't clobber
the face attributes."
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                        :family ehsan/mono-font-family
                        :height ehsan/mono-font-height
                        :weight ehsan/mono-font-weight)
    (set-face-attribute 'fixed-pitch nil
                        :family ehsan/mono-font-family
                        :height ehsan/mono-font-height
                        :weight ehsan/mono-font-weight)
    (set-face-attribute 'variable-pitch nil
                        :family ehsan/prose-font-family
                        :height (float ehsan/prose-font-scale)
                        :weight ehsan/prose-font-weight)))

(when (display-graphic-p)
  (let ((spec (format "%s-%s" ehsan/mono-font-family
                      (/ ehsan/mono-font-height 10.0))))
    (set-frame-font spec nil t)
    (add-to-list 'default-frame-alist (cons 'font spec)))
  (setq-default line-spacing 2)
  (ehsan/apply-font-after-theme))

;; Personal keybindings
(global-set-key (kbd "C-c t") (lambda ()
                                (interactive)
                                (message "Toggle theme placeholder")))

(add-hook 'after-load-theme-hook #'ehsan/apply-font-after-theme)

;; Also apply font after init
(add-hook 'after-init-hook #'ehsan/apply-font-after-theme)

(provide 'config)
