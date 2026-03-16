;;; ee-ai.el --- AI integration for eemacs -*- lexical-binding: t; -*-

;; API Key Configuration — resolved in order:
;; 1. Custom variable set in config.el  (setq deepseek-api-key "sk-...")
;; 2. Environment variable              (DEEPSEEK_API_KEY, KIMI_API_KEY, ANTHROPIC_API_KEY)
;; 3. Agenix secret file on disk        (e.g. ~/.config/secrets/deepseek-api-key)
;;
;; For NixOS + agenix setups, method 3 is automatic — no shell config needed.

;; Custom variables for API keys (can be set in config.el)
(defvar deepseek-api-key nil
  "API key for DeepSeek. Can be set in config.el or via DEEPSEEK_API_KEY environment variable.")

(defvar kimi-api-key nil
  "API key for Kimi (Moonshot AI). Can be set in config.el or via KIMI_API_KEY environment variable.")

(defvar anthropic-api-key nil
  "API key for Anthropic Claude. Can be set in config.el or via ANTHROPIC_API_KEY environment variable.")

;; Agenix secret file paths (override in config.el if your paths differ)
(defvar ee-ai-deepseek-secret-file "~/.config/secrets/deepseek-api-key"
  "Path to agenix-decrypted DeepSeek API key file.")

(defvar ee-ai-kimi-secret-file "~/.config/secrets/kimi-api-key"
  "Path to agenix-decrypted Kimi API key file.")

(defvar ee-ai-anthropic-secret-file "~/.config/secrets/anthropic-api-key"
  "Path to agenix-decrypted Anthropic API key file.")

(defun ee-ai--read-secret (file)
  "Read a single-line secret from FILE, or nil if FILE is unreadable."
  (let ((path (expand-file-name file)))
    (when (file-readable-p path)
      (string-trim
       (with-temp-buffer
         (insert-file-contents path)
         (buffer-string))))))

(defun ee-ai--get-key (custom-var env-var secret-file)
  "Resolve an API key from CUSTOM-VAR, ENV-VAR, or SECRET-FILE (first non-empty wins).
Skips empty strings so exec-path-from-shell blanks don't shadow the file fallback."
  (let ((env-val (getenv env-var)))
    (or (and custom-var (not (string-empty-p custom-var)) custom-var)
        (and env-val (not (string-empty-p env-val)) env-val)
        (ee-ai--read-secret secret-file))))

;; GPT integration with Emacs
(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode 'org-mode)

  ;; Key bindings
  (define-key gptel-mode-map (kbd "C-c RET") 'gptel-send)

  ;; Resolve API keys (custom var → env var → agenix secret file)
  (let ((deepseek-key (ee-ai--get-key deepseek-api-key
                                      "DEEPSEEK_API_KEY"
                                      ee-ai-deepseek-secret-file))
        (kimi-key     (ee-ai--get-key kimi-api-key
                                      "KIMI_API_KEY"
                                      ee-ai-kimi-secret-file))
        (anthropic-key (ee-ai--get-key anthropic-api-key
                                       "ANTHROPIC_API_KEY"
                                       ee-ai-anthropic-secret-file)))

    ;; Warn if API keys are not set
    (unless deepseek-key
      (display-warning 'gptel
                      "DEEPSEEK_API_KEY not set. DeepSeek backend will not work. See ee-ai.el for setup instructions."
                      :warning))

    (unless kimi-key
      (display-warning 'gptel
                      "KIMI_API_KEY not set. Kimi backend will not work. See ee-ai.el for setup instructions."
                      :warning))

    ;; DeepSeek backend
    (when deepseek-key
      (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/v1/chat/completions"
        :stream t
        :key deepseek-key
        :models '(deepseek-chat deepseek-coder)))

    ;; Kimi (Moonshot AI) backend
    (when kimi-key
      (gptel-make-openai "Kimi"
        :host "api.moonshot.cn"
        :endpoint "/v1/chat/completions"
        :stream t
        :key kimi-key
        :models '(moonshot-v1-8k moonshot-v1-32k moonshot-v1-128k)))

    ;; Anthropic Claude backend
    (when anthropic-key
      (gptel-make-anthropic "Claude"
        :stream t
        :key anthropic-key
        :models '(claude-sonnet-4-5-20250929
                  claude-opus-4-6
                  claude-haiku-4-5-20251001)))

    ;; Set default backend and model (only if DeepSeek is configured)
    (when deepseek-key
      (setq gptel-backend (gptel-get-backend "DeepSeek")
            gptel-model 'deepseek-chat)))

  ;; Move point to end of buffer after response
  (add-hook 'gptel-post-response-functions
            (lambda (_response _status)
              (goto-char (point-max))))
  )

(provide 'ee-ai)
