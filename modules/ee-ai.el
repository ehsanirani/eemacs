;;; ee-ai.el --- AI integration for eemacs -*- lexical-binding: t; -*-

;; API Key Configuration
;; You can set API keys in one of these ways:
;; 1. Environment variables (recommended): Set DEEPSEEK_API_KEY and KIMI_API_KEY in your shell
;;    - Add to ~/.bashrc, ~/.zshrc, or ~/.profile: export DEEPSEEK_API_KEY="your-key"
;;    - The exec-path-from-shell package will import them into Emacs
;; 2. In config.el: Set (setq deepseek-api-key "your-key") before loading this module
;; 3. Directly in this file (not recommended for security): Replace the fallback values below

;; Custom variables for API keys (can be set in config.el)
(defvar deepseek-api-key nil
  "API key for DeepSeek. Can be set in config.el or via DEEPSEEK_API_KEY environment variable.")

(defvar kimi-api-key nil
  "API key for Kimi (Moonshot AI). Can be set in config.el or via KIMI_API_KEY environment variable.")

;; GPT integration with Emacs
(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode 'org-mode)

  ;; Key bindings
  (define-key gptel-mode-map (kbd "C-c RET") 'gptel-send)

  ;; Get API keys from environment or custom variables
  (let ((deepseek-key (or deepseek-api-key
                         (getenv "DEEPSEEK_API_KEY")))
        (kimi-key (or kimi-api-key
                     (getenv "KIMI_API_KEY"))))

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
