;;; ee-ai.el --- AI integration for eemacs -*- lexical-binding: t; -*-

;; GPT integration with Emacs
(use-package gptel
  :straight t
  :config
  (setq gptel-default-mode 'org-mode)

  ;; Key bindings
  (define-key gptel-mode-map (kbd "C-c RET") 'gptel-send)

  ;; DeepSeek backend
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/v1/chat/completions"
    :stream t
    :key (or (getenv "DEEPSEEK_API_KEY") "your-deepseek-api-key")
    :models '(deepseek-chat deepseek-coder))

  ;; Kimi (Moonshot AI) backend
  (gptel-make-openai "Kimi"
    :host "api.moonshot.cn"
    :endpoint "/v1/chat/completions"
    :stream t
    :key (or (getenv "KIMI_API_KEY") "your-kimi-api-key")
    :models '(moonshot-v1-8k moonshot-v1-32k moonshot-v1-128k))

  ;; Set default backend and model
  (setq gptel-backend (gptel-get-backend "DeepSeek")
        gptel-model 'deepseek-chat)

  ;; Move point to end of buffer after response
  (add-hook 'gptel-post-response-functions
            (lambda (_response _status)
              (goto-char (point-max))))
  )

(provide 'ee-ai)
