;;; ee-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; Configure rust-analyzer for eglot
(with-eval-after-load 'eglot
  (when (fboundp 'my-add-eglot-server)
    (my-add-eglot-server '(rust-mode rust-ts-mode) '("rust-analyzer"))))

;; Enable eglot for both rust-mode and rust-ts-mode
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

;; Rust major mode
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

;; Cargo integration
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(provide 'ee-rust)
