;;; ee-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; Rust major mode
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure)
  :config
  (setq rust-format-on-save t)

  ;; Configure rust-analyzer for eglot
  (with-eval-after-load 'eglot
    (when (fboundp 'my-add-eglot-server)
      (my-add-eglot-server '(rust-mode rust-ts-mode) '("rust-analyzer")))))

;; Cargo integration
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(provide 'ee-rust)
