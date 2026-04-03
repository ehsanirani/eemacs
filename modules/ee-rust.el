;;; ee-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; Configure rust-analyzer for eglot
(with-eval-after-load 'eglot
  (when (fboundp 'my-add-eglot-server)
    (my-add-eglot-server '(rust-mode rust-ts-mode) '("rust-analyzer"))))

;; Enable eglot for both rust-mode and rust-ts-mode
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

;; Disable rust-ts-flymake (runs bare rustc, doesn't understand Cargo deps).
;; Eglot's flymake backend (from rust-analyzer) handles diagnostics properly.
(with-eval-after-load 'rust-ts-mode
  (remove-hook 'rust-ts-mode-hook #'rust-ts-flymake-setup))

;; Rust major mode
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :config
  ;; Disable rust-mode's own format-on-save; eglot handles formatting
  (setq rust-format-on-save nil))

;; Cargo integration
(use-package cargo
  :straight t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

(provide 'ee-rust)
