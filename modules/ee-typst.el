;;; ee-typst.el --- Typst configuration -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode" :branch "develop")
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :hook (typst-ts-mode . eglot-ensure)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu)

  ;; Configure tinymist LSP server for eglot
  (with-eval-after-load 'eglot
    (when (fboundp 'my-add-eglot-server)
      (my-add-eglot-server 'typst-ts-mode '("tinymist")))))

(provide 'ee-typst)
