;;; ee-treesit.el --- Tree-sitter configuration for eemacs -*- lexical-binding: t; -*-

;; Tree-sitter setup for Emacs 29+

(when (fboundp 'treesit-language-available-p)
  ;; Language grammar sources
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
          (typst "https://github.com/uben0/tree-sitter-typst")))

  ;; Install missing grammars
  (defun treesit-install-missing-grammars ()
    "Install missing tree-sitter grammars."
    (interactive)
    (dolist (lang treesit-language-source-alist)
      (let ((lang-symbol (car lang)))
        (unless (treesit-language-available-p lang-symbol)
          (message "Installing tree-sitter grammar for %s..." lang-symbol)
          (treesit-install-language-grammar lang-symbol))))))

(provide 'ee-treesit)
