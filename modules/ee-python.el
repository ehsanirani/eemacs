;;; ee-python.el --- Python configuration -*- lexical-binding: t; -*-

;; Python development setup

;; Check if tree-sitter is available
(defun python-ts-available-p ()
  "Check if python-ts-mode is available."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'python)))

;; Use python-ts-mode if available, otherwise use python-mode
(if (python-ts-available-p)
    (progn
      (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
      (add-to-list 'interpreter-mode-alist '("python3" . python-ts-mode))
      (add-hook 'python-ts-mode-hook #'eglot-ensure)
      (with-eval-after-load 'python-ts-mode
        (setq python-indent-offset 4)))
  (use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python3" . python-mode)
    :hook (python-mode . eglot-ensure)
    :config
    (setq python-indent-offset 4)))

;; Configure Python LSP
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp"))))

;; Smart Python docstring formatting
(use-package python-docstring
  :straight t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))

;; Pytest integration
(use-package python-pytest
  :straight t)

;; Pip requirements files
(use-package pip-requirements
  :straight t
  :mode (("requirements\\.in\\'" . pip-requirements-mode)
         ("\\.pip\\'" . pip-requirements-mode)
         ("requirements\\.txt\\'" . pip-requirements-mode)))

(provide 'ee-python)
